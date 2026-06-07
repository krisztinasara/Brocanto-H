library(tidyverse)

setwd("../")
#session_dir = "../data/MORFSession1"
# =============================================================================
# Functions
# =============================================================================

birth_col = "születési dátum (ÉÉÉÉ.HH.NN)"

# Load CSVs from a WO session folder and bind into one data frame (same column types as session1).
# Each file gets running_index_in_file = 1..n in file row order; use consolidate_running_index_after_read()
# so each identity (participant code when present, else parsed birth date) gets running_index 1..N.
read_wo_session = function(session_dir) {
  files = list.files(path = session_dir, pattern = "*.csv", full.names = TRUE)
  lst = lapply(files, read_csv)
  lst = lapply(lst, function(df) {
    # optionOrder is comma-separated indices in Psychopy; read_csv guesses char vs double per file
    df |>
      mutate(
        participant = as.character(participant),
        `születési dátum (ÉÉÉÉ.HH.NN)` = as.character(`születési dátum (ÉÉÉÉ.HH.NN)`)
      ) |>
      mutate(across(any_of("wordLearning.test.optionOrder"), as.character)) |>
      dplyr::mutate(running_index_in_file = dplyr::row_number())
  })
  dplyr::bind_rows(lst)
}

# After bind_rows: sort by participant, parsed birth, PsychoPy date, within-file index; number trials
# 1..N per (participant, birth) so rows with missing ID but different birthdays are not merged.
consolidate_running_index_after_read = function(df) {
  if (!"running_index_in_file" %in% names(df)) {
    warning("consolidate_running_index_after_read: missing running_index_in_file; leaving df unchanged")
    return(df)
  }
  bc = birth_col
  out = dplyr::mutate(
    df,
    participant = trimws(as.character(.data$participant)),
    .birth_key = normalize_birth_date(.data[[bc]])
  )
  if ("date" %in% names(out)) {
    out = dplyr::arrange(out, .data$participant, .data$.birth_key, .data$date, .data$running_index_in_file)
  } else {
    out = dplyr::arrange(out, .data$participant, .data$.birth_key, .data$running_index_in_file)
  }
  out |>
    dplyr::group_by(.data$participant, .data$.birth_key) |>
    dplyr::mutate(running_index = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::any_of(c("running_index_in_file", ".birth_key")))
}

# After dropping rows (manual filter): dense 1..n per (participant, birth_date), preserving trial order.
renumber_running_index_by_identity = function(df) {
  if (!"running_index" %in% names(df)) {
    return(df)
  }
  bd_key = if ("birth_date" %in% names(df)) {
    normalize_birth_date(df$birth_date)
  } else {
    rep(as.Date(NA), nrow(df))
  }
  df |>
    dplyr::mutate(.birth_key = bd_key) |>
    dplyr::arrange(
      .data$participant,
      .data$.birth_key,
      dplyr::desc(!is.na(.data$running_index)),
      .data$running_index
    ) |>
    dplyr::group_by(.data$participant, .data$.birth_key) |>
    dplyr::mutate(running_index = dplyr::row_number()) |>
    dplyr::ungroup() |>
    dplyr::select(-".birth_key")
}

# Row count in data/session_*_trials.csv = full-task length for completion %.
design_trial_path = function(wo_session) {
  file.path("data", paste0("session_", wo_session, "_trials.csv"))
}

full_task_trial_count_design = function(wo_session) {
  path = design_trial_path(wo_session)
  if (!file.exists(path)) {
    warning("full_task_trial_count_design: file not found: ", path)
    return(NA_integer_)
  }
  nrow(readr::read_csv(path, show_col_types = FALSE))
}

# participant_canonical values with at least one export run reaching the design trial count for wo_session.
participant_canonical_fully_completed = function(df, wo_session) {
  need = full_task_trial_count_design(wo_session)
  if (length(need) != 1L || is.na(need) || need <= 0L) {
    warning("participant_canonical_fully_completed: invalid design trial count for session ", wo_session)
    return(character(0))
  }
  if (nrow(df) == 0L || !"participant_canonical" %in% names(df)) {
    return(character(0))
  }
  # Per-folder session frames have no `session` column until mutate(session = ...) later; then all rows are that WO session.
  if ("session" %in% names(df)) {
    ws = as.integer(wo_session)
    df = dplyr::filter(df, as.integer(.data$session) == ws)
  }
  df |>
    dplyr::filter(!is.na(.data$participant_canonical)) |>
    dplyr::count(.data$participant_canonical, name = "n_trials") |>
    dplyr::filter(.data$n_trials >= need) |>
    dplyr::pull(.data$participant_canonical) |>
    unique()
}

participant_row_label = function(participant_chr, birth_date_chr) {
  p = trimws(as.character(participant_chr))
  bad = is.na(p) | !nzchar(p) | toupper(p) %in% c("NA", "NAN")
  ifelse(
    bad,
    paste0("(no ALP ID; birth ", ifelse(is.na(birth_date_chr), "?", as.character(birth_date_chr)), ")"),
    p
  )
}

# PsychoPy `date` often looks like 2026-04-17_13h11.56.167 — take YYYY-MM-DD for calendar gaps.
parse_export_session_date = function(x) {
  xc = trimws(as.character(x))
  dpart = ifelse(
    is.na(xc) | !nzchar(xc),
    NA_character_,
    ifelse(
      grepl("^\\d{4}-\\d{2}-\\d{2}", xc),
      sub("^([0-9]{4}-[0-9]{2}-[0-9]{2}).*", "\\1", xc),
      xc
    )
  )
  suppressWarnings(as.Date(dpart))
}

# PsychoPy `date` like 2026-04-17_13h11.56.167 → POSIXct (UTC); date-only strings → midnight that day.
parse_export_session_posixct = function(x) {
  xc = trimws(as.character(x))
  iso_like = gsub(
    "^([0-9]{4}-[0-9]{2}-[0-9]{2})_([0-9]{1,2})h([0-9]{2})\\.([0-9]{2})\\.([0-9]+)$",
    "\\1 \\2:\\3:\\4.\\5",
    xc,
    perl = TRUE
  )
  out = suppressWarnings(as.POSIXct(iso_like, tz = "UTC", format = "%Y-%m-%d %H:%M:%OS"))
  miss = is.na(out) & !is.na(xc) & nzchar(xc)
  if (any(miss)) {
    out[miss] = suppressWarnings(as.POSIXct(iso_like[miss], tz = "UTC", format = "%Y-%m-%d %H:%M:%S"))
    miss = is.na(out) & !is.na(xc) & nzchar(xc)
    if (any(miss)) {
      out[miss] = as.POSIXct(parse_export_session_date(xc[miss]), tz = "UTC")
    }
  }
  out
}

# One row per person (ALP label, or birth-only label if no ALP); trials and % of design length per WO session.
build_participant_session_completion_report = function(all_sessions_df) {
  n_design = vapply(1:3, full_task_trial_count_design, integer(1))
  tbl = all_sessions_df |>
    dplyr::mutate(
      participant_row = participant_row_label(.data$participant, .data$birth_date)
    ) |>
    dplyr::count(.data$participant_row, .data$session, name = "n_trials") |>
    tidyr::pivot_wider(
      id_cols = participant_row,
      names_from = session,
      values_from = n_trials,
      values_fill = 0L,
      names_prefix = "n_trials_s"
    )
  for (s in 1:3) {
    nm = paste0("n_trials_s", s)
    if (!nm %in% names(tbl)) {
      tbl[[nm]] = 0L
    }
  }
  d1 = n_design[[1]]
  d2 = n_design[[2]]
  d3 = n_design[[3]]

  has_date = "date" %in% names(all_sessions_df)
  gap_src = all_sessions_df |>
    dplyr::mutate(
      participant_row = participant_row_label(.data$participant, .data$birth_date),
      session_ts = if (has_date) {
        parse_export_session_posixct(.data$date)
      } else {
        rep(as.POSIXct(NA, tz = "UTC"), dplyr::n())
      }
    ) |>
    dplyr::group_by(.data$participant_row, .data$session) |>
    dplyr::summarise(
      first_session_time = {
        v = .data$session_ts[!is.na(.data$session_ts)]
        if (length(v)) min(v) else as.POSIXct(NA, tz = "UTC")
      },
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      id_cols = participant_row,
      names_from = session,
      values_from = first_session_time,
      names_prefix = "first_time_s"
    )
  for (s in 1:3) {
    nm = paste0("first_time_s", s)
    if (!nm %in% names(gap_src)) {
      gap_src[[nm]] = rep(as.POSIXct(NA, tz = "UTC"), nrow(gap_src))
    }
  }

  tbl = tbl |>
    dplyr::left_join(gap_src, by = "participant_row") |>
    dplyr::mutate(
      pct_session_1 = dplyr::case_when(
        is.na(d1) | d1 <= 0 ~ NA_real_,
        TRUE ~ round(100 * .data$n_trials_s1 / d1, 1)
      ),
      pct_session_2 = dplyr::case_when(
        is.na(d2) | d2 <= 0 ~ NA_real_,
        TRUE ~ round(100 * .data$n_trials_s2 / d2, 1)
      ),
      pct_session_3 = dplyr::case_when(
        is.na(d3) | d3 <= 0 ~ NA_real_,
        TRUE ~ round(100 * .data$n_trials_s3 / d3, 1)
      ),
      hours_between_s1_s2 = as.numeric(
        difftime(.data$first_time_s2, .data$first_time_s1, units = "hours")
      ),
      hours_between_s2_s3 = as.numeric(
        difftime(.data$first_time_s3, .data$first_time_s2, units = "hours")
      )
    ) |>
    dplyr::arrange(.data$participant_row) |>
    dplyr::select(
      participant_row,
      n_trials_s1,
      pct_session_1,
      n_trials_s2,
      pct_session_2,
      n_trials_s3,
      pct_session_3,
      hours_between_s1_s2,
      hours_between_s2_s3
    )

  cat("\n--- Participant completion vs full task (design CSV row counts) ---\n")
  cat(
    "Denominators — Session 1:", n_design[[1]],
    ", Session 2:", n_design[[2]],
    ", Session 3:", n_design[[3]], "\n"
  )
  cat(
    "hours_between_s1_s2 / hours_between_s2_s3: hours from earliest PsychoPy `date` in the earlier session ",
    "to earliest in the later session (same participant_row).\n\n"
  )
  print(tbl, row.names = FALSE)
  cat("\n")

  invisible(list(report = tbl, design_n_trials = n_design))
}

# --- Normalise birth dates (YYYY-MM-DD, dots, slashes, compact YYYYMMDD, etc.) ---
safe_ymd = function(y, m, d) {
  if (any(is.na(c(y, m, d)))) {
    return(as.Date(NA))
  }
  if (y < 1900 || y > 2100 || m < 1 || m > 12 || d < 1 || d > 31) {
    return(as.Date(NA))
  }
  dt = tryCatch(as.Date(sprintf("%04d-%02d-%02d", y, m, d)), error = function(e) as.Date(NA))
  if (is.na(dt)) {
    return(as.Date(NA))
  }
  chk = as.integer(strsplit(format(dt, "%Y-%m-%d"), "-")[[1]])
  if (chk[1] != y || chk[2] != m || chk[3] != d) {
    return(as.Date(NA))
  }
  dt
}

parse_birth_one = function(s) {
  s = trimws(as.character(s))
  if (is.na(s) || !nzchar(s) || s %in% c("NA", "NaN")) {
    return(as.Date(NA))
  }
  s = gsub("\\.+$", "", s)
  digits_only = gsub("\\D", "", s)
  if (nchar(digits_only) == 8 && grepl("^\\d{8}$", digits_only)) {
    y = as.integer(substr(digits_only, 1, 4))
    m = as.integer(substr(digits_only, 5, 6))
    d = as.integer(substr(digits_only, 7, 8))
    return(safe_ymd(y, m, d))
  }
  nums = regmatches(s, gregexpr("\\d+", s))[[1]]
  nums = as.integer(nums[nzchar(nums)])
  if (length(nums) < 3) {
    return(as.Date(NA))
  }
  if (nums[1] >= 1900 && nums[1] <= 2100) {
    return(safe_ymd(nums[1], nums[2], nums[3]))
  }
  if (nums[3] >= 1900 && nums[3] <= 2100) {
    return(safe_ymd(nums[3], nums[2], nums[1]))
  }
  as.Date(NA)
}

normalize_birth_date = function(x) {
  xc = as.character(x)
  out = rep(as.Date(NA), length(xc))
  for (i in seq_along(xc)) {
    out[i] = parse_birth_one(xc[i])
  }
  out
}

is_alp_id = function(x) {
  grepl("^ALP[0-9]+", as.character(x), ignore.case = TRUE)
}

# Best participant-code string from a vector of labels (never uses birth dates as IDs).
pick_canonical_from_strings = function(chr_vec) {
  chr_vec = trimws(as.character(chr_vec))
  chr_vec = chr_vec[
    !is.na(chr_vec) & nzchar(chr_vec) & !toupper(chr_vec) %in% c("NA", "NAN")
  ]
  if (!length(chr_vec)) {
    return(NA_character_)
  }
  alp = chr_vec[is_alp_id(chr_vec)]
  if (length(alp)) {
    return(min(unique(alp)))
  }
  min(unique(chr_vec))
}
# --- Union-find on rows of norm_pairs: edge if same p_key (ALP) or same birth_norm (parsed) ---
uf_make = function(n) {
  e = new.env(parent = emptyenv())
  e$parent = seq_len(n)
  e
}

uf_find = function(e, x) {
  p = e$parent[[x]]
  if (p != x) {
    e$parent[[x]] = uf_find(e, p)
  }
  e$parent[[x]]
}

uf_union = function(e, x, y) {
  rx = uf_find(e, x)
  ry = uf_find(e, y)
  if (rx != ry) {
    e$parent[[rx]] = ry
  }
}

union_find_identity_rows = function(df) {
  n = nrow(df)
  if (n == 0L) {
    return(integer())
  }
  e = uf_make(n)
  pk = df$p_key
  bn = df$birth_norm
  for (key in unique(pk[!is.na(pk)])) {
    idx = which(!is.na(pk) & pk == key)
    if (length(idx) >= 2L) {
      for (k in seq_along(idx)[-1L]) {
        uf_union(e, idx[[1L]], idx[[k]])
      }
    }
  }
  for (d in unique(bn[!is.na(bn)])) {
    idx = which(!is.na(bn) & bn == d)
    if (length(idx) >= 2L) {
      for (k in seq_along(idx)[-1L]) {
        uf_union(e, idx[[1L]], idx[[k]])
      }
    }
  }
  vapply(seq_len(n), function(i) uf_find(e, i), integer(1))
}
join_canonical = function(df) {
  bc = birth_col
  df |>
    left_join(birth_rep_by_participant, by = "participant") |>
    mutate(
      birth_for_match = dplyr::coalesce(
        if_else(
          !is.na(.data[[bc]]) & nzchar(trimws(as.character(.data[[bc]]))),
          as.character(.data[[bc]]),
          NA_character_
        ),
        .data$birth_rep_fill
      ),
      birth_norm_join = normalize_birth_date(.data$birth_for_match),
      participant_alp_key = {
        p = trimws(as.character(participant))
        ifelse(is_alp_id(p), p, NA_character_)
      }
    ) |>
    left_join(participant_lookup, by = c("participant", bc)) |>
    left_join(lookup_alp_canonical, by = c("participant_alp_key" = "participant_alp")) |>
    left_join(lookup_birth_canonical, by = c("birth_norm_join" = "birth_norm")) |>
    mutate(
      participant_canonical = dplyr::coalesce(
        .data$participant_canonical,
        .data$participant_canonical_alp,
        .data$participant_canonical_birth
      ),
      !!bc := .data$birth_for_match
    ) |>
    select(-any_of(c(
      "birth_for_match",
      "birth_norm_join",
      "participant_alp_key",
      "participant_canonical_alp",
      "participant_canonical_birth",
      "birth_rep_fill"
    )))
}
filter_participant_canonical_in = function(df, ids, label = "") {
  if (!length(ids)) {
    warning(
      "Empty cohort id list",
      if (nzchar(label)) paste0(" (", label, ")") else "",
      "; skipping this filter (no participants would remain otherwise)."
    )
    return(df)
  }
  df |> filter(.data$participant_canonical %in% ids)
}

# --- Narrow columns + chunk-task indices (chunkTest_s1 / chunkTest_s2 share one column family per session) ---
chunk_thisTrialN_cols = c(
  "chunkTest_s1.thisTrialN",
  "chunkTest_s2.thisTrialN",
  "chunkTest_s3.thisTrialN"
)
chunk_thisIndex_cols = c(
  "chunkTest_s1.thisIndex",
  "chunkTest_s2.thisIndex",
  "chunkTest_s3.thisIndex"
)
chunk_thisN_cols = c(
  "chunkTest_s1.thisN",
  "chunkTest_s2.thisN",
  "chunkTest_s3.thisN"
)
chunk_thisRepN_cols = c(
  "chunkTest_s1.thisRepN",
  "chunkTest_s2.thisRepN",
  "chunkTest_s3.thisRepN"
)

gj_thisTrialN_cols = c(
  "GJTest_s1.thisTrialN",
  "GJTest_s2.thisTrialN",
  "GJTest_s3.thisTrialN"
)

passive_thisTrialN_cols = c(
  "passiveTrials_s1.thisTrialN",
  "passiveTrials_s2.thisTrialN",
  "passiveTrials_s3.thisTrialN"
)

production_thisTrialN_cols = c(
  "productionTrials_s1.thisTrialN",
  "productionTrials_s2.thisTrialN",
  "productionTrials_s3.thisTrialN"
)

comp_thisTrialN_cols = c(
  "comprehensionTrials_s1.thisTrialN",
  "comprehensionTrials_s2.thisTrialN",
  "comprehensionTrials_s3.thisTrialN"
)

coalesce_present = function(df, candidates) {
  hit = intersect(candidates, names(df))
  if (!length(hit)) {
    rep(NA, nrow(df))
  } else if (length(hit) == 1) {
    df[[hit]]
  } else {
    do.call(coalesce, df[hit])
  }
}

coalesce_present_chr = function(df, candidates) {
  hit = intersect(candidates, names(df))
  if (!length(hit)) {
    rep(NA_character_, nrow(df))
  } else if (length(hit) == 1) {
    as.character(df[[hit]])
  } else {
    do.call(coalesce, lapply(hit, function(nm) as.character(df[[nm]])))
  }
}

coalesce_present_dbl = function(df, candidates) {
  hit = intersect(candidates, names(df))
  if (!length(hit)) {
    rep(NA_real_, nrow(df))
  } else if (length(hit) == 1) {
    suppressWarnings(as.numeric(df[[hit]]))
  } else {
    do.call(coalesce, lapply(hit, function(nm) suppressWarnings(as.numeric(df[[nm]]))))
  }
}

narrow_wo_session = function(df) {
  na_col = function(nm) {
    if (nm %in% names(df)) {
      df[[nm]]
    } else {
      rep(NA, nrow(df))
    }
  }
  df |>
    mutate(
      chunk_thisTrialN = coalesce_present(df, chunk_thisTrialN_cols),
      chunk_thisIndex = coalesce_present(df, chunk_thisIndex_cols),
      chunk_thisN = coalesce_present(df, chunk_thisN_cols),
      chunk_thisRepN = coalesce_present(df, chunk_thisRepN_cols),
      chunk_target = coalesce(na_col("familiarity.audio"), na_col("audio"))
    )
}

# Chunk vs GJ both use familiarityTrial.*; split using familiarity.testName (chunkTest_* vs GJTest_*).
col_has_value = function(df, nm) {
  if (!nm %in% names(df)) {
    rep(FALSE, nrow(df))
  } else {
    !is.na(df[[nm]])
  }
}

add_task_type = function(df) {
  ftest = if ("familiarity.testName" %in% names(df)) {
    as.character(df$familiarity.testName)
  } else {
    rep(NA_character_, nrow(df))
  }
  mutate(
    df,
    task_type = case_when(
      col_has_value(df, "passiveTrial.started") ~ "Passive",
      col_has_value(df, "comprehensionTrial.started") ~ "Comprehension",
      col_has_value(df, "productionTrial.started") ~ "Production",
      col_has_value(df, "familiarityTrial.started") & grepl("GJTest", ftest, fixed = TRUE) ~ "GJ",
      col_has_value(df, "familiarityTrial.started") & grepl("chunkTest", ftest, fixed = TRUE) ~ "Chunk",
      col_has_value(df, "familiarityTrial.started") ~ NA_character_,
      TRUE ~ NA_character_
    )
  )
}

# Match Psychopy chunk audio filenames to data/session_*_trials.csv (subtask == chunk)
normalize_chunk_label = function(x) {
  x = tolower(trimws(as.character(x)))
  x = gsub("\\.mp3$", "", x, ignore.case = TRUE)
  x = gsub("[-_.]", " ", x)
  x = gsub("\\s+", " ", x)
  trimws(x)
}

optional_col = function(df, nm, fill = NA_character_) {
  if (nm %in% names(df)) {
    df[[nm]]
  } else {
    rep(fill, nrow(df))
  }
}

# difficulty column from data/trial_difficulties.csv → sentence_complexity; match trial_id_global then sentence.
join_trial_difficulties = function(df, wo_session) {
  path = file.path("data", "trial_difficulties.csv")
  if (!file.exists(path)) {
    warning("join_trial_difficulties: file not found: ", path)
    return(dplyr::mutate(df, sentence_complexity = NA_real_))
  }
  td = readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::filter(as.integer(.data$session) == as.integer(wo_session)) |>
    dplyr::mutate(
      .tid = suppressWarnings(as.numeric(.data$trial_id_global)),
      .diff = suppressWarnings(as.numeric(.data$difficulty)),
      .sk = normalize_chunk_label(.data$sentence)
    )
  by_id = td |>
    dplyr::distinct(.data$.tid, .keep_all = TRUE) |>
    dplyr::transmute(trial_id_global = .data$.tid, .sc_tid = .data$.diff)
  by_sent = td |>
    dplyr::distinct(.data$.sk, .keep_all = TRUE) |>
    dplyr::transmute(.sk, .sc_sent = .data$.diff)

  ts = dplyr::na_if(trimws(as.character(optional_col(df, "target_sentence", NA_character_))), "")
  rs = dplyr::na_if(trimws(as.character(optional_col(df, "sentence", NA_character_))), "")
  lab = dplyr::coalesce(ts, rs)
  sk_row = ifelse(is.na(lab), NA_character_, normalize_chunk_label(lab))

  dplyr::mutate(df, trial_id_global = suppressWarnings(as.numeric(.data$trial_id_global))) |>
    dplyr::left_join(by_id, by = "trial_id_global") |>
    dplyr::mutate(.sk_row = sk_row) |>
    dplyr::left_join(by_sent, by = c(".sk_row" = ".sk")) |>
    dplyr::mutate(sentence_complexity = dplyr::coalesce(.data$.sc_tid, .data$.sc_sent)) |>
    dplyr::select(-dplyr::any_of(c(".sc_tid", ".sc_sent", ".sk_row")))
}

# left_join with an empty design table may omit columns; coalesce() needs them present.
ensure_design_join_columns = function(df) {
  n = nrow(df)
  chr_cols = c(
    "design_chunk_type",
    "design_chunk_sentence",
    "design_foil_sentence",
    "design_gj_error_type",
    "design_gj_category",
    "design_chunk_legality",
    "design_gj_legality",
    "design_comprehension_foil_type"
  )
  dbl_cols = c("design_session", "design_loop", "design_trial_index")
  for (nm in chr_cols) {
    if (!nm %in% names(df)) {
      df[[nm]] = rep(NA_character_, n)
    }
  }
  for (nm in dbl_cols) {
    if (!nm %in% names(df)) {
      df[[nm]] = rep(NA_real_, n)
    }
  }
  df
}

empty_design_chunk = function() {
  tibble(
    loop = double(),
    norm_sentence = character(),
    trial_id_global_chunk = double(),
    design_session = double(),
    design_loop = double(),
    design_trial_index = double(),
    design_chunk_sentence = character(),
    design_foil_sentence = character(),
    design_chunk_type = character(),
    design_chunk_legality = character()
  )
}

empty_design_gj = function() {
  tibble(
    loop = double(),
    norm_sentence = character(),
    design_gj_trial_index = double(),
    trial_id_global_gj = double(),
    design_gj_error_type = character(),
    design_gj_category = character(),
    design_gj_legality = character()
  )
}

empty_design_comp = function() {
  tibble(
    loop = double(),
    norm_sentence = character(),
    design_comp_trial_index = double(),
    trial_id_global_comp = double(),
    design_comprehension_foil_type = character()
  )
}

empty_design_passive = function() {
  tibble(
    loop = double(),
    design_passive_trial_index = double(),
    trial_id_global_passive = double()
  )
}

empty_design_production = function() {
  tibble(
    loop = double(),
    design_prod_trial_index = double(),
    trial_id_global_production = double()
  )
}

empty_trial_sentences = function() {
  tibble(
    trial_id_global = double(),
    loop_from_design = double(),
    target_sentence = character(),
    foil_sentence = character(),
    sentence_frame = character(),
    foil_sentence_frame = character()
  )
}

load_stim_chunk_trials = function(wo_session) {
  path = design_trial_path(wo_session)
  if (!file.exists(path)) {
    warning("Design trial file not found: ", path)
    return(empty_design_chunk())
  }
  read_csv(path, show_col_types = FALSE) |>
    filter(.data$subtask == "chunk", as.numeric(.data$session) == wo_session) |>
    mutate(norm_sentence = normalize_chunk_label(.data$sentence)) |>
    transmute(
      loop = as.numeric(loop),
      norm_sentence,
      trial_id_global_chunk = as.numeric(trial_id_global),
      design_session = as.numeric(session),
      design_loop = as.numeric(loop),
      design_trial_index = as.numeric(trial_index),
      design_chunk_sentence = sentence,
      design_foil_sentence = foil_sentence,
      design_chunk_type = as.character(category),
      design_chunk_legality = if_else(category == "illegal", "illegal", "legal")
    )
}

load_stim_gj_trials = function(wo_session) {
  path = design_trial_path(wo_session)
  if (!file.exists(path)) {
    return(empty_design_gj())
  }
  read_csv(path, show_col_types = FALSE) |>
    filter(.data$subtask == "gj", as.numeric(.data$session) == wo_session) |>
    mutate(
      norm_sentence = normalize_chunk_label(.data$sentence),
      gj_err = na_if(trimws(as.character(.data$gj_error_type)), ""),
      design_gj_legality = if_else(
        as.character(.data$gj_grammatical) %in% c("ungrammatical") | as.character(.data$category) == "ungrammatical",
        "illegal",
        "legal"
      )
    ) |>
    transmute(
      loop = as.numeric(loop),
      norm_sentence,
      design_gj_trial_index = as.numeric(trial_index),
      trial_id_global_gj = as.numeric(trial_id_global),
      design_gj_error_type = gj_err,
      design_gj_category = as.character(.data$category),
      design_gj_legality
    )
}

load_stim_comp_trials = function(wo_session) {
  path = design_trial_path(wo_session)
  if (!file.exists(path)) {
    return(empty_design_comp())
  }
  read_csv(path, show_col_types = FALSE) |>
    filter(.data$subtask == "comprehension", as.numeric(.data$session) == wo_session) |>
    mutate(norm_sentence = normalize_chunk_label(.data$sentence)) |>
    transmute(
      loop = as.numeric(loop),
      norm_sentence,
      design_comp_trial_index = as.numeric(trial_index),
      trial_id_global_comp = as.numeric(trial_id_global),
      design_comprehension_foil_type = foil_type
    )
}

load_stim_passive_trials = function(wo_session) {
  path = design_trial_path(wo_session)
  if (!file.exists(path)) {
    return(empty_design_passive())
  }
  read_csv(path, show_col_types = FALSE) |>
    filter(.data$subtask == "passive", as.numeric(.data$session) == wo_session) |>
    transmute(
      loop = as.numeric(loop),
      design_passive_trial_index = as.numeric(trial_index),
      trial_id_global_passive = as.numeric(trial_id_global)
    )
}

load_stim_production_trials = function(wo_session) {
  path = design_trial_path(wo_session)
  if (!file.exists(path)) {
    return(empty_design_production())
  }
  read_csv(path, show_col_types = FALSE) |>
    filter(.data$subtask == "production", as.numeric(.data$session) == wo_session) |>
    transmute(
      loop = as.numeric(loop),
      design_prod_trial_index = as.numeric(trial_index),
      trial_id_global_production = as.numeric(trial_id_global)
    )
}

load_stim_trial_sentences_by_global = function(wo_session) {
  path = design_trial_path(wo_session)
  if (!file.exists(path)) {
    warning("Design trial file not found: ", path)
    return(empty_trial_sentences())
  }
  read_csv(path, show_col_types = FALSE) |>
    filter(as.numeric(.data$session) == wo_session, !is.na(.data$trial_id_global)) |>
    transmute(
      trial_id_global = as.numeric(trial_id_global),
      loop_from_design = as.numeric(.data$loop),
      target_sentence = as.character(.data$sentence),
      foil_sentence = as.character(.data$foil_sentence),
      sentence_frame = as.character(.data$sentence_frame),
      foil_sentence_frame = as.character(.data$foil_sentence_frame)
    ) |>
    distinct(trial_id_global, .keep_all = TRUE)
}

enrich_design_trials = function(df, wo_session) {
  sc = load_stim_chunk_trials(wo_session)
  sg = load_stim_gj_trials(wo_session)
  smp = load_stim_comp_trials(wo_session)
  spass = load_stim_passive_trials(wo_session)
  sprod = load_stim_production_trials(wo_session)
  stim_lines = load_stim_trial_sentences_by_global(wo_session)

  # Psychopy thisTrialN (→ chunk_join_index / gj_join_index) is *presentation order* in the block.
  # Design trial_index is the row order in session_*_trials.csv. For Chunk and GJ those differ when
  # trials are presented in a random order — so we must join Chunk/GJ on loop + normalized stimulus
  # (audio / chunk target), not on trial position.
  out = df |>
    mutate(
      norm_comp_sentence = normalize_chunk_label(optional_col(df, "sentence")),
      norm_chunk_target = normalize_chunk_label(optional_col(df, "chunk_target")),
      norm_gj_audio = normalize_chunk_label(optional_col(df, "familiarity.audio")),
      chunk_join_index = as.numeric(optional_col(df, "chunk_thisTrialN")) + 1,
      gj_join_index = as.numeric(coalesce_present(df, gj_thisTrialN_cols)) + 1,
      comp_join_index = as.numeric(coalesce_present(df, comp_thisTrialN_cols)) + 1,
      passive_session_idx = as.numeric(optional_col(df, "passive.sessionIndex", NA_real_)),
      production_session_idx = as.numeric(optional_col(df, "production.sessionIndex", NA_real_)),
      passive_join_index = as.numeric(coalesce_present(df, passive_thisTrialN_cols)) + 1,
      production_join_index = as.numeric(coalesce_present(df, production_thisTrialN_cols)) + 1
    )

  if (!"familiarity.sessionIndex" %in% names(out)) {
    out$familiarity.sessionIndex = NA_real_
  }
  if (!"comprehension.sessionIndex" %in% names(out)) {
    out$comprehension.sessionIndex = NA_real_
  }
  out = out |>
    mutate(
      familiarity.sessionIndex = as.numeric(familiarity.sessionIndex),
      comprehension.sessionIndex = as.numeric(comprehension.sessionIndex)
    )

  no_design = nrow(sc) == 0 && nrow(sg) == 0 && nrow(smp) == 0 &&
    nrow(spass) == 0 && nrow(sprod) == 0 && nrow(stim_lines) == 0

  if (no_design) {
    out = out |>
      mutate(
        trial_id_global = NA_real_,
        design_session = NA_real_,
        design_loop = NA_real_,
        design_trial_index = NA_real_,
        design_chunk_sentence = NA_character_,
        design_foil_sentence = NA_character_,
        design_chunk_type = NA_character_,
        design_trial_legality = NA_character_,
        design_comprehension_foil_type = NA_character_,
        design_gj_error_type = NA_character_,
        design_gj_category = NA_character_,
        target_sentence = NA_character_,
        foil_sentence = NA_character_,
        sentence_frame = NA_character_,
        foil_sentence_frame = NA_character_,
        loop = NA_real_
      )
  } else {
    out = out |>
      left_join(
        sc,
        by = c(
          "familiarity.sessionIndex" = "loop",
          "norm_chunk_target" = "norm_sentence"
        )
      ) |>
      left_join(
        sg,
        by = c(
          "familiarity.sessionIndex" = "loop",
          "norm_gj_audio" = "norm_sentence"
        )
      ) |>
      left_join(
        smp,
        by = c(
          "comprehension.sessionIndex" = "loop",
          "norm_comp_sentence" = "norm_sentence",
          "comp_join_index" = "design_comp_trial_index"
        )
      ) |>
      left_join(
        spass,
        by = c(
          "passive_session_idx" = "loop",
          "passive_join_index" = "design_passive_trial_index"
        )
      ) |>
      left_join(
        sprod,
        by = c(
          "production_session_idx" = "loop",
          "production_join_index" = "design_prod_trial_index"
        )
      ) |>
      ensure_design_join_columns() |>
      mutate(
        trial_id_global = coalesce(
          .data$trial_id_global_chunk,
          .data$trial_id_global_gj,
          .data$trial_id_global_comp,
          .data$trial_id_global_passive,
          .data$trial_id_global_production
        ),
        design_trial_legality = coalesce(.data$design_chunk_legality, .data$design_gj_legality)
      ) |>
      left_join(stim_lines, by = "trial_id_global") |>
      left_join(
        sc |>
          transmute(
            trial_id_global = .data$trial_id_global_chunk,
            tid_chunk_type = .data$design_chunk_type,
            tid_chunk_leg = .data$design_chunk_legality,
            tid_chunk_sentence = .data$design_chunk_sentence,
            tid_chunk_foil = .data$design_foil_sentence,
            tid_design_session = .data$design_session,
            tid_design_loop = .data$design_loop,
            tid_design_trial_index = .data$design_trial_index
          ) |>
          distinct(trial_id_global, .keep_all = TRUE),
        by = "trial_id_global"
      ) |>
      left_join(
        sg |>
          transmute(
            trial_id_global = .data$trial_id_global_gj,
            tid_gj_error = .data$design_gj_error_type,
            tid_gj_category = .data$design_gj_category,
            tid_gj_leg = .data$design_gj_legality
          ) |>
          distinct(trial_id_global, .keep_all = TRUE),
        by = "trial_id_global"
      ) |>
      mutate(
        design_chunk_type = coalesce(.data$design_chunk_type, .data$tid_chunk_type),
        design_chunk_sentence = coalesce(.data$design_chunk_sentence, .data$tid_chunk_sentence),
        design_foil_sentence = coalesce(.data$design_foil_sentence, .data$tid_chunk_foil),
        design_session = coalesce(.data$design_session, .data$tid_design_session),
        design_loop = coalesce(.data$design_loop, .data$tid_design_loop),
        design_trial_index = coalesce(.data$design_trial_index, .data$tid_design_trial_index),
        design_gj_error_type = coalesce(.data$design_gj_error_type, .data$tid_gj_error),
        design_gj_category = coalesce(.data$design_gj_category, .data$tid_gj_category),
        design_trial_legality = coalesce(
          .data$design_trial_legality,
          .data$tid_chunk_leg,
          .data$tid_gj_leg
        )
      ) |>
      select(
        -tid_chunk_type,
        -tid_chunk_leg,
        -tid_chunk_sentence,
        -tid_chunk_foil,
        -tid_design_session,
        -tid_design_loop,
        -tid_design_trial_index,
        -tid_gj_error,
        -tid_gj_category,
        -tid_gj_leg
      ) |>
      mutate(
        loop = dplyr::coalesce(.data$loop_from_design, .data$design_loop)
      ) |>
      select(-any_of(c("loop_from_design", "design_loop")))
  }

  strip_design_helpers = intersect(
    c(
      "norm_comp_sentence",
      "norm_chunk_target",
      "norm_gj_audio",
      "chunk_join_index",
      "gj_join_index",
      "comp_join_index",
      "passive_session_idx",
      "production_session_idx",
      "passive_join_index",
      "production_join_index",
      "trial_id_global_chunk",
      "trial_id_global_gj",
      "trial_id_global_comp",
      "trial_id_global_passive",
      "trial_id_global_production",
      "design_chunk_legality",
      "design_gj_legality"
    ),
    names(out)
  )
  if (length(strip_design_helpers)) {
    out = out |> select(-all_of(strip_design_helpers))
  }
  out
}

join_sentence_encounters = function(df, wo_session) {
  path = file.path("data", "sentence_encounters.csv")
  if (!file.exists(path)) {
    warning("sentence_encounters.csv not found: ", path)
    return(df)
  }
  enc = read_csv(path, show_col_types = FALSE) |>
    filter(as.numeric(.data$session) == wo_session) |>
    transmute(
      trial_id_global = as.numeric(.data$trial_id_global),
      enc_sentence = trimws(as.character(.data$sentence)),
      encounter_from_design = as.numeric(.data$encounter_count)
    ) |>
    distinct(trial_id_global, enc_sentence, .keep_all = TRUE)

  psych = trimws(as.character(optional_col(df, "sentence", NA_character_)))
  psych = case_when(
    is.na(psych) ~ NA_character_,
    psych %in% c("", "NA") ~ NA_character_,
    TRUE ~ psych
  )
  tgt = trimws(as.character(optional_col(df, "target_sentence", NA_character_)))
  tgt = case_when(
    is.na(tgt) ~ NA_character_,
    tgt %in% c("", "NA") ~ NA_character_,
    TRUE ~ tgt
  )
  # Prefer design text (target_sentence) for matching sentence_encounters.csv. Comprehension
  # (and similar) often store an audio filename in `sentence`, which would never match the CSV.
  sentence_for_encounter = coalesce(tgt, psych)

  out = df |>
    mutate(
      sentence_for_encounter = sentence_for_encounter
    ) |>
    left_join(
      enc,
      by = c("trial_id_global" = "trial_id_global", "sentence_for_encounter" = "enc_sentence")
    )

  if ("familiarity.previous_encounters" %in% names(out)) {
    out = out |>
      mutate(
        familiarity.previous_encounters = coalesce(
          suppressWarnings(as.numeric(familiarity.previous_encounters)),
          encounter_from_design
        )
      )
  } else {
    out = mutate(out, familiarity.previous_encounters = encounter_from_design)
  }

  drop_me = intersect(c("sentence_for_encounter", "encounter_from_design"), names(out))
  if (length(drop_me)) {
    out = out |> select(-all_of(drop_me))
  }
  out
}

collapse_trial_columns = function(df) {
  target_video_cols = c("passive.movie", "passiveMovie", "video", "video_correct", "production.video")
  audio_cols = c("passive.sound", "passiveSound", "familiarity.audio", "audio")
  t_start_cols = c(
    "passiveTrial.started",
    "comprehension.t_sentenceAudioEnd",
    "production.t_firstVideoEnd",
    "familiarity.t_audioEnd"
  )
  t_end_cols = c(
    "passiveTrial.stopped",
    "comprehension.t_lastChoiceClick",
    "production.t_lastWordClick",
    "familiarity.t_lastLikertClick"
  )
  correct_response_cols = c("comprehension.correctPosition", "production.correct_words")
  response_cols = c("comprehension.response", "production.response", "familiarity.rating")
  accuracy_cols = c("comprehension.correct", "production.correct")

  mutate(
    df,
    target_video = coalesce_present_chr(df, target_video_cols),
    foil_video = coalesce_present_chr(df, "video_incorrect"),
    audio = coalesce_present_chr(df, audio_cols),
    t_start = coalesce_present_dbl(df, t_start_cols),
    t_end = coalesce_present_dbl(df, t_end_cols),
    correct_response = coalesce_present_chr(df, correct_response_cols),
    response = coalesce_present_chr(df, response_cols),
    accuracy = coalesce_present_dbl(df, accuracy_cols)
  )
}

rename_experiment_name = function(df) {
  if ("expName" %in% names(df)) {
    rename(df, experiment_name = expName)
  } else {
    mutate(df, experiment_name = NA_character_)
  }
}

# trial_id_global matches session_*_trials.csv. Chunk/GJ are joined by stimulus, so each row gets the correct ID.
# Export row order ≈ presentation order; sorting by trial_id_global follows the design spreadsheet order (e.g. 189, 190, …).
add_trial_type_legality = function(df) {
  n = nrow(df)
  chunk_col = if ("design_chunk_type" %in% names(df)) as.character(df$design_chunk_type) else rep(NA_character_, n)
  gj_err_col = if ("design_gj_error_type" %in% names(df)) as.character(df$design_gj_error_type) else rep(NA_character_, n)
  comp_col = if ("design_comprehension_foil_type" %in% names(df)) {
    as.character(df$design_comprehension_foil_type)
  } else {
    rep(NA_character_, n)
  }
  gj_cat_col = if ("design_gj_category" %in% names(df)) as.character(df$design_gj_category) else rep(NA_character_, n)
  df |>
    mutate(
      trial_type = case_when(
        task_type == "Chunk" ~ chunk_col,
        task_type == "GJ" ~ gj_err_col,
        task_type == "Comprehension" ~ comp_col,
        TRUE ~ NA_character_
      ),
      legality = case_when(
        task_type == "Chunk" & !is.na(trial_type) & tolower(trimws(as.character(trial_type))) == "illegal" ~ "illegal",
        task_type == "Chunk" ~ "legal",
        task_type == "GJ" & tolower(trimws(gj_cat_col)) == "grammatical" ~ "legal",
        task_type == "GJ" & tolower(trimws(gj_cat_col)) == "ungrammatical" ~ "illegal",
        task_type == "GJ" ~ NA_character_,
        TRUE ~ "legal"
      )
    ) |>
    select(-any_of(c(
      "design_chunk_type",
      "design_gj_error_type",
      "design_comprehension_foil_type",
      "design_gj_category"
    )))
}

finalize_export_identity = function(df) {
  n = nrow(df)
  raw_p = trimws(as.character(optional_col(df, "participant", NA_character_)))
  bad_p = is.na(raw_p) | !nzchar(raw_p) | toupper(raw_p) %in% c("NA", "NAN")
  participant_clean = ifelse(bad_p, NA_character_, raw_p)

  pc = if ("participant_canonical" %in% names(df)) {
    trimws(as.character(df$participant_canonical))
  } else {
    rep(NA_character_, n)
  }
  pc_na = is.na(pc) | !nzchar(pc) | toupper(pc) %in% c("NA", "NAN")
  pc_use = ifelse(pc_na, NA_character_, pc)

  participant_out = dplyr::coalesce(pc_use, participant_clean)
  # Export only canonical ALPxxxx IDs; replace numeric / other Psychopy codes with NA (rows kept).
  participant_out = ifelse(is_alp_id(participant_out), participant_out, NA_character_)

  raw_birth = if (birth_col %in% names(df)) df[[birth_col]] else rep(NA_character_, n)
  bd = normalize_birth_date(raw_birth)
  birth_date_out = ifelse(is.na(bd), NA_character_, format(bd, "%Y-%m-%d"))

  df |>
    mutate(
      participant = participant_out,
      birth_date = birth_date_out
    ) |>
    select(-any_of(c("participant_canonical", birth_col))  )
}

# Drop rows matching manual participant IDs and/or normalized birth dates (run after duplicate check).
apply_manual_session_filters = function(df, filter_participant, filter_birth_date) {
  fp = trimws(as.character(filter_participant))
  fp = fp[!is.na(fp) & nzchar(fp)]
  fb_in = trimws(as.character(filter_birth_date))
  fb_in = fb_in[!is.na(fb_in) & nzchar(fb_in)]
  if (!length(fp) && !length(fb_in)) {
    return(df)
  }
  out = df
  if (length(fp) && "participant" %in% names(out)) {
    out = dplyr::filter(out, !trimws(as.character(.data$participant)) %in% fp)
  }
  if (length(fb_in) && "birth_date" %in% names(out)) {
    bd_cut = normalize_birth_date(fb_in)
    bd_cut = bd_cut[!is.na(bd_cut)]
    if (length(bd_cut)) {
      bd_row = normalize_birth_date(out$birth_date)
      out = out[!(bd_row %in% bd_cut), , drop = FALSE]
    }
  }
  out
}

add_reaction_time = function(df) {
  dplyr::mutate(
    df,
    reaction_time = suppressWarnings(as.numeric(.data$t_end) - as.numeric(.data$t_start))
  )
}

# Stable analysis ID: ALP code when present; otherwise missing-YYYY-MM-DD from birth_date.
add_participant_unique_id = function(df) {
  n = nrow(df)
  p = if ("participant" %in% names(df)) trimws(as.character(df$participant)) else rep(NA_character_, n)
  bd = if ("birth_date" %in% names(df)) trimws(as.character(df$birth_date)) else rep(NA_character_, n)
  p[is.na(p)] = ""
  bd[is.na(bd)] = ""
  has_alp = nzchar(p) & !toupper(p) %in% c("NA", "NAN")
  uid = ifelse(
    has_alp,
    p,
    ifelse(nzchar(bd), paste0("missing-", bd), "missing-unknown")
  )
  df$participant_unique_id = uid
  df
}

cols_keep = c(
  "participant",
  "birth_date",
  "date",
  "experiment_name",
  "session",
  "loop",
  "trial_id_global",
  "running_index",
  "task_type",
  "trial_type",
  "legality",
  "sentence_complexity",
  "target_sentence",
  "foil_sentence",
  "sentence_frame",
  "foil_sentence_frame",
  "previous_encounters",
  "target_video",
  "foil_video",
  "audio",
  "t_start",
  "t_end",
  "reaction_time",
  "correct_response",
  "response",
  "accuracy"
)

rename_previous_encounters = function(df) {
  has_fam = "familiarity.previous_encounters" %in% names(df)
  has_loose = "previous_encounters" %in% names(df)
  if (!has_fam && !has_loose) {
    return(df)
  }
  n = nrow(df)
  v_fam = if (has_fam) {
    suppressWarnings(as.numeric(df$familiarity.previous_encounters))
  } else {
    rep(NA_real_, n)
  }
  v_loose = if (has_loose) {
    suppressWarnings(as.numeric(df$previous_encounters))
  } else {
    rep(NA_real_, n)
  }
  merged = dplyr::coalesce(v_fam, v_loose)
  df |>
    select(-any_of(c("familiarity.previous_encounters", "previous_encounters"))) |>
    mutate(previous_encounters = merged)
}

# Illegal chunk items have no meaningful prior-exposure count; treat design encounter 0 as missing.
na_zero_previous_encounters_illegal_chunk = function(df) {
  if (!all(c("previous_encounters", "task_type", "design_trial_legality") %in% names(df))) {
    return(df)
  }
  mutate(
    df,
    previous_encounters = if_else(
      task_type == "Chunk" & design_trial_legality == "illegal" & !is.na(previous_encounters) & previous_encounters == 0,
      NA_real_,
      suppressWarnings(as.numeric(previous_encounters))
    )
  )
}
flag_session_participant_date_duplicates = function(df, session_label) {
  has_raw_birth = birth_col %in% names(df)
  has_export_birth = "birth_date" %in% names(df)
  if (!("date" %in% names(df)) || (!has_raw_birth && !has_export_birth)) {
    warning(
      "flag_session_participant_date_duplicates: need date and birth (",
      birth_col,
      " or birth_date); skipping session ",
      session_label
    )
    return(invisible(tibble()))
  }
  if (!"participant" %in% names(df)) {
    warning(
      "flag_session_participant_date_duplicates: need participant column; skipping session ",
      session_label
    )
    return(invisible(tibble()))
  }

  bc = if (has_raw_birth) birth_col else "birth_date"
  empty_pid = function(p) {
    p = trimws(as.character(p))
    is.na(p) | !nzchar(p) | toupper(p) %in% c("NA", "NAN")
  }

  x = df |>
    mutate(
      participant = trimws(as.character(participant)),
      birth_norm = normalize_birth_date(.data[[bc]]),
      birth_date_label = dplyr::if_else(
        !is.na(birth_norm),
        format(.data$birth_norm, "%Y-%m-%d"),
        paste0("unparsed:", trimws(as.character(.data[[bc]])))
      ),
      psycho_date = trimws(as.character(date)),
      has_pid = !empty_pid(participant),
      has_birth = !is.na(birth_norm)
    )

  n = nrow(x)
  if (n == 0L) {
    message("Duplicate check: empty data for session ", session_label, ".")
    return(invisible(tibble()))
  }

  # Same union-find helpers as identity matching (session rows = trial rows)
  union_find_session_rows = function(has_pid_vec, participant_chr, birth_norm_vec) {
    e = uf_make(n)
    pk = ifelse(has_pid_vec, trimws(as.character(participant_chr)), NA_character_)
    bn = birth_norm_vec
    for (key in unique(pk[!is.na(pk) & nzchar(pk)])) {
      idx = which(!is.na(pk) & pk == key)
      if (length(idx) >= 2L) {
        for (k in seq_along(idx)[-1L]) {
          uf_union(e, idx[[1L]], idx[[k]])
        }
      }
    }
    for (d in unique(bn[!is.na(bn)])) {
      idx = which(!is.na(bn) & bn == d)
      if (length(idx) >= 2L) {
        for (k in seq_along(idx)[-1L]) {
          uf_union(e, idx[[1L]], idx[[k]])
        }
      }
    }
    vapply(seq_len(n), function(i) uf_find(e, i), integer(1))
  }

  roots = union_find_session_rows(x$has_pid, x$participant, x$birth_norm)

  psycho_chr = x$psycho_date
  u_roots = sort(unique(roots))
  bad_root_int = u_roots[
    vapply(u_roots, function(r) {
      dplyr::n_distinct(psycho_chr[roots == r], na.rm = TRUE) > 1L
    }, logical(1))
  ]

  if (!length(bad_root_int)) {
    message(
      "Duplicate check (linked participant/birth vs PsychoPy date): none found for session ",
      session_label,
      "."
    )
    return(invisible(tibble()))
  }

  x_bad = x |>
    mutate(
      root = roots,
      identity_component = match(.data$root, sort(bad_root_int)),
      participant_cell = dplyr::if_else(has_pid, participant, "(participant missing)"),
      birth_cell = dplyr::if_else(has_birth, birth_date_label, "(birth missing)")
    ) |>
    dplyr::filter(.data$root %in% bad_root_int)

  detail = x_bad |>
    dplyr::group_by(.data$identity_component, .data$participant_cell, .data$birth_cell, .data$psycho_date) |>
    dplyr::summarise(row_count = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(.data$identity_component, .data$psycho_date, .data$participant_cell)

  cat(
    "\n--- Session ",
    session_label,
    ": conflicting PsychoPy `date` within one linked identity (participant and/or birth) ---\n",
    "Rows with the same participant code are merged; rows with the same parsed birth are merged;",
    " those links chain (e.g. missing ID + same birth ties to rows that have the ID).\n",
    sep = ""
  )
  print(as.data.frame(detail), row.names = FALSE)
  cat("\n")

  invisible(detail)
}

# =============================================================================
# Procedure
# =============================================================================

session1 = read_wo_session("data/MORFSession1") |> consolidate_running_index_after_read()
session2 = read_wo_session("data/MORFSession2") |> consolidate_running_index_after_read()
#session3 = read_wo_session("data/MORFSession3") |> consolidate_running_index_after_read()
# Distinct (participant, birth) pairs across all WO sessions — union-find merges rows linked by
# same ALP code or same parsed birth date so ALP propagates across sessions / numeric codes.
#all_pairs = bind_rows(
#  session1 |> select(participant, all_of(birth_col)) |> distinct(),
#  session2 |> select(participant, all_of(birth_col)) |> distinct()
#) |>
#  distinct()

all_pairs <- bind_rows(
  session1%>%
    mutate(participant = participant) %>%              # no‑op if it exists
    select(participant, all_of(birth_col)) %>%
    distinct(),
  session2%>%
    mutate(participant = participant) %>%
    select(participant, all_of(birth_col)) %>%
    distinct()
  # add session3 similarly if used
)

norm_pairs = all_pairs |>
  mutate(
    birth_norm = normalize_birth_date(.data[[birth_col]]),
    p_key = {
      p = trimws(as.character(participant))
      ifelse(
        is.na(p) | !nzchar(p) | toupper(p) %in% c("NA", "NAN") | !is_alp_id(p),
        NA_character_,
        p
      )
    }
  )
roots = union_find_identity_rows(norm_pairs)

root_ids = sort(unique(roots))
canon_for_root = vapply(root_ids, function(r) {
  pick_canonical_from_strings(norm_pairs$participant[roots == r])
}, character(1))
names(canon_for_root) = as.character(root_ids)

participant_canonical_row = unname(canon_for_root[as.character(roots)])

birth_rep_for_root = vapply(root_ids, function(r) {
  ix = which(roots == r)
  subs = norm_pairs[[birth_col]][ix]
  subs = subs[!is.na(subs) & nzchar(trimws(as.character(subs)))]
  if (!length(subs)) {
    return(NA_character_)
  }
  trimws(as.character(subs[[1L]]))
}, character(1))
names(birth_rep_for_root) = as.character(root_ids)

norm_pairs = norm_pairs |>
  mutate(
    participant_canonical = participant_canonical_row,
    birth_rep_component = unname(birth_rep_for_root[as.character(roots)])
  )

participant_lookup = norm_pairs |>
  select(any_of(c("participant", birth_col, "participant_canonical"))) |>
  distinct()

lookup_alp_canonical = norm_pairs |>
  filter(!is.na(.data$p_key)) |>
  transmute(
    participant_alp = .data$p_key,
    participant_canonical_alp = .data$participant_canonical
  ) |>
  distinct(participant_alp, participant_canonical_alp)

lookup_birth_canonical = norm_pairs |>
  filter(!is.na(.data$birth_norm)) |>
  transmute(
    birth_norm = .data$birth_norm,
    participant_canonical_birth = .data$participant_canonical
  ) |>
  distinct(birth_norm, participant_canonical_birth)

birth_rep_by_participant = norm_pairs |>
  mutate(p_chr = trimws(as.character(participant))) |>
  filter(nzchar(p_chr), !toupper(p_chr) %in% c("NA", "NAN")) |>
  group_by(participant = p_chr) |>
  summarise(
    birth_rep_fill = {
      v = birth_rep_component[
        !is.na(birth_rep_component) & nzchar(trimws(as.character(birth_rep_component)))
      ]
      if (length(v)) {
        v[[1L]]
      } else {
        NA_character_
      }
    },
    .groups = "drop"
  )

session1 = join_canonical(session1)
session2 = join_canonical(session2)
#session3 = join_canonical(session3)
# Who appears in which session (from full exports; used after narrow/enrich to filter S2/S3)
cid_session1 = session1 |>
  filter(!is.na(.data$participant_canonical)) |>
  distinct(participant_canonical) |>
  pull(participant_canonical)
cid_session2 = session2 |>
  filter(!is.na(.data$participant_canonical)) |>
  distinct(participant_canonical) |>
  pull(participant_canonical)
# Next session only if they fully completed the previous session’s design trial count at least once.
cid_session1_complete = participant_canonical_fully_completed(session1, 1L)
cid_session2_eligible = intersect(cid_session1, cid_session1_complete)
cid_session2_complete = participant_canonical_fully_completed(session2, 2L)
#cid_session3_eligible = Reduce(intersect, list(cid_session1, cid_session2, cid_session2_complete))

session1 = narrow_wo_session(session1) |> add_task_type() |> enrich_design_trials(1) |> join_sentence_encounters(1) |> collapse_trial_columns() |> rename_experiment_name() |> rename_previous_encounters() |> na_zero_previous_encounters_illegal_chunk() |> add_trial_type_legality() |> join_trial_difficulties(1L) |> mutate(session = 1L) |> finalize_export_identity() |> add_reaction_time() |> select(any_of(cols_keep))
names(session1)

session2 = narrow_wo_session(session2) |> add_task_type() |> enrich_design_trials(2) |> join_sentence_encounters(2) |> collapse_trial_columns() |> rename_experiment_name() |> rename_previous_encounters() |> na_zero_previous_encounters_illegal_chunk() |> add_trial_type_legality() |> join_trial_difficulties(2L) |> mutate(session = 2L) |> filter_participant_canonical_in(cid_session2_eligible, "cid_session2_eligible") |> finalize_export_identity() |> add_reaction_time() |> select(any_of(cols_keep))
#session3 = narrow_wo_session(session3) |> add_task_type() |> enrich_design_trials(3) |> join_sentence_encounters(3) |> collapse_trial_columns() |> rename_experiment_name() |> rename_previous_encounters() |> na_zero_previous_encounters_illegal_chunk() |> add_trial_type_legality() |> join_trial_difficulties(3L) |> mutate(session = 3L) |> filter_participant_canonical_in(cid_session3_eligible, "cid_session3_eligible") |> finalize_export_identity() |> add_reaction_time() |> select(any_of(cols_keep))

flag_session_participant_date_duplicates(session1, 1)
flag_session_participant_date_duplicates(session2, 2)
#flag_session_participant_date_duplicates(session3, 3)

# --- Manual exclusions (edit after reviewing duplicate-date output) ---
# Participant IDs as in the export column (ALPXXXX). Birth values as in birth_date, e.g. "2000-05-09".
session_1_filter_participant = c("ALP1108")
session_1_filter_birth_date = c("1987-03-24", "1999-09-04")
session_2_filter_participant = c("ALP1108")
session_2_filter_birth_date = c("1987-03-24", "1999-09-04")
session_3_filter_participant = c("ALP1108")
session_3_filter_birth_date = c("1987-03-24", "1999-09-04")

# Drop manually flagged participants / births after duplicate check; renumber running_index 1..n per (participant, birth).
session1 = session1 |>
  apply_manual_session_filters(session_1_filter_participant, session_1_filter_birth_date) |>
  renumber_running_index_by_identity()
session2 = session2 |>
  apply_manual_session_filters(session_2_filter_participant, session_2_filter_birth_date) |>
  renumber_running_index_by_identity()
#session3 = session3 |>
#  apply_manual_session_filters(session_3_filter_participant, session_3_filter_birth_date) |>
#  renumber_running_index_by_identity()

# Stacked sessions: participant, birth_date, then session and trial order (avoids mixing NA-ID rows).
all_sessions = dplyr::bind_rows(session1, session2) |>
  dplyr::arrange(.data$participant, .data$birth_date, .data$session, .data$running_index) |>
  add_participant_unique_id() |>
  dplyr::relocate(participant_unique_id, .after = participant)

participant_report = build_participant_session_completion_report(all_sessions)
participant_completion_report = participant_report$report
design_n_trials_full_task = participant_report$design_n_trials

write_csv(all_sessions, "results/all_sessions.csv", na = "")
write_csv(participant_completion_report, "results/participant_completion_report.csv", na = "")
