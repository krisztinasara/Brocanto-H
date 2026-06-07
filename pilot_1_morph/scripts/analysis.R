library(tidyverse)
library(ggh4x)
library(showtext)

font_add_google("Raleway", "Raleway")
showtext_auto()

setwd("../")

# Session 1 has only loop 1; session 2 has loops 1 and 2.
session_loop_levels = c("1_1", "2_1", "2_2")
session_loop_half_levels = c(
  "1_1 · 1st half",
  "1_1 · 2nd half",
  "2_1 · 1st half",
  "2_1 · 2nd half",
  "2_2 · 1st half",
  "2_2 · 2nd half"
)
session_loop_half_labels = c(
  "Session 1 · Loop 1\n1st half",
  "Session 1 · Loop 1\n2nd half",
  "Session 2 · Loop 1\n1st half",
  "Session 2 · Loop 1\n2nd half",
  "Session 2 · Loop 2\n1st half",
  "Session 2 · Loop 2\n2nd half"
)
# Between Session 2 Loop 1 and Loop 2 (after 2nd half of loop 1).
session_loop_break = 4.5
complexity_levels = 1:4
complexity_pal = c(
  `1` = "#4ecbbf",
  `2` = "#298a80",
  `3` = "#113b37",
  `4` = "#000000"
)

theme_pilot = theme_minimal(base_size = 12, base_family = "Raleway") +
  theme(
    text = element_text(family = "Raleway"),
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(color = "grey30"),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

# Curve points for geom_line: NLS (or lm fallback) when >= 2 session-loop halves have data.
session_loop_curve_points = function(d, y_var, smooth = c("rt", "glm")) {
  smooth = match.arg(smooth, choices = c("rt", "glm"))
  if (!nrow(d)) {
    return(tibble())
  }

  x = d$session_loop_half_num
  y = d[[y_var]]
  ok = !is.na(x) & !is.na(y)
  x = x[ok]
  y = y[ok]
  phase_x = sort(unique(as.integer(x)))
  if (length(phase_x) < 2L) {
    return(tibble())
  }

  sub = tibble(x = x, y = y)
  xseq = seq(min(phase_x), max(phase_x), length.out = 80)
  pred = if (smooth == "glm" && dplyr::n_distinct(sub$y) < 2L) {
    rep(mean(sub$y), length(xseq))
  } else if (smooth == "glm") {
    tryCatch(
      {
        fit = glm(y ~ x, data = sub, family = binomial)
        as.numeric(stats::predict(fit, newdata = tibble(x = xseq), type = "response"))
      },
      error = function(e) {
        fit = lm(y ~ x, data = sub)
        as.numeric(stats::predict(fit, newdata = tibble(x = xseq)))
      }
    )
  } else {
    tryCatch(
      {
        fit = nls(
          y ~ a * x^b,
          data = sub,
          start = list(a = max(0.1, mean(sub$y, na.rm = TRUE)), b = -0.3)
        )
        as.numeric(stats::predict(fit, newdata = tibble(x = xseq)))
      },
      error = function(e) {
        fit = lm(y ~ x, data = sub)
        as.numeric(stats::predict(fit, newdata = tibble(x = xseq)))
      }
    )
  }

  tibble(
    session_loop_half_num = xseq,
    y_fit = pred,
    curve_id = 1L
  )
}

session_loop_curve_groups = function(df, color_col, by_participant = FALSE) {
  cols = c(color_col, intersect(c("task_type", "metric"), names(df)))
  if (by_participant && "participant_unique_id" %in% names(df)) {
    cols = c(cols, "participant_unique_id")
  }
  unique(cols)
}

# Sessions 1–2 only; split each session_loop into 1st / 2nd half by running_index.
add_session_loop_half = function(df) {
  df |>
    filter(session %in% c(1L, 2L)) |>
    mutate(session_loop = paste(session, loop, sep = "_")) |>
    filter(session_loop %in% session_loop_levels) |>
    group_by(participant_unique_id, session_loop) |>
    arrange(running_index, .by_group = TRUE) |>
    mutate(
      loop_half = if_else(
        row_number() <= ceiling(dplyr::n() / 2),
        "1st half",
        "2nd half"
      ),
      session_loop_half = paste(session_loop, loop_half, sep = " · ")
    ) |>
    ungroup() |>
    mutate(
      session_loop_half_num = as.numeric(factor(
        session_loop_half,
        levels = session_loop_half_levels
      ))
    )
}

resolve_session_loop_smooth = function(d, smooth) {
  smooth = match.arg(smooth, choices = c("rt", "glm", "auto"))
  if (smooth != "auto") {
    return(smooth)
  }
  if ("metric" %in% names(d) && all(d$metric == "Mean accuracy", na.rm = TRUE)) {
    "glm"
  } else {
    "rt"
  }
}

# Per-row y zoom (Median RT (s) vs Mean accuracy). oob_keep = clip at panel edge only;
# values outside the window are not removed from the data or from curve fitting.
summary_metric_zoom = function(rt_max = 50) {
  ggh4x::facetted_pos_scales(
    y = list(
      `Median RT (s)` = scale_y_continuous(limits = c(0, rt_max), oob = scales::oob_keep),
      `Mean accuracy` = scale_y_continuous(limits = c(0, 1), oob = scales::oob_keep)
    )
  )
}

add_session_loop_layers = function(
  p,
  y_var = "value",
  color_col = "sentence_complexity",
  smooth = c("rt", "glm", "auto"),
  by_participant = FALSE
) {
  smooth_mode = match.arg(smooth, choices = c("rt", "glm", "auto"))
  df = p$data
  grp = session_loop_curve_groups(df, color_col, by_participant = by_participant)

  # Trial-level: one median per half before curve fit (same logic as summary plots).
  curve_df = if (by_participant) {
    df |>
      group_by(across(all_of(grp)), session_loop_half_num) |>
      summarise(
        !!y_var := if (smooth_mode == "glm" && y_var == "accuracy") {
          mean(.data[[y_var]], na.rm = TRUE)
        } else {
          median(.data[[y_var]], na.rm = TRUE)
        },
        .groups = "drop"
      )
  } else {
    df
  }

  curves = curve_df |>
    group_by(across(all_of(grp))) |>
    group_modify(function(.x, ...) {
      session_loop_curve_points(
        .x,
        y_var,
        resolve_session_loop_smooth(.x, smooth_mode)
      )
    }) |>
    ungroup()

  if (nrow(curves)) {
    curves = curves |>
      mutate(curve_group = interaction(!!!syms(c(grp, "curve_id")), drop = TRUE))
  }

  color_sym = rlang::sym(color_col)
  p +
    geom_point(position = position_jitter(width = 0.12, height = 0)) +
    geom_line(
      data = curves,
      aes(
        x = session_loop_half_num,
        y = y_fit,
        color = !!color_sym,
        group = curve_group
      ),
      inherit.aes = FALSE,
      linewidth = 0.9,
      na.rm = TRUE
    ) +
    scale_x_continuous(
      breaks = seq_along(session_loop_half_levels),
      labels = session_loop_half_labels
    )
}

# Load the data (sessions 1 and 2 only)

data = read_csv("results/BrocantoHPilot1MorphData.csv") |>
  mutate(sentence_complexity = factor(sentence_complexity, levels = c(1:6))) |>
  filter(session %in% c(1L, 2L))

# =============================================================================
# Summary plots — session–loop half (combined by color grouping variable)
# =============================================================================

bind_rows(
  data |>
    filter(
      task_type == "Comprehension",
      accuracy == 1,
      sentence_complexity %in% complexity_levels
    ) |>
    add_session_loop_half() |>
    group_by(participant_unique_id, session_loop_half_num, sentence_complexity) |>
    summarise(value = median(reaction_time, na.rm = TRUE), .groups = "drop") |>
    mutate(metric = "Median RT (s)", task_type = "Comprehension"),
  data |>
    filter(
      task_type == "Comprehension",
      sentence_complexity %in% complexity_levels
    ) |>
    add_session_loop_half() |>
    group_by(participant_unique_id, session_loop_half_num, sentence_complexity) |>
    summarise(value = mean(accuracy, na.rm = TRUE), .groups = "drop") |>
    mutate(metric = "Mean accuracy", task_type = "Comprehension"),
  data |>
    filter(
      task_type == "Production",
      accuracy == 1,
      sentence_complexity %in% complexity_levels
    ) |>
    add_session_loop_half() |>
    group_by(participant_unique_id, session_loop_half_num, sentence_complexity) |>
    summarise(value = median(reaction_time, na.rm = TRUE), .groups = "drop") |>
    mutate(metric = "Median RT (s)", task_type = "Production"),
  data |>
    filter(
      task_type == "Production",
      sentence_complexity %in% complexity_levels
    ) |>
    add_session_loop_half() |>
    group_by(participant_unique_id, session_loop_half_num, sentence_complexity) |>
    summarise(value = mean(accuracy, na.rm = TRUE), .groups = "drop") |>
    mutate(metric = "Mean accuracy", task_type = "Production")
) |>
  mutate(
    metric = factor(metric, levels = c("Median RT (s)", "Mean accuracy")),
    task_type = factor(task_type, levels = c("Comprehension", "Production"))
  ) |>
  ggplot(aes(x = session_loop_half_num, y = value, color = sentence_complexity)) |>
  add_session_loop_layers(y_var = "value", color_col = "sentence_complexity", smooth = "auto") +
  geom_vline(
    xintercept = session_loop_break,
    linetype = "dashed",
    linewidth = 0.45,
    color = "grey35"
  ) +
  facet_grid(metric ~ task_type, scales = "free_y") +
  summary_metric_zoom(rt_max = 30) +
  scale_color_manual(values = complexity_pal) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL,
    y = NULL,
    color = "Sentence complexity"
  ) +
  theme_pilot +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 0, hjust = 0.5, lineheight = 0.9, size = 9),
    panel.spacing.x = unit(2, "lines")
  )

# =============================================================================
# Session 2: Loop 1 → Loop 2 change scores (Comprehension & Production)
# =============================================================================

# Per-complexity differences first (complexities 1–3 only — the ones present in
# both loops of Session 2; loop 2 also has complexities 4–6 but those never
# appear in loop 1, so no difference can be formed), then averaged across
# complexities within participant (na.rm so a participant with a missing cell
# still contributes the complexities they do have).
# Direction follows expected improvement:
#   RT       = Loop 1 − Loop 2  (positive = speed-up; medians, correct trials)
#   accuracy = Loop 2 − Loop 1  (positive = improvement; means, all trials)

rt_change = data |>
  filter(
    task_type %in% c("Comprehension", "Production"),
    session == 2L,
    loop %in% c(1, 2),
    sentence_complexity %in% 1:3,
    accuracy == 1
  ) |>
  group_by(participant_unique_id, task_type, sentence_complexity, loop) |>
  summarise(rt = median(reaction_time, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = loop, values_from = rt, names_prefix = "loop_") |>
  mutate(rt_diff = loop_1 - loop_2) |>
  group_by(participant_unique_id, task_type) |>
  summarise(rt_change = mean(rt_diff, na.rm = TRUE), .groups = "drop")

acc_change = data |>
  filter(
    task_type %in% c("Comprehension", "Production"),
    session == 2L,
    loop %in% c(1, 2),
    sentence_complexity %in% 1:3
  ) |>
  group_by(participant_unique_id, task_type, sentence_complexity, loop) |>
  summarise(acc = mean(accuracy, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = loop, values_from = acc, names_prefix = "loop_") |>
  mutate(acc_diff = loop_2 - loop_1) |>
  group_by(participant_unique_id, task_type) |>
  summarise(acc_change = mean(acc_diff, na.rm = TRUE), .groups = "drop")

learning_scores = rt_change |>
  full_join(acc_change, by = c("participant_unique_id", "task_type")) |>
  rename(rt_diff = rt_change, acc_diff = acc_change) |>
  pivot_wider(
    names_from = task_type,
    values_from = c(rt_diff, acc_diff),
    names_glue = "{str_to_lower(task_type)}_{.value}"
  ) |>
  select(
    participant_unique_id,
    comprehension_rt_diff,
    comprehension_acc_diff,
    production_rt_diff,
    production_acc_diff
  ) |>
  arrange(participant_unique_id)

# =============================================================================
# Session 2: Likert graded sensitivity for GJ and Chunk
# =============================================================================

# Pull in just the likert_sensitivity() function from d_primes.R. Sourcing it
# into a fresh env keeps that script's data-loading side effects out of here
# (and harmlessly swallows the read_csv error caused by the different wd).
likert_sensitivity = local({
  e = new.env()
  tryCatch(sys.source("scripts/d_primes.R", envir = e), error = function(err) invisible(NULL))
  e$likert_sensitivity
})

# GJ: legal vs. illegal split is given by the `legality` column (the
# `trial_type` column is only filled for illegal trials).
gj_sens = data |>
  filter(task_type == "GJ", session == 2L) |>
  mutate(response = as.numeric(response)) |>
  group_by(participant_unique_id) |>
  summarise(
    gj_likert_sens = likert_sensitivity(
      pick(everything()), legality, response, "legal", "illegal", 1, 6
    ),
    .groups = "drop"
  )

# Chunk: relabel trial_type into target / nontarget before scoring.
# targets    = {seen_high_freq, seen_low_freq}
# nontargets = {illegal, unseen_legal}  (the user-facing "legal_not_seen")
chunk_sens = data |>
  filter(task_type == "Chunk", session == 2L) |>
  mutate(
    response = as.numeric(response),
    target_label = case_when(
      trial_type %in% c("seen_high_freq", "seen_low_freq") ~ "target",
      trial_type %in% c("illegal", "unseen_legal") ~ "nontarget"
    )
  ) |>
  filter(!is.na(target_label)) |>
  group_by(participant_unique_id) |>
  summarise(
    chunk_likert_sens = likert_sensitivity(
      pick(everything()), target_label, response, "target", "nontarget", 1, 6
    ),
    .groups = "drop"
  )

learning_scores = learning_scores |>
  full_join(gj_sens, by = "participant_unique_id") |>
  full_join(chunk_sens, by = "participant_unique_id") |>
  arrange(participant_unique_id)

learning_scores

# =============================================================================
# Learning scores — violin plots (reference = 0)
# =============================================================================

learning_scores_long = learning_scores |>
  pivot_longer(
    -participant_unique_id,
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(
    metric = factor(
      recode(
        metric,
        comprehension_rt_diff = "Comprehension\nRT difference (s)",
        comprehension_acc_diff = "Comprehension\nACC difference",
        production_rt_diff = "Production\nRT difference (s)",
        production_acc_diff = "Production\nACC difference",
        chunk_likert_sens = "Chunk\nsensitivity",
        gj_likert_sens = "Grammatical\nsensitivity"
      ),
      levels = c(
        "Comprehension\nRT difference (s)",
        "Comprehension\nACC difference",
        "Production\nRT difference (s)",
        "Production\nACC difference",
        "Chunk\nsensitivity",
        "Grammatical\nsensitivity"
      )
    )
  ) |>
  filter(is.finite(value)) |>
  mutate(
    task_col = factor(
      case_when(
        str_detect(metric, "Comprehension") ~ "Comprehension",
        str_detect(metric, "Production") ~ "Production",
        TRUE ~ "Sensitivity"
      ),
      levels = c("Comprehension", "Production", "Sensitivity")
    ),
    scale_group = case_when(
      metric %in% c("Comprehension\nRT difference (s)", "Production\nRT difference (s)") ~ "rt_diff",
      metric %in% c("Comprehension\nACC difference", "Production\nACC difference") ~ "acc_diff",
      TRUE ~ "sensitivity"
    )
  )

learning_score_pal = c(
  Comprehension = "#5E76BF",
  Production = "#84BFB9",
  Sensitivity = "#D94A3D"
)

# Shared y scale per row type: Comp + Prod RT, Comp + Prod ACC, Chunk + GJ.
learning_score_y_limits = tibble(
  scale_group = c("rt_diff", "acc_diff", "sensitivity"),
  ymin = c(-10, -0.25, -3),
  ymax = c(20, 0.5, 6)
)

learning_score_y_scale_df = learning_scores_long |>
  distinct(metric, scale_group) |>
  left_join(learning_score_y_limits, by = "scale_group") |>
  arrange(metric)  # match facet panel order (factor levels), not column order

learning_score_y_scales = setNames(
  lapply(seq_len(nrow(learning_score_y_scale_df)), function(i) {
    row = learning_score_y_scale_df[i, ]
    scale_y_continuous(limits = c(row$ymin, row$ymax))
  }),
  learning_score_y_scale_df$metric
)

learning_scores_long |>
  ggplot(aes(x = 1, y = value, fill = task_col)) +
  geom_violin(trim = FALSE, width = 0.675, color = "black", alpha = 0.85, linewidth = 0.4) +
  geom_jitter(width = 0.08, height = 0, color = "black", alpha = 0.65, size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5, color = "grey30") +
  facet_wrap(~metric, scales = "free_y", nrow = 1) +
  ggh4x::facetted_pos_scales(y = learning_score_y_scales) +
  scale_fill_manual(values = learning_score_pal) +
  scale_x_continuous(breaks = NULL, limits = c(0.5, 1.5)) +
  labs(
    title = NULL,
    subtitle = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_pilot +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    strip.text = element_text(face = "bold", size = 10)
  )
