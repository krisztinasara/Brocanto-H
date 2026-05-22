library(tidyverse)
library(ggh4x)
library(lme4)
library(lmerTest)
library(performance)

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
complexity_levels = 1:4

theme_pilot = theme_minimal(base_size = 12) +
  theme(
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
  if ("metric" %in% names(d) && all(d$metric == "Mean accuracy")) {
    "glm"
  } else {
    "rt"
  }
}

# Per-row y zoom (Median RT vs Mean accuracy). oob_keep = clip at panel edge only;
# values outside the window are not removed from the data or from curve fitting.
summary_metric_zoom = function(rt_max = 50) {
  ggh4x::facetted_pos_scales(
    y = list(
      `Median RT` = scale_y_continuous(limits = c(0, rt_max), oob = scales::oob_keep),
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
      labels = session_loop_half_levels
    )
}

# Load the data (sessions 1 and 2 only)

data = read_csv("results/BrocantoHPilot1WOData.csv") |>
  mutate(sentence_complexity = factor(sentence_complexity, levels = c(1:6))) |>
  filter(session %in% c(1L, 2L))

# =============================================================================
# Summary plots — session–loop half (combined by color grouping variable)
# =============================================================================

# Colored by sentence complexity

bind_rows(
  data |>
    filter(task_type == "Passive", sentence_complexity %in% complexity_levels) |>
    add_session_loop_half() |>
    group_by(participant_unique_id, session_loop_half_num, sentence_complexity) |>
    summarise(value = median(reaction_time, na.rm = TRUE), .groups = "drop") |>
    mutate(metric = "Median RT", task_type = "Passive"),
  data |>
    filter(
      task_type == "Comprehension",
      accuracy == 1,
      sentence_complexity %in% complexity_levels
    ) |>
    add_session_loop_half() |>
    group_by(participant_unique_id, session_loop_half_num, sentence_complexity) |>
    summarise(value = median(reaction_time, na.rm = TRUE), .groups = "drop") |>
    mutate(metric = "Median RT", task_type = "Comprehension"),
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
    mutate(metric = "Median RT", task_type = "Production"),
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
    metric = factor(metric, levels = c("Median RT", "Mean accuracy")),
    task_type = factor(task_type, levels = c("Passive", "Comprehension", "Production"))
  ) |>
  ggplot(aes(x = session_loop_half_num, y = value, color = sentence_complexity)) |>
  add_session_loop_layers(y_var = "value", color_col = "sentence_complexity", smooth = "auto") +
  facet_grid(metric ~ task_type, scales = "free_y") +
  summary_metric_zoom(rt_max = 50) +
  labs(
    title = "Session–loop summary by sentence complexity (sessions 1–2)",
    subtitle = "Median RT on correct trials; mean accuracy on all trials; 1st / 2nd half per loop",
    x = "Session–loop (1st / 2nd half)",
    y = NULL,
    color = "Sentence complexity"
  ) +
  theme_pilot +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 35, hjust = 1))

# Colored by trial type (comprehension)

bind_rows(
  data |>
    filter(
      task_type == "Comprehension",
      accuracy == 1,
      !is.na(trial_type),
      nzchar(trimws(as.character(trial_type)))
    ) |>
    add_session_loop_half() |>
    group_by(participant_unique_id, session_loop_half_num, trial_type) |>
    summarise(value = median(reaction_time, na.rm = TRUE), .groups = "drop") |>
    mutate(metric = "Median RT"),
  data |>
    filter(
      task_type == "Comprehension",
      !is.na(trial_type),
      nzchar(trimws(as.character(trial_type)))
    ) |>
    add_session_loop_half() |>
    group_by(participant_unique_id, session_loop_half_num, trial_type) |>
    summarise(value = mean(accuracy, na.rm = TRUE), .groups = "drop") |>
    mutate(metric = "Mean accuracy")
) |>
  mutate(metric = factor(metric, levels = c("Median RT", "Mean accuracy"))) |>
  ggplot(aes(x = session_loop_half_num, y = value, color = trial_type)) |>
  add_session_loop_layers(y_var = "value", color_col = "trial_type", smooth = "auto") +
  facet_wrap(~metric, scales = "free_y", ncol = 1) +
  labs(
    title = "Comprehension session–loop summary by trial type (sessions 1–2)",
    subtitle = "Median RT on correct trials; mean accuracy on all trials; 1st / 2nd half per loop",
    x = "Session–loop (1st / 2nd half)",
    y = NULL,
    color = "Trial type"
  ) +
  theme_pilot +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 35, hjust = 1))

# =============================================================================
# Visualization — trial level
# =============================================================================

data |>
  filter(task_type == "Passive") |>
  filter(sentence_complexity %in% complexity_levels) |>
  add_session_loop_half() |>
  ggplot(aes(x = session_loop_half_num, y = reaction_time, color = sentence_complexity)) |>
  add_session_loop_layers(
    y_var = "reaction_time",
    color_col = "sentence_complexity",
    smooth = "rt",
    by_participant = TRUE
  ) +
  facet_wrap(~participant_unique_id) +
  labs(
    title = "Passive: RT by session–loop half and participant (sessions 1–2)",
    x = "Session–loop (1st / 2nd half)",
    y = "Reaction time (s)",
    color = "Sentence complexity"
  ) +
  coord_cartesian(ylim = c(0, 50)) +
  theme_pilot +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 35, hjust = 1))

data |>
  filter(task_type == "Comprehension" & accuracy == 1) |>
  filter(sentence_complexity %in% complexity_levels) |>
  add_session_loop_half() |>
  ggplot(aes(x = session_loop_half_num, y = reaction_time, color = sentence_complexity)) |>
  add_session_loop_layers(
    y_var = "reaction_time",
    color_col = "sentence_complexity",
    smooth = "rt",
    by_participant = TRUE
  ) +
  facet_wrap(~participant_unique_id) +
  labs(
    title = "Comprehension: RT by session–loop half and participant (correct trials, sessions 1–2)",
    x = "Session–loop (1st / 2nd half)",
    y = "Reaction time (s)",
    color = "Sentence complexity"
  ) +
  coord_cartesian(ylim = c(0, 50)) +
  theme_pilot +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 35, hjust = 1))

data |>
  filter(task_type == "Comprehension") |>
  filter(sentence_complexity %in% complexity_levels) |>
  add_session_loop_half() |>
  ggplot(aes(x = session_loop_half_num, y = accuracy, color = sentence_complexity)) |>
  add_session_loop_layers(
    y_var = "accuracy",
    color_col = "sentence_complexity",
    smooth = "glm",
    by_participant = TRUE
  ) +
  facet_wrap(~participant_unique_id) +
  labs(
    title = "Comprehension: accuracy by session–loop half and participant (sessions 1–2)",
    x = "Session–loop (1st / 2nd half)",
    y = "Accuracy",
    color = "Sentence complexity"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pilot +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 35, hjust = 1))

data |>
  filter(task_type == "Production" & accuracy == 1) |>
  filter(sentence_complexity %in% complexity_levels) |>
  add_session_loop_half() |>
  ggplot(aes(x = session_loop_half_num, y = reaction_time, color = sentence_complexity)) |>
  add_session_loop_layers(
    y_var = "reaction_time",
    color_col = "sentence_complexity",
    smooth = "rt",
    by_participant = TRUE
  ) +
  facet_wrap(~participant_unique_id) +
  labs(
    title = "Production: RT by session–loop half and participant (correct trials, sessions 1–2)",
    x = "Session–loop (1st / 2nd half)",
    y = "Reaction time (s)",
    color = "Sentence complexity"
  ) +
  coord_cartesian(ylim = c(0, 50)) +
  theme_pilot +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 35, hjust = 1))

data |>
  filter(task_type == "Production") |>
  filter(sentence_complexity %in% complexity_levels) |>
  add_session_loop_half() |>
  ggplot(aes(x = session_loop_half_num, y = accuracy, color = sentence_complexity)) |>
  add_session_loop_layers(
    y_var = "accuracy",
    color_col = "sentence_complexity",
    smooth = "glm",
    by_participant = TRUE
  ) +
  facet_wrap(~participant_unique_id) +
  labs(
    title = "Production: accuracy by session–loop half and participant (sessions 1–2)",
    x = "Session–loop (1st / 2nd half)",
    y = "Accuracy",
    color = "Sentence complexity"
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  theme_pilot +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 35, hjust = 1))
