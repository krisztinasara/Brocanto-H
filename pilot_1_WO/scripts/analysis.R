library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)

# Set working directory

setwd("../")

# Load the data

data = read_csv("results/all_sessions.csv") |>
  mutate(sentence_complexity = factor(sentence_complexity, levels = c(1:6)))

# Visualization

# Group level aggregated

data |>
  filter(task_type == "Passive") |>
  filter(sentence_complexity %in% c(1, 2, 3, 4)) |>
  group_by(participant_unique_id, session, loop, sentence_complexity) |>
  summarise(reaction_time = median(reaction_time, na.rm = TRUE)) |>
  unite("session_loop", c(session, loop), sep = "_") |>
  mutate(session_loop_num = as.numeric(factor(session_loop, 
                                              levels = c("1_1","2_1","2_2","3_1","3_2")))) |>
  ggplot(aes(
    x = session_loop_num, y = reaction_time, color = sentence_complexity
  )) +
  geom_point() +
  geom_smooth(
    method = "nls",
    method.args = list(formula = y ~ a * x^b, start = list(a = 10, b = -0.3)),
    se = FALSE
  ) +
  scale_x_continuous(
    breaks = 1:5,
    labels = c("1_1","2_1","2_2","3_1","3_2")
  ) +
  labs(x = "session_loop") +
  coord_cartesian(ylim = c(0, 30))

data |>
  filter(task_type == "Comprehension" & accuracy == 1) |>
  filter(sentence_complexity %in% c(1, 2, 3, 4)) |>
  group_by(participant_unique_id, session, loop, sentence_complexity) |>
  summarise(reaction_time = median(reaction_time, na.rm = TRUE)) |>
  unite("session_loop", c(session, loop), sep = "_") |>
  mutate(session_loop_num = as.numeric(factor(session_loop,
                                              levels = c("1_1","2_1","2_2","3_1","3_2")))) |>
  ggplot(aes(x = session_loop_num, y = reaction_time, color = sentence_complexity)) +
  geom_point() +
  geom_smooth(method = "nls",
              method.args = list(formula = y ~ a * x^b, start = list(a = 10, b = -0.3)),
              se = FALSE) +
  scale_x_continuous(breaks = 1:5, labels = c("1_1","2_1","2_2","3_1","3_2")) +
  labs(x = "session_loop") +
  coord_cartesian(ylim = c(0, 30))

data |>
  filter(task_type == "Comprehension") |>
  filter(sentence_complexity %in% c(1, 2, 3, 4)) |>
  group_by(participant_unique_id, session, loop, sentence_complexity) |>
  summarise(accuracy = mean(accuracy, na.rm = TRUE)) |>
  unite("session_loop", c(session, loop), sep = "_") |>
  mutate(session_loop_num = as.numeric(factor(session_loop,
                                              levels = c("1_1","2_1","2_2","3_1","3_2")))) |>
  ggplot(aes(x = session_loop_num, y = accuracy, color = sentence_complexity)) +
  geom_point() +
  geom_smooth(method = "nls",
              method.args = list(formula = y ~ a * x^b, start = list(a = 10, b = -0.3)),
              se = FALSE) +
  scale_x_continuous(breaks = 1:5, labels = c("1_1","2_1","2_2","3_1","3_2")) +
  labs(x = "session_loop") +
  coord_cartesian(ylim = c(0, 1))

data |>
  filter(task_type == "Production" & accuracy == 1) |>
  filter(sentence_complexity %in% c(1, 2, 3, 4)) |>
  group_by(participant_unique_id, session, loop, sentence_complexity) |>
  summarise(reaction_time = median(reaction_time, na.rm = TRUE)) |>
  unite("session_loop", c(session, loop), sep = "_") |>
  mutate(session_loop_num = as.numeric(factor(session_loop,
                                              levels = c("1_1","2_1","2_2","3_1","3_2")))) |>
  ggplot(aes(x = session_loop_num, y = reaction_time, color = sentence_complexity)) +
  geom_point() +
  geom_smooth(method = "nls",
              method.args = list(formula = y ~ a * x^b, start = list(a = 10, b = -0.3)),
              se = FALSE) +
  scale_x_continuous(breaks = 1:5, labels = c("1_1","2_1","2_2","3_1","3_2")) +
  labs(x = "session_loop") +
  coord_cartesian(ylim = c(0, 30))

data |>
  filter(task_type == "Production") |>
  filter(sentence_complexity %in% c(1, 2, 3, 4)) |>
  group_by(participant_unique_id, session, loop, sentence_complexity) |>
  summarise(accuracy = mean(accuracy, na.rm = TRUE)) |>
  unite("session_loop", c(session, loop), sep = "_") |>
  mutate(session_loop_num = as.numeric(factor(session_loop,
                                              levels = c("1_1","2_1","2_2","3_1","3_2")))) |>
  ggplot(aes(x = session_loop_num, y = accuracy, color = sentence_complexity)) +
  geom_point() +
  geom_smooth(method = "nls",
              method.args = list(formula = y ~ a * x^b, start = list(a = 10, b = -0.3)),
              se = FALSE) +
  scale_x_continuous(breaks = 1:5, labels = c("1_1","2_1","2_2","3_1","3_2")) +
  labs(x = "session_loop") +
  coord_cartesian(ylim = c(0, 1))

# Trial level

data |>
  filter(task_type == "Passive") |>
  filter(sentence_complexity %in% c(1, 2, 3, 4)) |>
  ggplot(aes(
      x = trial_id_global, y = reaction_time, color = sentence_complexity
      )) +
  geom_point() +
  geom_smooth(
    method = "nls",
    method.args = list(formula = y ~ a * x^b, start = list(a = 10, b = -0.3)),
    se = FALSE  # nls confidence intervals aren't reliable, better to omit
  ) +
  facet_wrap(~participant_unique_id) +
  coord_cartesian(ylim = c(-5, 30))


data |>
  filter(task_type == "Comprehension" & accuracy == 1) |>
  filter(sentence_complexity %in% c(1, 2, 3, 4)) |>
  ggplot(aes(
    x = trial_id_global, y = reaction_time, color = sentence_complexity
  )) +
  geom_point() +
  geom_smooth(
    method = "nls",
    method.args = list(formula = y ~ a * x^b, start = list(a = 10, b = -0.3)),
    se = FALSE  # nls confidence intervals aren't reliable, better to omit
  ) +
  facet_wrap(~participant_unique_id) +
  coord_cartesian(ylim = c(-5, 30))

data |>
  filter(task_type == "Comprehension") |>
  filter(sentence_complexity %in% c(1, 2, 3, 4)) |>
  ggplot(aes(
    x = trial_id_global, y = accuracy, color = sentence_complexity
  )) +
  geom_point() +
  geom_smooth(
    method = "glm",
    method.args = list(family = binomial)
  ) +
  facet_wrap(~participant_unique_id) +
  coord_cartesian(ylim = c(0, 1))

data |>
  filter(task_type == "Production" & accuracy == 1) |>
  filter(sentence_complexity %in% c(1, 2, 3, 4)) |>
  ggplot(aes(
    x = trial_id_global, y = reaction_time, color = sentence_complexity
  )) +
  geom_point() +
  geom_smooth(
    method = "nls",
    method.args = list(formula = y ~ a * x^b, start = list(a = 10, b = -0.3)),
    se = FALSE  # nls confidence intervals aren't reliable, better to omit
  ) +
  facet_wrap(~participant_unique_id) +
  coord_cartesian(ylim = c(-5, 30))

data |>
  filter(task_type == "Production") |>
  filter(sentence_complexity %in% c(1, 2, 3, 4)) |>
  ggplot(aes(
    x = trial_id_global, y = accuracy, color = sentence_complexity
  )) +
  geom_point() +
  geom_smooth(
    method = "glm",
    method.args = list(family = binomial)
  ) +
  facet_wrap(~participant_unique_id) +
  coord_cartesian(ylim = c(0, 1))

