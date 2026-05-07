library(tidyverse)
library(lme4)
library(lmerTest)
library(performance)

# Load the data

data = read_csv("results/all_sessions.csv") |>
  mutate(sentence_complexity = factor(sentence_complexity, levels = c(1:6)))

# Visualization

data |>
  filter(task_type == "Passive") |>
  filter(sentence_complexity %in% c(1, 2, 3, 4)) |>
  ggplot(aes(
      x = trial_id_global, y = reaction_time, color = sentence_complexity
      )) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~participant_unique_id)

data |>
  filter(task_type == "Comprehension" & accuracy == 1) |>
  filter(sentence_complexity %in% c(1, 2, 3, 4)) |>
  ggplot(aes(
    x = trial_id_global, y = reaction_time, color = sentence_complexity
  )) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~participant_unique_id) +
  coord_cartesian(ylim = c(-5, 30))

data |>
  filter(task_type == "Comprehension") |>
  filter(sentence_complexity %in% c(1, 2, 3, 4)) |>
  ggplot(aes(
    x = trial_id_global, y = accuracy, color = sentence_complexity
  )) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~participant_unique_id) +
  coord_cartesian(ylim = c(0, 1))

data |>
  filter(task_type == "Production" & accuracy == 1) |>
  filter(sentence_complexity %in% c(1, 2, 3, 4)) |>
  ggplot(aes(
    x = trial_id_global, y = reaction_time, color = sentence_complexity
  )) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~participant_unique_id) +
  coord_cartesian(ylim = c(-5, 30))

data |>
  filter(task_type == "Production") |>
  filter(sentence_complexity %in% c(1, 2, 3, 4)) |>
  ggplot(aes(
    x = trial_id_global, y = accuracy, color = sentence_complexity
  )) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap(~participant_unique_id) +
  coord_cartesian(ylim = c(0, 1))

