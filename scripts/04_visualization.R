library(tidyverse)
library(nlme)

# Read data and models
edmonton_opioid_data <- read_rds("data/processed/edmonton_opioid.rds")
calgary_opioid_data <- read_rds("data/processed/calgary_opioid.rds")

edmonton_opioid_final <- read_rds("models/edmonton_opioid_final.rds")
calgary_opioid_final <- read_rds("models/calgary_opioid_final.rds")

# Build data for visualization
edmonton_plot_data <- edmonton_opioid_data |> 
  ungroup() |> 
  mutate(fitted = fitted(edmonton_opioid_final))
calgary_plot_data <- calgary_opioid_data |> 
  ungroup() |> 
  mutate(fitted = fitted(calgary_opioid_final))

edmonton_counter_factual <- edmonton_opioid_data |> 
  mutate(
    intervention = 0,
    time_after = 0,
    time_after_sq = 0
  ) 
calgary_counter_factual <- calgary_opioid_data |> 
  mutate(
    intervention = 0,
    time_after = 0,
    time_after_sq = 0
  )

edmonton_counter_factual$counter_factual_fit <- predict(edmonton_opioid_final, newdata = edmonton_counter_factual)
calgary_counter_factual$counter_factual_fit <- predict(calgary_opioid_final, newdata = calgary_counter_factual)

edmonton_plot_data <- edmonton_plot_data |> 
  filter(zone == "Edmonton")
calgary_plot_data <- calgary_plot_data |> 
  filter(zone == 'Calgary')
edmonton_counter_factual <- edmonton_counter_factual |> 
  filter(zone == "Edmonton") 
calgary_counter_factual <- calgary_counter_factual |> 
  filter(zone == "Calgary")

# Visualization
ggplot() +
  geom_point(data = edmonton_plot_data, aes(x = date, y = rate)) +
  geom_line(data = edmonton_plot_data, aes(x = date, y = fitted), color = "#2a78d6", linewidth = 1) +
  geom_line(data = edmonton_counter_factual, aes(x = date, y = counter_factual_fit), color = "#888780", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = ymd("2018-03-01"), linetype = "solid", color = "#eda100", linewidth = 0.8) +
  geom_vline(xintercept = ymd("2020-03-01"), linetype = "dotted", color = "gray50") +
  labs(
    title = "Edmonton: observed opioid death rate vs. model-fitted and counterfactual trajectories",
    subtitle = "Solid amber = SCS opening (Mar 2018)  |  Dotted = COVID onset (Mar 2020)\nBlue = fitted (actual model)  |  Gray dashed = counterfactual (no SCS effect)",
    x = "Date", y = "Rate per 100,000 person-years"
  ) +
  theme_minimal()

ggplot() +
  geom_point(data = calgary_plot_data, aes(x = date, y = rate)) +
  geom_line(data = calgary_plot_data, aes(x = date, y = fitted), color = "#2a78d6", linewidth = 1) +
  geom_line(data = calgary_counter_factual, aes(x = date, y = counter_factual_fit), color = "#888780", linewidth = 1, linetype = "dashed") +
  geom_vline(xintercept = ymd("2017-10-01"), linetype = "solid", color = "#eda100", linewidth = 0.8) +
  geom_vline(xintercept = ymd("2020-03-01"), linetype = "dotted", color = "gray50") +
  labs(
    title = "Calgary: observed opioid death rate vs. model-fitted and counterfactual trajectories",
    subtitle = "Solid amber = SCS opening (Oct 2017)  |  Dotted = COVID onset (Mar 2020)\nBlue = fitted (actual model)  |  Gray dashed = counterfactual (no SCS effect)",
    x = "Date", y = "Rate per 100,000 person-years"
  ) +
  theme_minimal()

# Predicted difference (effect) at each post-intervention time point, with SE
calgary_effect <- calgary_plot_data |>
  filter(intervention == 1) |>
  mutate(effect = fitted - calgary_counter_factual$counter_factual_fit[match(date, calgary_counter_factual$date)])

ggplot(calgary_effect, aes(date, effect)) +
  geom_line(color = "#2a78d6", linewidth = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  labs(title = "Calgary: estimated SCS effect on opioid death rate over time",
       subtitle = "Effect = fitted − counterfactual. Dashed line = no effect.",
       y = "Estimated effect (rate per 100,000)")
