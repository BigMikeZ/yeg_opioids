library(nlme)
library(tidyverse)
library(lubridate)

# further cleaning

all_data_clean <- readRDS("data/processed/all_data_clean.rds")
head(all_data_clean)

opioid_data <- all_data_clean |> 
  filter(outcome == "any_opioid") |> 
  group_by(zone) |> 
  mutate(
    time = (min(date) %--% date) / months(1) + 1,
    covid = if_else(date >= as.Date("2020-03-01"), 1, 0),
    zone = factor(zone)
  ) |> 
  ungroup()

edmonton_opioid_data <- opioid_data |> 
  filter(zone %in% c("Edmonton", "Central", "North", "South")) |> 
  mutate(zone = relevel(zone, ref = "Central"))

calgary_opioid_data <- opioid_data |> 
  filter(zone %in% c("Calgary", "Central", "North", "South")) |> 
  mutate(zone = relevel(zone, ref = "Central"))

# build pre models
edmonton_pre <- edmonton_opioid_data |> 
  filter(date < ymd("2018-03-01")) 
calgary_pre <- calgary_opioid_data |> 
  filter(date < ymd("2017-10-01")) 

pre_calgary  <- gls(rate ~ time * zone, data = calgary_pre,
                    correlation = corAR1(form = ~ time | zone), method = "REML")
pre_edmonton <- gls(rate ~ time * zone, data = edmonton_pre,
                    correlation = corAR1(form = ~ time | zone), method = "REML")

summary(pre_calgary)$tTable
summary(pre_edmonton)$tTable

# build full models with ML for comparison reasons
calgary_ols  <- gls(rate ~ time + zone + intervention + time_after + covid,
                    data = calgary_opioid_data, method = "ML")
calgary_ar1  <- gls(rate ~ time + zone + intervention + time_after + covid,
                    data = calgary_opioid_data,
                    correlation = corAR1(form = ~ time | zone), method = "ML")
anova(calgary_ols, calgary_ar1)

edmonton_ols <- gls(rate ~ time + zone + intervention + time_after + covid,
                    data = edmonton_opioid_data, method = "ML")
edmonton_ar1 <- gls(rate ~ time + zone + intervention + time_after + covid,
                    data = edmonton_opioid_data,
                    correlation = corAR1(form = ~ time | zone), method = "ML")
anova(edmonton_ols, edmonton_ar1)

calgary_final  <- gls(rate ~ time + zone + intervention + time_after + covid,
                    data = calgary_opioid_data,
                    correlation = corAR1(form = ~ time | zone), method = "REML")
edmonton_final <- gls(rate ~ time + zone + intervention + time_after + covid,
                    data = edmonton_opioid_data,
                    correlation = corAR1(form = ~ time | zone), method = "REML")

# linearity check within segments (visual)
linearity_check_data <- opioid_data  |> 
  mutate(
    segment = case_when(
      zone %in% c("Edmonton", "Calgary") & intervention == 0           ~     "pre-intervention",
      intervention == 1                                                ~     "post-intervention",
      intervention == 0                                                ~     "Full series (control)"
    )
  ) 

linearity_check_data |> 
  ggplot(aes(x = time, y = rate)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = 'lm', se = FALSE, color = 'blue') +
  geom_smooth(method = 'loess', se = FALSE, color = 'red', span = 0.9) +
  facet_wrap(~ zone + segment) +
  labs(title = "Linear (blue) vs LOESS (red), by zone and segment")

# Quadratic term test
calgary_opioid_data  <- calgary_opioid_data  |> 
  mutate(
    time_sq = time^2,
    time_after_sq = time_after^2,
    residual = residuals(calgary_final, type = 'normalized')
  )
edmonton_opioid_data <- edmonton_opioid_data |> 
  mutate(
    time_sq = time^2,
    time_after_sq = time_after^2,
    residual = residuals(edmonton_final, type = 'normalized')
  )

calgary_quad  <- gls(rate ~ time + time_sq + zone + intervention + time_after + covid,
                     data = calgary_opioid_data,
                     correlation = corAR1(form = ~ time | zone), method = "ML")
edmonton_quad <- gls(rate ~ time + time_sq + zone + intervention + time_after + covid,
                     data = edmonton_opioid_data,
                     correlation = corAR1(form = ~ time | zone), method = "ML")

anova(calgary_ar1, calgary_quad)    
anova(edmonton_ar1, edmonton_quad)  

# residual plot
calgary_opioid_data |> 
  filter(zone == "Calgary") |> 
  ggplot(aes(x = time, y = residual, color = factor(intervention))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE) +
  labs(title = "Calgary residuals vs time", color = "post-intervention")

edmonton_opioid_data |> 
  filter(zone == "Edmonton") |> 
  ggplot(aes(x = time, y = residual, color = factor(intervention))) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_smooth(se = FALSE) +
  labs(title = "Edmonton residuals vs time", color = "post-intervention")

# Adjust models for non-linearity by adding another interruption (COVID)
covid_date <- ymd("2020-03-01")

calgary_opioid_data <- calgary_opioid_data |> 
  group_by(zone) |> 
  mutate(
    covid_time = if_else(
      covid == 1,
      ((covid_date %--% date) %/% months(1)) + 1,
      0
    )
  ) 

edmonton_opioid_data <- edmonton_opioid_data |> 
  group_by(zone) |> 
  mutate(
    covid_time = if_else(
      covid == 1,
      ((covid_date %--% date) %/% months(1)) + 1,
      0
    )
  ) 

calgary_covid_slope <- gls(rate ~ time + zone + intervention + time_after + covid + covid_time,
                           data = calgary_opioid_data,
                           correlation = corAR1(form = ~ time | zone),
                           method = "ML")
edmonton_covid_slope <- gls(rate ~ time + zone + intervention + time_after + covid + covid_time,
                           data = edmonton_opioid_data,
                           correlation = corAR1(form = ~ time | zone),
                           method = 'ML')

anova(calgary_ar1, calgary_covid_slope)
anova(edmonton_ar1, edmonton_covid_slope)

calgary_full <- gls(rate ~ time + zone + intervention + time_after + time_after_sq + covid + covid_time,
                    data = calgary_opioid_data,
                    correlation = corAR1(form = ~ time | zone),
                    method = "ML")
edmonton_full <- gls(rate ~ time + zone + intervention + time_after + time_after_sq + covid + covid_time,
                    data = edmonton_opioid_data,
                    correlation = corAR1(form = ~ time | zone),
                    method = "ML")

anova(calgary_covid_slope, calgary_full)
anova(edmonton_covid_slope, edmonton_full)  # COVID explains the curvature mostly

# Set up final models (COVID as second intervention without adding quadratic terms)
calgary_final  <- gls(rate ~ time + zone + intervention + time_after + covid + covid_time,
                      data = calgary_opioid_data,
                      correlation = corAR1(form = ~ time | zone), method = "REML")

edmonton_final <- gls(rate ~ time + zone + intervention + time_after + covid + covid_time,
                      data = edmonton_opioid_data,
                      correlation = corAR1(form = ~ time | zone), method = "REML")

summary(calgary_final)
summary(edmonton_final)

qqnorm(residuals(calgary_final, type = "normalized")); qqline(residuals(calgary_final, type = "normalized"))
qqnorm(residuals(edmonton_final, type = "normalized")); qqline(residuals(edmonton_final, type = "normalized"))

# Save cleaned opioid data
saveRDS(edmonton_opioid_data, 'data/processed/edmonton_opioid.rds')
saveRDS(calgary_opioid_data, 'data/processed/calgary_opioid.rds')
