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
  ) 

edmonton_opioid_data <- opioid_data |> 
  filter(zone %in% c("Edmonton", "Central", "North", "South"))

calgary_opioid_data <- opioid_data |> 
  filter(zone %in% c("Calgary", "Central", "North", "South"))

# build pre models
edmonton_pre <- edmonton_opioid_data |> 
  filter(intervention == 0) |> 
  mutate(zone = relevel(zone, ref = "Central"))
calgary_pre <- calgary_opioid_data |> 
  filter(intervention == 0) |> 
  mutate(zone = relevel(zone, ref = "Central"))

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
