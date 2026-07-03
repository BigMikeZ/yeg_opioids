library(tidyverse)
library(lubridate)

all_data <- read_rds('data/processed/all_data.rds')
head(all_data)

all_data_clean <- all_data |> 
  mutate(
    intervention = if_else(
      ((date >= '2017-10-01' & zone == 'Calgary') | (date >= '2018-03-01' & zone == 'Edmonton')), 
      1,
      0
    )
  ) |> 
  group_by(zone, outcome) |> 
  mutate(
    time_after = if_else(
      intervention == 1, 
      cumsum(intervention),
      0
    )
  ) |> 
  ungroup() 
  
saveRDS(all_data_clean, "data/processed/all_data_clean.rds")
