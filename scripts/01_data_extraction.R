library(tidyverse)

month_lookup <- c("0"="Apr","1"="Aug","2"="Dec","3"="Feb","4"="Jan","5"="Jul",
                  "6"="Jun","7"="Mar","8"="May","9"="Nov","10"="Oct","11"="Sep")

file_map <- tribble(
  ~zone,      ~outcome,        ~path,
  "Edmonton", "any_opioid",    "data/raw/edmonton_raw.csv",
  "Edmonton", "any_substance", "data/raw/edmonton_all_raw.csv",
  "Calgary",  "any_opioid",    "data/raw/calgary_raw.csv",
  "Calgary",  "any_substance", "data/raw/calgary_all_raw.csv",
  "North",    "any_opioid",    "data/raw/north_raw.csv",
  "North",    "any_substance", "data/raw/north_all_raw.csv",
  "Central",  "any_opioid",    "data/raw/central_raw.csv",
  "Central",  "any_substance", "data/raw/central_all_raw.csv",
  "South",    "any_opioid",    "data/raw/south_raw.csv",
  "South",    "any_substance", "data/raw/south_all_raw.csv"
)

read_one <- function(zone, outcome, path) {
  read.csv(path, header = FALSE, col.names = c("month_code","rate","year")) |>
    mutate(
      zone       = zone,
      outcome    = outcome,
      month      = month_lookup[as.character(month_code)],
      month_num  = match(month, month.abb),
      date       = as.Date(paste(year, month_num, "01", sep = "-")),
      year       = as.integer(year)
    ) |>
    select(zone, outcome, date, year, month, rate)
}

all_data <- pmap_dfr(file_map, read_one) |>
  arrange(zone, outcome, date)

all_data |>
  group_by(zone, outcome) |>
  summarise(
    n = n(), 
    min_date = min(date), 
    max_date = max(date), 
    .groups = "drop")

all_data |>
  count(zone, outcome, date) |>
  filter(n > 1)

all_data |>
  group_by(zone, outcome) |>
  complete(date = seq.Date(min(date), max(date), by = "month")) |>
  filter(is.na(rate)) |>
  select(zone, outcome, date)

all_data |>
  filter(outcome == "any_opioid") |>
  ggplot(aes(date, rate, color = zone)) +
  geom_line() +
  geom_vline(xintercept = as.Date(c("2017-10-01","2018-03-01")),
             linetype = "dashed", color = "gray40") +
  labs(title = "Opioid poisoning death rate by zone",
       subtitle = "Dashed lines: Calgary (Oct 2017) and Edmonton (Mar 2018) SCS openings")
