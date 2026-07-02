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

