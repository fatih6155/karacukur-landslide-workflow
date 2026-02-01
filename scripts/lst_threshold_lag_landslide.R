# ==============================================================
# LST threshold-crossing lag before rockfall/landslide events
# --------------------------------------------------------------
# Purpose:
#   For each event date, compute the number of days ("lag")
#   since the most recent temperature threshold crossing:
#   previous day temp <= 0??C  AND  current day temp > 0??C.
#
# Inputs:
#   1) Daily LST time series (date, temp)
#   2) Event dates (one column in an Excel file)
#
# Output:
#   A table with event_date, cross_date, lag_days + a histogram.
# ==============================================================

library(tidyverse)
library(lubridate)
library(readxl)
library(purrr)

# -----------------------------
# 1) Read daily LST time series
# -----------------------------
lst <- readr::read_delim(
  file = "F:/Kurtun_heyelan/Makale/R_code/mid_basin_LST.csv",
  delim = ";",
  show_col_types = FALSE
) %>%
  mutate(
    date = dmy(date),
    temp = as.numeric(temp)
  ) %>%
  arrange(date)

# Quick checks (optional)
head(lst)
str(lst)


# -----------------------------
# 2) Read event dates from Excel
# -----------------------------
events_raw <- readxl::read_excel(
  path = "F:/Kurtun_heyelan/Makale/R_code/Landslide_date.xlsx",
  col_names = FALSE   # treat the first row as data as well
)

# Collect all non-missing values from the first column into a Date vector
event_dates <- events_raw %>%
  pull(1) %>%
  as.character() %>%
  discard(~ is.na(.x) || .x == "") %>%
  dmy()

event_dates

# ---------------------------------------------------------------
# 3) Function: lag from last (<=0 -> >0??C) crossing to event date
# ---------------------------------------------------------------
find_last_cross_lag <- function(event_date, lookback_days = 90) {
  
  # Subset LST data within the lookback window ending at event_date
  sub <- lst %>%
    filter(
      date <= event_date,
      date >= event_date - days(lookback_days)
    ) %>%
    arrange(date)
  
  # Need at least two days to detect a day-to-day crossing
  if (nrow(sub) < 2) {
    return(tibble(cross_date = as.Date(NA), lag_days = NA_real_))
  }
  
  # Identify crossings: previous day <= 0 and current day > 0
  sub <- sub %>%
    mutate(
      temp_prev = lag(temp),
      is_cross  = !is.na(temp_prev) & temp_prev <= 0 & temp > 0
    )
  
  # Get the last crossing within the window
  last_cross <- sub %>%
    filter(is_cross) %>%
    slice_tail(n = 1)
  
  if (nrow(last_cross) == 0) {
    tibble(cross_date = as.Date(NA), lag_days = NA_real_)
  } else {
    cross_date <- last_cross$date[1]
    lag_days   <- as.numeric(difftime(event_date, cross_date, units = "days"))
    tibble(cross_date = cross_date, lag_days = lag_days)
  }
}

# ---------------------------------------------------------------
# 4) Apply the function to all events and create a results table
# ---------------------------------------------------------------
results <- map_dfr(event_dates, function(ed) {
  info <- find_last_cross_lag(ed, lookback_days = 90)
  tibble(
    event_date = ed,
    cross_date = info$cross_date,
    lag_days   = info$lag_days
  )
})

results
summary(results$lag_days)

# ---------------------------------------------------------------
# 5) Plot: distribution of lag days
# ---------------------------------------------------------------
results %>%
  filter(!is.na(lag_days)) %>%
  ggplot(aes(x = lag_days)) +
  geom_histogram(binwidth = 5) +
  labs(
    x = "Lag (days since last LST crossing from \u2264 0\u00B0C to > 0\u00B0C)",
    y = "Number of events"
  )

    