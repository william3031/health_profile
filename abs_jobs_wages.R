# https://www.abs.gov.au/statistics/labour/earnings-and-work-hours/weekly-payroll-jobs-and-wages-australia/latest-release
# ABS 6160.0.55.001 - Weekly Payroll Jobs and Wages in Australia

# libraries
pacman::p_load(tidyverse, readxl, ggrepel, janitor, scales, glue, lubridate, plotly)

# dates for glue
abs_path <- "data_in/6160055001_DO004.xlsx"

# read in raw data - VICTORIAN DATA
weekly_jobs_data_raw <- read_excel(abs_path, sheet = "Payroll jobs index", skip = 5) %>%
  clean_names() %>%
  filter(state_or_territory == "2. VIC") %>%
  mutate(type = "Jobs")

weekly_wages_data_raw <- read_excel(abs_path, sheet = "Total wages index", skip = 5) %>%
  clean_names() %>%
  filter(state_or_territory == "2. VIC") %>%
  mutate(type = "Wages")

# date for glue - latest week for map below
abs_col_names <- colnames(weekly_jobs_data_raw)

abs_latest_numeric <- abs_col_names[length(abs_col_names) -1] %>%
  str_remove(., "x")  %>% as.numeric(.)

latest_week <- as.Date(abs_latest_numeric, origin = "1899-12-30") %>%
  format(., format = "%d %B")

#filter and join
weekly_jobs_data <- weekly_jobs_data_raw %>%
  select(industry_division, sex, age_group, last = tail(names(.), 2)) %>%  # the last two columns
  rename(latest_week = last1, type = last2)

weekly_wages_data <- weekly_wages_data_raw %>%
  select(industry_division, sex, age_group, last = tail(names(.), 2)) %>%  # the last two columns
  rename(latest_week = last1, type = last2)

weekly_abs_data <- bind_rows(weekly_jobs_data, weekly_wages_data) %>%
  mutate(sex = str_remove(sex, ".*? ")) %>%
  mutate(age_group = str_remove(age_group, ".*? ")) %>%
  mutate(industry_division = str_remove(industry_division, ".*? ")) %>%
  mutate(age_group = factor(age_group, levels = c("Under 20", "20-29", "30-39", "40-49", "50-59", "60-69", "70 and over", "All ages"))) %>%
  mutate(industry_division = if_else(industry_division == "All industries", "All industries", str_sub(industry_division, 3, -1))) %>%
  mutate(industry_division= factor(industry_division)) %>%
  mutate(latest_week = as.numeric(latest_week)) %>%
  mutate(sex = factor(sex))

# data for age and gender - victoria
jobs_wages_by_age_data <- weekly_abs_data %>%
  filter(sex %in% c("Males", "Females")) %>%
  mutate(latest_week = round(latest_week - 100, 1)) %>%
  filter(industry_division == "All industries")
write_csv(jobs_wages_by_age_data, "data_in/jobs_wages_by_age_data.csv")
