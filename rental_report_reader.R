# libraries
pacman::p_load(tidyverse, janitor, readxl)

# functions to get the rental report data out in to something useable

# get file location
rr_file_location <- "data_in/dhhs_rental_report/Affordable lettings by local government area - June quarter 2020.xlsx"

rr_reader <- function(file_location){
  # read in the sheets
  rr_lga_aff_1br <- read_excel(file_location, sheet = "lga aff 1br", skip = 1) %>% clean_names()
  rr_lga_aff_2br <- read_excel(file_location, sheet = "lga aff 2br", skip = 1) %>% clean_names()
  rr_lga_aff_3br <- read_excel(file_location, sheet = "lga aff 3br", skip = 1) %>% clean_names()
  rr_lga_aff_4br <- read_excel(file_location, sheet = "lga aff 4br", skip = 1) %>% clean_names()
  rr_lga_aff_tot <- read_excel(file_location, sheet = "lga aff total", skip = 1) %>% clean_names()

  # function for the file mutation
  rr_mutator <- function(rr_file, bedrooms_text){
    rr_file_saved <- rr_file %>%
      rename(region = x1) %>%
      filter(!is.na(region)) %>%
      filter(region != "Table Total") %>%
      pivot_longer(-region, names_to = "cat1", values_to = "values") %>%
      mutate(cat2 = str_detect(cat1, "x")) %>%
      mutate(month = if_else(cat2 == TRUE, lag(cat1), cat1)) %>%
      mutate(type = if_else(cat2 == TRUE, "Proportion affordable", "Affordable lettings")) %>%
      select(-cat1, -cat2) %>%
      mutate(month = str_replace(month, "mar_", "31-03-")) %>%
      mutate(month = str_replace(month, "jun_", "30-06-")) %>%
      mutate(month = str_replace(month, "sep_", "30-09-")) %>%
      mutate(month = str_replace(month, "dec_", "31-12-")) %>%
      mutate(month = as.Date(month, format = "%d-%m-%Y")) %>%
      mutate(bedrooms = bedrooms_text) %>%
      select(region, bedrooms, type, month, values)
  }

  # mutate the files
  aff_1br <- rr_mutator(rr_lga_aff_1br, "1 bedroom")
  aff_2br <- rr_mutator(rr_lga_aff_2br, "2 bedrooms")
  aff_3br <- rr_mutator(rr_lga_aff_3br, "3 bedrooms")
  aff_4br <- rr_mutator(rr_lga_aff_4br, "4 bedrooms")
  aff_tot <- rr_mutator(rr_lga_aff_tot, "total")

  # bind it all together
  rr_aff <- bind_rows(aff_1br, aff_2br, aff_3br, aff_4br, aff_tot)
}

# run it
rental_report_affordable <- rr_reader(rr_file_location) %>%
  write_csv("data_in/dhhs_rental_report/lga_affordable.csv")

# look at the moonee valley ones
rental_report_affordable %>%
  filter(region == "Moonee Valley") %>%
  #filter(type == "Proportion affordable") %>%
  filter(type == "Affordable lettings") %>%
  tail()
