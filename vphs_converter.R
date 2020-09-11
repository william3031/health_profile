# vphs 2017
pacman::p_load(tidyverse, janitor)

# function to convert from datapasta'd content from the vphs dashboard spreadsheet
# may need to delete the first two characters (use alt and select) , then delete the NA rows
vphs_dash_converter <- function(dataset, suffix){
    colnames_vphs_dash <- c("selections", "mv_pct", "mv_ll", "mv_ul", "wma_pct", "wma_ll", "wma_ul",
                          "wd_pct", "wd_ll", "wd_ul", "vic_pct", "vic_ll", "vic_ul")

  colnames(dataset) <- colnames_vphs_dash

  converted <- dataset %>%
    remove_empty() %>%
    pivot_longer(-selections, names_to = "column", values_to = "percentage") %>%
    separate(column, c("region", "column")) %>%
    mutate(region = case_when(region == "mv"~ "Moonee Valley",
                              region == "wma"~ "Western Melbourne Area",
                              region == "wd" ~ "West Division ",
                              region == "vic" ~ "Victoria")) %>%
    pivot_wider(names_from = column, values_from = percentage) %>%
    write_csv(paste0("data_in/vphs_", suffix, ".csv"))
}

# dental ####
## self reported dental health status #################
srdhs <- data.frame(
  stringsAsFactors = FALSE,
  check.names = FALSE,
  `Proportion.of.adult.population,.by.self-reported.dental.health.status` = c("Excellent / Very good","Good",
                                                                              "Fair/Poor",
                                                                              "Not applicable"),
  `%` = c(43.5, 31.7, 22, 2.4),
  LL = c(37.6, 26.3, 17.5, 1.2),
  UL = c(49.6, 37.8, 27.3, 4.9),
  `%` = c(38.3, 35.2, 22.7, 3.5),
  LL = c(35.5, 32.4, 20.3, 2.7),
  UL = c(41.1, 38.1, 25.4, 4.6),
  `%` = c(36.1, 34.5, 24.3, 4.7),
  LL = c(34.4, 32.8, 22.7, 4.1),
  UL = c(37.9, 36.3, 25.9, 5.5),
  `%` = c(37.2, 34, 24.4, 4),
  LL = c(36.3, 33.2, 23.6, 3.7),
  UL = c(38.1, 34.9, 25.2, 4.3)
)

vphs_srdhs <- vphs_dash_converter(srdhs, "srdhs")

## self reported gum disease #############
srgd <- data.frame(
  stringsAsFactors = FALSE,
                V1 = c("Yes", "No"),
                V2 = c(9.4, 88.4),
                V4 = c(6.5, 83.8),
                V5 = c(13.5, 91.9),
                V6 = c(10.4, 87.1),
                V8 = c(8.6, 84.8),
                V9 = c(12.6, 89),
               V10 = c(10.5, 86.7),
               V12 = c(9.4, 85.5),
               V13 = c(11.6, 87.9),
               V14 = c(10.8, 86.7),
               V16 = c(10.3, 86.1),
               V17 = c(11.4, 87.3)
)

vphs_srgd <- vphs_dash_converter(srgd, "srgd")

## avoided or delayed dental because of cost

addbc <- data.frame(
  stringsAsFactors = FALSE,
                V1 = c("Yes", "No"),
                V2 = c(31.6, 68.4),
                V4 = c(26, 62.2),
                V5 = c(37.8, 74),
                V6 = c(31, 68.6),
                V8 = c(28.5, 66),
                V9 = c(33.6, 71.2),
               V10 = c(35.4, 64.2),
               V12 = c(33.7, 62.5),
               V13 = c(37.1, 65.9),
               V14 = c(33.9, 65.5),
               V16 = c(33.1, 64.7),
               V17 = c(34.8, 66.4)
)

vphs_addbc <- vphs_dash_converter(addbc, "addbc")
