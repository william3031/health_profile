# vphs 2017
pacman::p_load(tidyverse, janitor)

# function to convert from datapasta'd content from the vphs dashboard spreadsheet
vphs_dash_converter <- function(dataset, suffix){
    colnames_vphs_dash <- c("selections", "mv_pct", "del1", "mv_ll", "mv_ul", "wma_pct", "del2", "wma_ll", "wma_ul",
                            "wd_pct", "del3", "wd_ll", "wd_ul", "vic_pct", "del4", "vic_ll", "vic_ul")

  colnames(dataset) <- colnames_vphs_dash

  converted <- dataset %>%
    remove_empty() %>%
    pivot_longer(-selections, names_to = "column", values_to = "percentage") %>%
    filter(!column %in% c("del1", "del2", "del3", "del4")) %>%
    separate(column, c("region", "column")) %>%
    mutate(region = case_when(region == "mv"~ "Moonee Valley",
                              region == "wma"~ "Western Melbourne Area",
                              region == "wd" ~ "West Division",
                              region == "vic" ~ "Victoria")) %>%
    pivot_wider(names_from = column, values_from = percentage) %>%
    filter(region != "West Division") %>%
    write_csv(paste0("data_in/vphs_", suffix, ".csv"))
}

# function to convert from datapasta'd content from the vphs dashboard spreadsheet
# version where the first two characters (use alt and select) are deleted
vphs_dash_converter2 <- function(dataset, suffix){
  colnames_vphs_dash <- c("selections", "mv_pct", "mv_ll", "mv_ul", "wma_pct", "wma_ll", "wma_ul",
                          "wd_pct", "wd_ll", "wd_ul", "vic_pct", "vic_ll", "vic_ul")

  colnames(dataset) <- colnames_vphs_dash

  converted <- dataset %>%
    remove_empty() %>%
    pivot_longer(-selections, names_to = "column", values_to = "percentage") %>%
    separate(column, c("region", "column")) %>%
    mutate(region = case_when(region == "mv"~ "Moonee Valley",
                              region == "wma"~ "Western Melbourne Area",
                              region == "wd" ~ "West Division",
                              region == "vic" ~ "Victoria")) %>%
    pivot_wider(names_from = column, values_from = percentage) %>%
    filter(region != "West Division") %>%
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

vphs_srdhs <- vphs_dash_converter2(srdhs, "srdhs")

## self reported gum disease #############
srgd <- data.frame(
  stringsAsFactors = FALSE,
                V1 = c("Yes", "No"),
                V2 = c(9.4, 88.4),
                V3 = c(NA, NA),
                V4 = c(6.5, 83.8),
                V5 = c(13.5, 91.9),
                V6 = c(10.4, 87.1),
                V7 = c(NA, NA),
                V8 = c(8.6, 84.8),
                V9 = c(12.6, 89),
               V10 = c(10.5, 86.7),
               V11 = c(NA, NA),
               V12 = c(9.4, 85.5),
               V13 = c(11.6, 87.9),
               V14 = c(10.8, 86.7),
               V15 = c(NA, NA),
               V16 = c(10.3, 86.1),
               V17 = c(11.4, 87.3)
        )

vphs_srgd <- vphs_dash_converter(srgd, "srgd")

## avoided or delayed dental because of cost

addbc <- data.frame(
  stringsAsFactors = FALSE,
                V1 = c("Yes", "No"),
                V2 = c(31.6, 68.4),
                V3 = c(NA, NA),
                V4 = c(26, 62.2),
                V5 = c(37.8, 74),
                V6 = c(31, 68.6),
                V7 = c(NA, NA),
                V8 = c(28.5, 66),
                V9 = c(33.6, 71.2),
               V10 = c(35.4, 64.2),
               V11 = c(NA, NA),
               V12 = c(33.7, 62.5),
               V13 = c(37.1, 65.9),
               V14 = c(33.9, 65.5),
               V15 = c(NA, NA),
               V16 = c(33.1, 64.7),
               V17 = c(34.8, 66.4)
)

vphs_addbc <- vphs_dash_converter(addbc, "addbc")

## fruit and veg intake ####
vphs_fvi <- data.frame(
    stringsAsFactors = FALSE,
    check.names = FALSE,
    `Proportion.of.adult.population,.by.compliance.with.fruit.and.vegetable.consumption.guidelines` = c("Met fruit and vegetable consumption guidelines",
                                                                                                                    "Met vegetable consumption guidelines only",
                                                                                                                    "Met fruit consumption guidelines only",
                                                                                                                    "Did not meet fruit and vegetable consumption guidelines"),
    `%` = c(3.2, 4.4, 47.5, 49.5),
    LL = c(1.8, 2.8, 41.6, 43.7),
    UL = c(5.6, 6.9, 53.4, 55.5),
    `%` = c(2.9, 4.6, 43.6, 50.6),
    LL = c(2.2, 3.7, 40.8, 47.8),
    UL = c(3.8, 5.7, 46.5, 53.5),
    `%` = c(3.5, 5.2, 42.8, 51.3),
    LL = c(3, 4.5, 41, 49.5),
    UL = c(4.2, 5.9, 44.6, 53.1),
    `%` = c(3.6, 5.4, 43.2, 51.7),
    LL = c(3.3, 5, 42.3, 50.8),
    UL = c(3.9, 5.8, 44.1, 52.6)
)

vphs_fvi <- vphs_dash_converter2(vphs_fvi, "fvi")

# Chronic disease ####
## tests
vphs_blood <- data.frame(
  stringsAsFactors = FALSE,
  check.names = FALSE,
  `Proportion.of.adult.population.who.had.a.health.related.check,.conducted.by.a.doctor.or.other.health.professional,.in.the.last.two.years` = c("Blood pressure","Blood lipids",
                                                                                                                                                 "Blood glucose"),
  `%` = c(78.2, 55.6, 47.6),
  LL = c(72.6, 50, 41.9),
  UL = c(82.9, 61.2, 53.5),
  `%` = c(76.5, 55.1, 48.4),
  LL = c(74, 52.4, 45.6),
  UL = c(78.7, 57.7, 51.3),
  `%` = c(77.9, 55.5, 49.9),
  LL = c(76.3, 53.8, 48.2),
  UL = c(79.5, 57.2, 51.6),
  `%` = c(79.6, 56.8, 50.7),
  LL = c(78.8, 56, 49.9),
  UL = c(80.4, 57.6, 51.6)
)

vphs_blood <- vphs_dash_converter2(vphs_blood, "blood")

# faecal occult blood test
vphs_fobt <- data.frame(
  stringsAsFactors = FALSE,
                             V1 = c("Received the FOBT kit",
                                    "Completed and returned the FOBT kit"),
                V2 = c(64.5, 61.2),
                V3 = c(NA, NA),
                V4 = c(57.4, 52.6),
                V5 = c(71, 69.1),
                V6 = c(67.3, 55.8),
                V7 = c(NA, NA),
                V8 = c(63, 50.3),
                V9 = c(71.3, 61.1),
               V10 = c(67.8, 60),
               V11 = c(NA, NA),
               V12 = c(65.3, 57.2),
               V13 = c(70.2, 62.7),
               V14 = c(66.7, 60.1),
               V15 = c(NA, NA),
               V16 = c(65.5, 58.8),
               V17 = c(67.8, 61.5)
             )

vphs_fobt <- vphs_dash_converter(vphs_fobt, "fobt")

# bowel examination
vphs_bowel <- data.frame(
  stringsAsFactors = FALSE,
                V1 = c("Males", "Females", "People"),
                V2 = c(37.5, 44.3, 41.2),
                V3 = c(NA, NA, NA),
                V4 = c(27.4, 36.1, 34.6),
                V5 = c(48.9, 52.9, 48.1),
                V6 = c(44.7, 43.2, 43.9),
                V7 = c(NA, NA, NA),
                V8 = c(38.7, 37.2, 39.7),
                V9 = c(50.8, 49.3, 48.2),
               V10 = c(46.3, 44.3, 45.2),
               V11 = c(NA, NA, NA),
               V12 = c(42.8, 41.1, 42.9),
               V13 = c(49.7, 47.5, 47.6),
               V14 = c(48.8, 44.9, 46.8),
               V15 = c(NA, NA, NA),
               V16 = c(47.1, 43.3, 45.6),
               V17 = c(50.6, 46.4, 47.9)
              )

vphs_bowel <- vphs_dash_converter(vphs_bowel, "bowel")

# mammogram
vphs_mamm <- data.frame(
  stringsAsFactors = FALSE,
                V1 = c("Ever", "In last 2 years"),
                V2 = c(79.6, 83.5),
                V3 = c(NA, NA),
                V4 = c(69.3, 74.6),
                V5 = c(87, 89.7),
                V6 = c(84.3, 83.5),
                V7 = c(NA, NA),
                V8 = c(75.9, 78.5),
                V9 = c(90.2, 87.5),
               V10 = c(86.6, 79.9),
               V11 = c(NA, NA),
               V12 = c(83.3, 77.1),
               V13 = c(89.3, 82.5),
               V14 = c(88, 79.2),
               V15 = c(NA, NA),
               V16 = c(86.7, 77.7),
               V17 = c(89.2, 80.7)
             )

vphs_mamm <- vphs_dash_converter(vphs_mamm, "mamm")

# smoking #####
vphs_smoking <- data.frame(
  stringsAsFactors = FALSE,
                                V1 = c("Current smoker","Ex-smoker",
                                       "Non-smoker"),
                V2 = c(15.4, 22.7, 60.8),
                V3 = c(NA, NA, NA),
                V4 = c(11.2, 18.3, 54.9),
                V5 = c(20.7, 27.9, 66.4),
                V6 = c(15.1, 21.8, 62.6),
                V7 = c(NA, NA, NA),
                V8 = c(13.2, 19.7, 59.9),
                V9 = c(17.3, 24.1, 65.3),
               V10 = c(17, 23.2, 59.3),
               V11 = c(NA, NA, NA),
               V12 = c(15.7, 21.9, 57.5),
               V13 = c(18.5, 24.6, 61),
               V14 = c(16.7, 24.4, 58.1),
               V15 = c(NA, NA, NA),
               V16 = c(16, 23.7, 57.2),
               V17 = c(17.5, 25.1, 59)
                )

vphs_smoking <- vphs_dash_converter(vphs_smoking, "smoking")

## obesity ####
vphs_obese <- data.frame(
  stringsAsFactors = FALSE,
  check.names = FALSE,
  `Proportion.of.adult.population.who.were.overweight.(pre-obese.or.obese)` = c("Pre-obese or obese"),
  `%` = c(47.6),
  LL = c(41.7),
  UL = c(53.7),
  `%` = c(47.9),
  LL = c(45.2),
  UL = c(50.7),
  `%` = c(50.4),
  LL = c(48.6),
  UL = c(52.1),
  `%` = c(50.8),
  LL = c(49.9),
  UL = c(51.7)
)

vphs_obese <- vphs_dash_converter2(vphs_obese, "obese")

vphs_takeaway <- data.frame(
  stringsAsFactors = FALSE,
                                 V1 = c("Never",
                                        "1 or less than 1 times/week","More than 1 times/week"),
                V2 = c(18.4, 67.2, 14.1),
                V3 = c(NA, NA, NA),
                V4 = c(14.8, 61.5, 10.1),
                V5 = c(22.6, 72.5, 19.2),
                V6 = c(16.9, 66.5, 16.3),
                V7 = c(NA, NA, NA),
                V8 = c(14.9, 63.7, 14.1),
                V9 = c(19.1, 69.2, 18.9),
               V10 = c(15, 66.9, 17.1),
               V11 = c(NA, NA, NA),
               V12 = c(13.7, 65.1, 15.7),
               V13 = c(16.3, 68.7, 18.7),
               V14 = c(14.9, 69.1, 15.3),
               V15 = c(NA, NA, NA),
               V16 = c(14.3, 68.2, 14.6),
               V17 = c(15.5, 70, 16)
                 )

vphs_takeaway <- vphs_dash_converter(vphs_takeaway, "takeaway")

vphs_sugardr <- data.frame(
  stringsAsFactors = FALSE,
  check.names = FALSE,
  `Proportion.of.adult.population.who.consumed.sugar-sweetened.beverages.daily` = c("Consumed sugar-sweetened soft drinks daily"),
  `%` = c(6.4),
  LL = c(4.3),
  UL = c(9.4),
  `%` = c(10.8),
  LL = c(8.7),
  UL = c(13.2),
  `%` = c(11.3),
  LL = c(0),
  UL = c(12.6),
  `%` = c(10.1),
  LL = c(9.5),
  UL = c(10.7)
)

vphs_sugardr <- vphs_dash_converter2(vphs_sugardr, "sugardr")

# physical activity ####

vphs_phys <- data.frame(
  stringsAsFactors = FALSE,
  check.names = FALSE,
  Sedentary = c("Insufficient", "Met guidelines"),
  `4.7` = c(36.7, 57.7),
  `2.3` = c(31, 51.5),
  `9.3` = c(42.7, 63.6),
  `2.6` = c(46.6, 48.5),
  `1.9` = c(43.8, 45.7),
  `3.7` = c(49.5, 51.3),
  `3.0` = c(45.1, 48.8),
  `2.4` = c(43.4, 47.1),
  `3.8` = c(46.9, 50.6),
  `2.5` = c(44.1, 50.9),
  `2.3` = c(43.2, 50),
  `2.9` = c(45, 51.8)
)

vphs_phys <- vphs_dash_converter2(vphs_phys, "phys")

vphs_sitting <- data.frame(
  stringsAsFactors = FALSE,
                V1 = c("Week day", "Weekend day"),
                V2 = c(29.7, 11.6),
                V3 = c(NA, NA),
                V4 = c(24.4, 8.2),
                V5 = c(35.7, 16.2),
                V6 = c(28.3, 13.5),
                V7 = c(NA, NA),
                V8 = c(25.9, 11.7),
                V9 = c(31, 15.6),
               V10 = c(26.4, 13.7),
               V11 = c(NA, NA),
               V12 = c(24.8, 12.5),
               V13 = c(28, 15),
               V14 = c(26.6, 14.1),
               V15 = c(NA, NA),
               V16 = c(25.8, 13.4),
               V17 = c(27.5, 14.7)
                )

vphs_sitting <- vphs_dash_converter(vphs_sitting, "sitting")

# mental health and wellbeing ####

vphs_anx <- data.frame(
    stringsAsFactors = FALSE,
    check.names = FALSE,
    Proportion.of.adult.population.ever.diagnosed.with.anxiety.or.depression = c("Anxiety or depression"),
    `%` = c(30.4),
    LL = c(25),
    UL = c(36.4),
    `%` = c(25),
    LL = c(22.7),
    UL = c(27.6),
    `%` = c(27.7),
    LL = c(26.2),
    UL = c(29.3),
    `%` = c(27.4),
    LL = c(26.6),
    UL = c(28.2)
)

vphs_anx <- vphs_dash_converter2(vphs_anx, "anx")

vphs_srhealth <- data.frame(
  stringsAsFactors = FALSE,
                                 V1 = c("Excellent / very good","Good",
                                        "Fair/poor"),
                V2 = c(47.5, 34.6, 17.7),
                V3 = c(NA, NA, NA),
                V4 = c(41.6, 29.3, 13.7),
                V5 = c(53.5, 40.4, 22.5),
                V6 = c(41.6, 38.6, 19.6),
                V7 = c(NA, NA, NA),
                V8 = c(38.8, 35.8, 17.4),
                V9 = c(44.4, 41.4, 22.1),
               V10 = c(40.2, 38.2, 21),
               V11 = c(NA, NA, NA),
               V12 = c(38.5, 36.4, 19.6),
               V13 = c(42, 40, 22.5),
               V14 = c(41.6, 37.6, 20.3),
               V15 = c(NA, NA, NA),
               V16 = c(40.7, 36.7, 19.6),
               V17 = c(42.5, 38.5, 21)
                 )

vphs_srhealth <- vphs_dash_converter(vphs_srhealth, "srhealth")

vphs_lifesat <- data.frame(
  stringsAsFactors = FALSE,
                                V1 = c("Low or medium (0-6)","High (7-8)",
                                       "Very high (9-10)"),
                V2 = c(19.9, 49.9, 27.9),
                V3 = c(NA, NA, NA),
                V4 = c(15.3, 43.9, 23),
                V5 = c(25.6, 56, 33.4),
                V6 = c(21.5, 50.4, 26.3),
                V7 = c(NA, NA, NA),
                V8 = c(19, 47.5, 23.8),
                V9 = c(24.3, 53.3, 28.9),
               V10 = c(21.4, 49.9, 27.1),
               V11 = c(NA, NA, NA),
               V12 = c(19.8, 48.1, 25.6),
               V13 = c(23, 51.7, 28.7),
               V14 = c(20.5, 50.9, 27),
               V15 = c(NA, NA, NA),
               V16 = c(19.7, 50, 26.2),
               V17 = c(21.2, 51.8, 27.8)
                )

vphs_lifesat <- vphs_dash_converter(vphs_lifesat, "lifesat")

vphs_worthwhile <- data.frame(
  stringsAsFactors = FALSE,
                                   V1 = c("Low or medium (0-6)",
                                          "High (7-8)","Very high (9-10)"),
                V2 = c(19.1, 48.7, 30.2),
                V3 = c(NA, NA, NA),
                V4 = c(14.5, 42.6, 25.2),
                V5 = c(24.7, 54.8, 35.7),
                V6 = c(17.5, 48.7, 29.4),
                V7 = c(NA, NA, NA),
                V8 = c(15.3, 45.8, 27),
                V9 = c(20, 51.6, 32),
               V10 = c(17.7, 45.8, 32.9),
               V11 = c(NA, NA, NA),
               V12 = c(0, 0, 0),
               V13 = c(19.3, 47.6, 34.5),
               V14 = c(16.7, 46.2, 34),
               V15 = c(NA, NA, NA),
               V16 = c(16, 45.3, 33.1),
               V17 = c(17.5, 47.1, 34.8)
                   )

vphs_worthwhile <- vphs_dash_converter(vphs_worthwhile, "worthwhile")
