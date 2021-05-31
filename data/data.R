library(tidyverse)

read_raw_csv <- function(name, col_types = cols(), ...) {
  read_csv(glue::glue("data-raw/{name}.csv"), col_types = col_types, ...)
}

read_raw_xls <- function(name, ...) {
  readxl::read_excel(glue::glue("data-raw/{name}.xls"), ...)
}

compare_vectors <- function(vec1, vec2, vec1_lbl = "in1", vec2_lbl = "in2") {
  in1 <- setdiff(vec1, vec2)
  in2 <- setdiff(vec2, vec1)
  if (length(in1) > length(in2)) {
    longer <- in1
    shorter <- in2
  } else {
    longer <- in2
    shorter <- in1
  }
  shorter_padded <- c(sort(shorter), rep(NA, length(longer) - length(shorter)))
  cmp <- tibble(sort(longer), shorter_padded)
  if (length(in1) > length(in2)) {
    colnames(cmp) <- c(vec1_lbl, vec2_lbl)
  } else {
    colnames(cmp) <- c(vec2_lbl, vec1_lbl)
  }
  cmp %>%
    select(!!rlang::sym(vec1_lbl), !!rlang::sym(vec2_lbl)) %>%
    filter(!is.na(!!rlang::sym(vec1_lbl)) | !is.na(!!rlang::sym(vec2_lbl)))
}

# The data ====================================================================

stringency_raw <- read_raw_csv("stringency")

covid_cases_raw <- read_raw_csv("covid-cases")

flu_surveillance_raw <- map_dfr(
  list.files("data-raw/flu-surveillance"),
  \(filename) read_raw_csv(
    paste0("flu-surveillance/", str_replace(filename, ".csv", "")),
    skip = 2,
    guess_max = 1e5
  )
)

country_codes_raw <- read_raw_csv("country-codes")

# Country names have to match =================================================

# Stringency countries ========================================================

# KOS isn't present in stringency
compare_vectors(
  stringency_raw$country_code, country_codes_raw$code3,
  "stringency", "codes"
) %>%
  select(stringency) %>%
  filter(!is.na(stringency))

# Country code for Kosovo is KOS but the license plate code is RKS
stringency_raw %>% filter(country_code == "KOS")
stringency_raw %>% filter(country_code == "RKS")

# Assume that RKS is actually Kosovo
stringency_rks_fixed <- stringency_raw %>%
  mutate(country_code = recode(country_code, "RKS" = "KOS"))

country_rks_fixed <- country_codes_raw %>%
  bind_rows(tibble(name = "Kosovo", code2 = NA, code3 = "KOS", code_num = NA))

compare_vectors(
  stringency_rks_fixed$country_code, country_rks_fixed$code3,
  "stringency", "codes"
) %>%
  select(stringency) %>%
  filter(!is.na(stringency))

# Covid cases countries =======================================================

covid_cases_renamed <- covid_cases_raw %>%
  select(
    country_name = Name,
    cases_cumulative_total = `Cases - cumulative total`,
    cases_cumulative_total_per_100000 =
      `Cases - cumulative total per 100000 population`,
    deaths_cumulative_total = `Deaths - cumulative total`,
    deaths_cumulative_total_per_100000 =
      `Deaths - cumulative total per 100000 population`
  )

compare_vectors(
  covid_cases_renamed$country_name, country_rks_fixed$name,
  "covid", "codes"
) %>%
  select(covid) %>%
  filter(!is.na(covid))

# There are some '(the)' appearances in country codes
country_codes_the_fixed <- country_rks_fixed %>%
  mutate(name = str_replace(name, "\\(the\\)", "") %>% str_trim())

compare_vectors(
  covid_cases_renamed$country_name, country_codes_the_fixed$name,
  "covid", "codes"
) %>%
  select(covid) %>%
  filter(!is.na(covid))

# Special cases
country_special_fixed <- country_codes_the_fixed %>%
  mutate(
    name = recode(name,
      "Virgin Islands (British)" = "British Virgin Islands",
      "Côte d'Ivoire" = "Ivory Coast",
      "Korea (the Democratic People's Republic of)" = "North Korea",
      "Congo (the Democratic Republic of the)" = "West Congo",
      "Falkland Islands  [Malvinas]" = "Falkland Islands",
      "Republic of North Macedonia" = "North Macedonia",
      "Northern Mariana Islands" = "Mariana Islands",
      "Palestine, State of" = "Palestine",
      "United Kingdom of Great Britain and Northern Ireland" = "UK",
      "Korea (the Republic of)" = "South Korea",
      "Moldova (the Republic of)" = "Moldova",
      "Saint Helena, Ascension and Tristan da Cunha" = "Saint Helena",
      "Saint Martin (French part)" = "Saint Martin",
      "Sint Maarten (Dutch part)" = "Sint Maarten",
      "Tanzania, United Republic of" = "Tanzania",
      "Virgin Islands (U.S.)" = "US Virgin Islands",
    )
  ) %>%
  filter(name != "Bonaire, Sint Eustatius and Saba")

covid_special_countries <- covid_cases_renamed %>%
  mutate(
    country_name = recode(
      country_name,
      "Côte d’Ivoire" = "Ivory Coast",
      "Democratic People's Republic of Korea" = "North Korea",
      "Democratic Republic of the Congo" = "West Congo",
      "Falkland Islands (Malvinas)" = "Falkland Islands",
      "Kosovo[1]" = "Kosovo",
      "Northern Mariana Islands (Commonwealth of the)" = "Mariana Islands",
      "occupied Palestinian territory, including east Jerusalem" = "Palestine",
      "The United Kingdom" = "UK",
      "Pitcairn Islands" = "Pitcairn",
      "Republic of Korea" = "South Korea",
      "Republic of Moldova" = "Moldova",
      "United Republic of Tanzania" = "Tanzania",
      "United States Virgin Islands" = "US Virgin Islands",
    )
  ) %>%
  filter(
    !country_name %in% c("Global", "Other", "Bonaire", "Sint Eustatius", "Saba")
  )

compare_vectors(
  covid_special_countries$country_name, country_special_fixed$name,
  "covid", "codes"
) %>%
  select(covid) %>%
  filter(!is.na(covid))

# Flu surveillance countries ==================================================

flu_surveillance_renamed <- flu_surveillance_raw %>%
  select(
    country_name = Country, year = Year, week = Week,
    week_start_date = SDATE, week_end_date = EDATE,
    received = SPEC_RECEIVED_NB, processed = SPEC_PROCESSED_NB,
    a_h1 = AH1, a_h1n1pdm = AH1N12009,
    a_h3 = AH3, a_h5 = AH5,
    a_not_subtyped = ANOTSUBTYPED,
    a_total = INF_A,
    b_yam = BYAMAGATA, b_vic = BVICTORIA,
    b_undetermined = BNOTDETERMINED,
    b_total = INF_B,
    total_pos = ALL_INF,
    total_neg = ALL_INF2,
  )

compare_vectors(
  flu_surveillance_renamed$country_name, country_special_fixed$name,
  "flu", "codes"
) %>%
  select(flu) %>%
  filter(!is.na(flu))

# Special cases
flu_surveillance_special_fixed <- flu_surveillance_renamed %>%
  mutate(
    country_name = recode(
      country_name,
      "Côte d'Ivoire" = "Ivory Coast",
      "Democratic People's Republic of Korea" = "North Korea",
      "Democratic Republic of the Congo" = "West Congo",
      "Kosovo (in accordance with Security Council resolution 1244 (1999))" =
        "Kosovo",
      "West Bank and Gaza Strip" = "Palestine",
      "United Kingdom of Great Britain and Northern Ireland" = "UK",
      "Republic of Korea" = "South Korea",
      "Republic of Moldova" = "Moldova",
      "United Republic of Tanzania" = "Tanzania",
      "Turks and Caicos Is." = "Turks and Caicos Islands"
    )
  )

compare_vectors(
  flu_surveillance_special_fixed$country_name, country_special_fixed$name,
  "flu", "codes"
) %>%
  select(flu) %>%
  filter(!is.na(flu))
