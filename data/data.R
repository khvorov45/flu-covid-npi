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

save_data <- \(data, name) write_csv(data, glue::glue("data/{name}.csv"))

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

sequences_raw <- read_raw_xls("gisaid_epiflu_isolates", guess_max = 1e5)

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

# There are some '(the)' appearances in country codes
country_codes_the_fixed <- country_rks_fixed %>%
  mutate(name = str_replace(name, "\\(the\\)", "") %>% str_trim())

# Special cases
country_special_fixed <- country_codes_the_fixed %>%
  mutate(
    name = recode(name,
      "Virgin Islands (British)" = "British Virgin Islands",
      "Côte d'Ivoire" = "Ivory Coast",
      "Korea (the Democratic People's Republic of)" = "North Korea",
      "Congo (the Democratic Republic of the)" = "East Congo",
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
      "Bolivia (Plurinational State of)" = "Bolivia",
      "Brunei Darussalam" = "Brunei",
      "Lao People's Democratic Republic" = "Lao",
      "Taiwan (Province of China)" = "Taiwan",
      "Viet Nam" = "Vietnam"
    )
  ) %>%
  filter(name != "Bonaire, Sint Eustatius and Saba")

covid_special_countries <- covid_cases_renamed %>%
  mutate(
    country_name = recode(
      country_name,
      "Côte d’Ivoire" = "Ivory Coast",
      "Democratic People's Republic of Korea" = "North Korea",
      "Democratic Republic of the Congo" = "East Congo",
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
      "Bolivia (Plurinational State of)" = "Bolivia",
      "Brunei Darussalam" = "Brunei",
      "Viet Nam" = "Vietnam",
      "Lao People's Democratic Republic" = "Lao",
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
    count_received = SPEC_RECEIVED_NB,
    count_processed = SPEC_PROCESSED_NB,
    count_a_h1 = AH1,
    count_a_h1n1pdm = AH1N12009,
    count_a_h3 = AH3,
    count_a_h5 = AH5,
    count_a_not_subtyped = ANOTSUBTYPED,
    count_a_total = INF_A,
    count_b_yam = BYAMAGATA,
    count_b_vic = BVICTORIA,
    count_b_undetermined = BNOTDETERMINED,
    count_b_total = INF_B,
    count_total_pos = ALL_INF,
    count_total_neg = ALL_INF2,
  )

# Special cases
flu_surveillance_special_fixed <- flu_surveillance_renamed %>%
  mutate(
    country_name = recode(
      country_name,
      "Côte d'Ivoire" = "Ivory Coast",
      "Democratic People's Republic of Korea" = "North Korea",
      "Democratic Republic of the Congo" = "East Congo",
      "Kosovo (in accordance with Security Council resolution 1244 (1999))" =
        "Kosovo",
      "West Bank and Gaza Strip" = "Palestine",
      "United Kingdom of Great Britain and Northern Ireland" = "UK",
      "Republic of Korea" = "South Korea",
      "Republic of Moldova" = "Moldova",
      "United Republic of Tanzania" = "Tanzania",
      "Turks and Caicos Is." = "Turks and Caicos Islands",
      "Bolivia (Plurinational State of)" = "Bolivia",
      "Viet Nam" = "Vietnam",
      "Lao People's Democratic Republic" = "Lao",
    )
  )

compare_vectors(
  flu_surveillance_special_fixed$country_name, country_special_fixed$name,
  "flu", "codes"
) %>%
  select(flu) %>%
  filter(!is.na(flu))

# Sequences countries =========================================================

sequences_renamed <- sequences_raw %>%
  select(
    virus_name = Isolate_Name, subtype = Subtype, lineage = Lineage,
    location = Location
  )

sequences_countries <- sequences_renamed %>%
  mutate(
    country_name = location %>%
      str_split("/") %>%
      map(2) %>%
      as.character() %>%
      str_trim() %>%
      recode(
        "NULL" = "Russian Federation",
        "United States" = "United States of America",
        "United Kingdom" = "UK",
        "Bolivia, Plurinationial State of" = "Bolivia",
        "Congo, the Democatic Republic of" = "East Congo",
        "Cote d'Ivoire" = "Ivory Coast",
        "Czech Republic" = "Czechia",
        "Hong Kong (SAR)" = "Hong Kong",
        "Korea, Republic of" = "South Korea",
        "Lao, People's Democratic Republic" = "Lao",
        "Macedonia, the former Yogoslav Republic of" = "North Macedonia",
        "Moldova, Republic of" = "Moldova",
        "Saint Kitts and Nevis, Federation of" = "Saint Kitts and Nevis",
      )
  )

compare_vectors(
  sequences_countries$country_name, country_special_fixed$name,
  "sequences", "codes"
) %>%
  select(sequences) %>%
  filter(!is.na(sequences))

# Subtype encoding ============================================================

sequence_subtype_fixed <- sequences_countries %>%
  mutate(
    lineage = lineage %>% recode("Victoria" = "Vic", "Yamagata" = "Yam"),
    subtype = subtype %>%
      str_replace("A / ", "") %>%
      str_trim() %>%
      paste0(if_else(is.na(lineage), "", lineage)) %>%
      recode(
        "H1N1" = "H1",
        "H1N1pdm09" = "H1",
        "H1N2" = "H1",
        "H3N2" = "H3",
        "H5N1" = "H5",
        "H5N6" = "H5",
        "H5N8" = "H5",
        "H9N2" = "H9"
      ),
  )


flu_subtypes <- flu_surveillance_special_fixed %>%
  pivot_longer(
    contains("count_"),
    names_to = "count_name", values_to = "count"
  ) %>%
  mutate(
    count_name = count_name %>% str_replace("count_", "")
  ) %>%
  filter(!count_name %in% c(
    "received", "processed", "total_pos", "b_total", "a_total", "total_neg"
  )) %>%
  mutate(
    subtype = count_name %>% recode(
      "a_h1" = "H1",
      "a_h3" = "H3",
      "a_h1n1pdm" = "H1",
      "a_h5" = "H5",
      "a_not_subtyped" = "A",
      "b_vic" = "BVic",
      "b_yam" = "BYam",
      "b_undetermined" = "B"
    )
  )

compare_vectors(
  sequence_subtype_fixed$subtype,
  flu_subtypes$subtype, "seq", "flu"
)

# Save data ===================================================================

sequence_final <- sequence_subtype_fixed
flu_final <- flu_subtypes
country_final <- country_special_fixed
covid_final <- covid_special_countries

compare_vectors(
  sequence_final$subtype,
  flu_final$subtype, "seq", "flu"
)
compare_vectors(
  sequence_final$country_name,
  country_final$name, "seq", "country"
) %>%
  select(seq) %>%
  filter(!is.na(seq))

save_data(sequence_final, "sequence")

compare_vectors(
  flu_final$country_name,
  country_final$name, "flu", "country"
) %>%
  select(flu) %>%
  filter(!is.na(flu))

save_data(flu_final, "flu")

compare_vectors(
  covid_final$country_name,
  country_final$name, "covid", "country"
) %>%
  select(covid) %>%
  filter(!is.na(covid))

save_data(covid_final, "covid")

compare_vectors(
  stringency_rks_fixed$country_code,
  country_final$code3, "stringency", "country"
) %>%
  select(stringency) %>%
  filter(!is.na(stringency))

stringency_final <- stringency_rks_fixed %>%
  inner_join(
    country_final %>% select(code3, country_name = name),
    by = c("country_code" = "code3")
  ) %>%
  select(-country_code)

compare_vectors(
  stringency_final$country_name,
  country_final$name, "stringency", "country"
) %>%
  select(stringency) %>%
  filter(!is.na(stringency))

save_data(stringency_final, "stringency")

save_data(country_final, "country")
