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

extract_week_and_year <- \(d) d %>%
  mutate(week = lubridate::isoweek(date), year = lubridate::year(date))

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

country_populations_raw <- read_raw_csv("country-population")

# Country names have to match =================================================

# Country codes ===============================================================

# Add Kosovo
country_codes_raw %>% filter(code2 == "KS" | code3 == "KOS" | name == "Kosovo")
country_rks_fixed <- country_codes_raw %>%
  bind_rows(tibble(name = "Kosovo", code2 = "KS", code3 = "KOS", code_num = NA))

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
      "Congo (the Democratic Republic of the)" = "DR Congo",
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
      "Viet Nam" = "Vietnam",
      "Bonaire, Sint Eustatius and Saba" = "Caribbean Netherlands",
      "Iran (Islamic Republic of)" = "Iran",
      "Venezuela (Bolivarian Republic of)" = "Venezuela",
      "United States of America" = "USA",
      "Syrian Arab Republic" = "Syria",
      "Saint Barthélemy" = "Saint Barthelemy",
      "Russian Federation" = "Russia",
      "Micronesia (Federated States of)" = "Micronesia",
    )
  )

# Country populations =========================================================

country_populations_renamed <- country_populations_raw %>%
  select(
    country_name = `Country (or dependency)`,
    population_2020 = `Population\n(2020)`,
  )

country_populations_fixed <- country_populations_renamed %>%
  filter(
    !country_name %in% c("Channel Islands")
  ) %>%
  mutate(
    country_name = country_name %>%
      recode(
        "Côte d'Ivoire" = "Ivory Coast",
        "Czech Republic (Czechia)" = "Czechia",
        "United States" = "USA",
        "United Kingdom" = "UK",
        "U.S. Virgin Islands" = "US Virgin Islands",
        "Turks and Caicos" = "Turks and Caicos Islands",
        "State of Palestine" = "Palestine",
        "St. Vincent & Grenadines" = "Saint Vincent and the Grenadines",
        "Northern Mariana Islands" = "Mariana Islands",
        "Laos" = "Lao",
        "Faeroe Islands" = "Faroe Islands",
      ) %>%
      str_replace("&", "and")
  )

compare_vectors(
  country_populations_fixed$country_name, country_special_fixed$name,
  "pop", "codes"
) %>%
  select(pop) %>%
  filter(!is.na(pop)) %>%
  print(n = 100)

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

compare_vectors(
  stringency_rks_fixed$country_code, country_special_fixed$code3,
  "stringency", "codes"
) %>%
  select(stringency) %>%
  filter(!is.na(stringency))

# Covid cases countries =======================================================

covid_cases_renamed <- covid_cases_raw %>%
  select(
    date = Date_reported,
    country_code = Country_code,
    country_name = Country,
    cases_new = New_cases,
    deaths_new = New_deaths
  )

compare_vectors(
  covid_cases_renamed$country_code, country_special_fixed$code2,
  "covid", "codes"
) %>%
  select(covid) %>%
  filter(!is.na(covid))

covid_cases_codes_fixed <- covid_cases_renamed %>%
  filter(
    # Bonaire, Saint Eustatious and Saba
    !country_code %in% c("XA", "XB", "XC")
  ) %>%
  mutate(
    # Kosovo
    country_code = recode(country_code, "XK" = "KS")
  )

compare_vectors(
  covid_cases_codes_fixed$country_code, country_special_fixed$code2,
  "covid", "codes"
) %>%
  select(covid) %>%
  filter(!is.na(covid))

# Covid cases from JHU countries =============================================

# The 'lookup' table (contains countries/states)
covid_jhu_lut_raw <- read_raw_csv("covid-cases-jhu-lut", guess_max = 1e5)

covid_jhu_lut_renamed <- covid_jhu_lut_raw %>%
  rename(code3 = ISO1_3C)

covid_jhu_lut_codes_fixed <- covid_jhu_lut_renamed %>%
  mutate(code3 = recode(code3, "XKX" = "KOS")) %>%
  filter(code3 != "XXX") # Cruise ships

compare_vectors(
  covid_jhu_lut_codes_fixed$code3, country_special_fixed$code3,
  "covid", "country"
) %>%
  filter(!is.na(covid)) %>%
  select(covid)

covid_jhu_lut_no_missing_id <- covid_jhu_lut_codes_fixed %>%
  mutate(ID = replace_na(ID, "NA")) # This is actually Nambia

# The actual data
covid_jhu_raw <- readRDS("data-raw/covid-cases-jhu.rds")

# Of course ID's in cases don't always match ID's in the 'lookup'
# table, what a joke
covid_jhu_ids_fixed <- covid_jhu_raw %>%
  filter(!is.na(ID), !ID %in% c("XX97", "XX99", "XXXX"))

compare_vectors(
  covid_jhu_ids_fixed$ID, covid_jhu_lut_no_missing_id$ID, "case", "lut"
) %>%
  filter(!is.na(case)) %>%
  select(case)

covid_jhu_filtered <- covid_jhu_ids_fixed %>%
  filter(Type == "Confirmed", Age == "Total", Sex == "Total") %>%
  # Multiple sources appear to report the same cases
  group_by(ID, Date) %>%
  filter(Cases_New == max(Cases_New)) %>%
  filter(row_number() == 1) %>%
  ungroup()

covid_jhu_with_names <- covid_jhu_filtered %>%
  inner_join(covid_jhu_lut_no_missing_id, "ID") %>%
  filter(Level == "Country")

compare_vectors(
  covid_jhu_with_names$code3, country_special_fixed$code3, "case", "country"
) %>%
  filter(!is.na(case)) %>%
  select(case)

covid_jhu_with_countries <- covid_jhu_with_names %>%
  inner_join(country_special_fixed, "code3")

unique(covid_jhu_with_countries$Age)
unique(covid_jhu_with_countries$Sex)
unique(covid_jhu_with_countries$Admin)
unique(covid_jhu_with_countries$Admin0)
unique(covid_jhu_with_countries$Admin1)
unique(covid_jhu_with_countries$Admin2)
unique(covid_jhu_with_countries$Admin3)

covid_jhu_with_countries %>%
  filter(name == "USA", Date > lubridate::ymd("2020-05-01")) %>%
  group_by(ID, Date) %>%
  filter(n() > 1)

covid_jhu_with_countries_renamed <- covid_jhu_with_countries %>%
  select(
    cases_new = Cases_New, date = Date,
    country_name = name
  )

covid_jhu_summarised <- covid_jhu_with_countries_renamed %>%
  group_by(date, country_name) %>%
  summarise(
    .groups = "drop",
    cases_new = sum(cases_new, na.rm = TRUE),
  )

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
      "Democratic Republic of the Congo" = "DR Congo",
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
      "Iran (Islamic Republic of)" = "Iran",
      "Russian Federation" = "Russia",
      "Syrian Arab Republic" = "Syria",
      "United States of America" = "USA",
      "Venezuela (Bolivarian Republic of)" = "Venezuela",
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
    location = Location, date = Collection_Date
  )

sequences_countries <- sequences_renamed %>%
  mutate(
    country_name = location %>%
      str_split("/") %>%
      map(2) %>%
      as.character() %>%
      str_trim() %>%
      recode(
        "NULL" = "Russia",
        "United States" = "USA",
        "United Kingdom" = "UK",
        "Bolivia, Plurinationial State of" = "Bolivia",
        "Congo, the Democatic Republic of" = "DR Congo",
        "Cote d'Ivoire" = "Ivory Coast",
        "Czech Republic" = "Czechia",
        "Hong Kong (SAR)" = "Hong Kong",
        "Korea, Republic of" = "South Korea",
        "Lao, People's Democratic Republic" = "Lao",
        "Macedonia, the former Yogoslav Republic of" = "North Macedonia",
        "Moldova, Republic of" = "Moldova",
        "Saint Kitts and Nevis, Federation of" = "Saint Kitts and Nevis",
        "Russian Federation" = "Russia",
      )
  )

compare_vectors(
  sequences_countries$country_name, country_special_fixed$name,
  "sequences", "codes"
) %>%
  select(sequences) %>%
  filter(!is.na(sequences))

# Sequences dates ============================================================

sequence_date_fixed <- sequences_countries %>%
  # There will be NA's corresponding to incomplete dates (e.g. '2021')
  mutate(date = lubridate::ymd(date, quiet = TRUE)) %>%
  extract_week_and_year()

# Subtype encoding ============================================================

sequence_subtype_fixed <- sequence_date_fixed %>%
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

country_final <- country_special_fixed %>%
  full_join(country_populations_fixed, c("name" = "country_name"))

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
  covid_cases_codes_fixed$country_code,
  country_final$code2, "covid", "country"
) %>%
  select(covid) %>%
  filter(!is.na(covid))

covid_final <- covid_cases_codes_fixed %>%
  extract_week_and_year() %>%
  select(-country_name) %>%
  inner_join(
    select(country_final, code2, country_name = name),
    c("country_code" = "code2")
  ) %>%
  select(-country_code)

compare_vectors(
  covid_final$country_name,
  country_final$name, "covid", "country"
) %>%
  select(covid) %>%
  filter(!is.na(covid))

save_data(covid_final, "covid")

covid_jhu_final <- covid_jhu_summarised %>%
  extract_week_and_year()

compare_vectors(
  covid_jhu_final$country_name,
  country_final$name, "covid", "country"
) %>%
  select(covid) %>%
  filter(!is.na(covid))

save_data(covid_jhu_final, "covid-jhu")

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
  select(-country_code, date = date_value) %>%
  extract_week_and_year()

compare_vectors(
  stringency_final$country_name,
  country_final$name, "stringency", "country"
) %>%
  select(stringency) %>%
  filter(!is.na(stringency))

save_data(stringency_final, "stringency")

save_data(country_final, "country")
