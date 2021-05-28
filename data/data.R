library(tidyverse)

read_raw_csv <- function(name, col_types = cols(), ...) {
  read_csv(glue::glue("data-raw/{name}.csv"), col_types = col_types, ...)
}

read_raw_xls <- function(name, ...) {
  readxl::read_excel(glue::glue("data-raw/{name}.xls"), ...)
}

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
