library(tidyverse)

save_csv <- \(data, name) write_csv(data, glue::glue("data-raw/{name}.csv"))

read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv") %>%
  save_csv("covid-cases")

stringency <- httr::GET(
  "https://covidtrackerapi.bsg.ox.ac.uk/api/v2/stringency/date-range/2020-01-01/2021-12-31",
  httr::config(timeout = 1e6),
  httr::progress()
)

stringency_content <- stringency %>% httr::content()

stringency_dfr <- map_dfr(
  stringency_content$data,
  \(per_day) map_dfr(
    per_day,
    \(row) as_tibble(map(row, \(value) ifelse(is.null(value), NA, value)))
  )
)

save_csv(stringency_dfr, "stringency")
