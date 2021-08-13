library(tidyverse)

save_csv <- \(data, name) write_csv(data, glue::glue("data-raw/{name}.csv"))

read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv") %>%
  save_csv("covid-cases")

read_csv(
  "https://raw.githubusercontent.com/CSSEGISandData/COVID-19_Unified-Dataset/master/COVID-19_LUT.csv",
  guess_max = 1e5
) %>%
  save_csv("covid-cases-jhu-lut")

download.file(
  "https://github.com/CSSEGISandData/COVID-19_Unified-Dataset/raw/master/COVID-19.rds",
  "data-raw/covid-cases-jhu.rds",
)
