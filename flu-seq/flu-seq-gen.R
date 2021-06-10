library(tidyverse)

source("functions/monday_from_week_year.R")
Rcpp::sourceCpp("flu-seq/flu-seq-gen.cpp")

# Data ========================================================================

read_data <- \(name) read_csv(glue::glue("data/{name}.csv"), col_types = cols())

flu <- read_data("flu")

stringency <- read_data("stringency")

sequence <- read_data("sequence")

# =============================================================================

# Mark each count as being accomponied by a sequence within some
# arbitrary time period (e.g. 3 weeks each way)
is_accompanied_by_sequence <- function(subtype_value, country_value,
                                       year, week,
                                       sequence,
                                       before = 3, after = before) {
  date_monday <- monday_from_week_year(year, week)
  date_sunday <- date_monday + 6
  min_date <- date_monday - before * 7
  max_date <- date_sunday + after * 7

  is_accompanied_by_sequence_cpp(
    subtype_value, country_value, min_date, max_date,
    sequence
  )
}

subtype_value <- c("H1", "H1")
country_value <- c("USA", "USA")
year <- c(2020, 2020)
week <- c(1, 25)

is_accompanied_by_sequence(
  subtype_value, country_value, year, week, sequence
)

is_accompanied_by_sequence("H1", "USA", 2020, 25, sequence)

# This takes a while
flu_seq <- flu %>%
  mutate(
    accompanied_by_sequence = is_accompanied_by_sequence(
      subtype, country_name, year, week,
      sequence
    )
  )

write_csv(flu_seq, "flu-seq/flu-seq.csv")
