library(tidyverse)

save_plot <- function(plot, name, ...) {
  ggdark::ggsave_dark(
    glue::glue("data-summary/{name}.pdf"), plot, units = "cm", ...
  )
}

# The data
read_data <- \(name) read_csv(glue::glue("data/{name}.csv"), col_types = cols())
stringency <- read_data("stringency")
covid <- read_data("covid")
flu <- read_data("flu")
country <- read_data("country") %>%
  select(country_name = name, population_2020) %>%
  filter(!is.na(population_2020))

# =============================================================================
# Work out averages for a simple summary. Should have 1 observation per country.

flu_weekly <- flu %>%
  group_by(country_name, year, week) %>%
  summarise(.groups = "drop", case_count = sum(count, na.rm = TRUE))

extract_week_and_year <- \(d) d %>%
  mutate(week = lubridate::isoweek(date), year = lubridate::year(date))

covid_weekly <- covid %>%
  extract_week_and_year() %>%
  group_by(country_name, year, week) %>%
  summarise(.groups = "drop", case_count = sum(cases_new, na.rm = TRUE))

flu_covid_weekly <- flu_weekly %>%
  rename(flu_case_count = case_count) %>%
  full_join(
    covid_weekly %>% rename(covid_case_count = case_count),
    c("country_name", "year", "week")
  ) %>%
  pivot_longer(
    contains("case_count"),
    names_to = "disease", values_to = "count"
  ) %>%
  mutate(disease = str_replace(disease, "_case_count", "")) %>%
  filter(!is.na(count))

filter_dates <- \(d) d %>% filter(
  # 2020-03-02 -> 2021-05-30, weeks are Mon-Sun (isoweek)
  (year == 2020 & week >= 10) | (year == 2021 & week <= 21)
)

flu_covid_average <- flu_covid_weekly %>%
  inner_join(country, "country_name") %>%
  filter_dates() %>%
  mutate(
    rate_per_1e5 = (count * 1e5) / population_2020
  ) %>%
  group_by(country_name, disease) %>%
  summarise(
    .groups = "drop",
    rate_per_1e5_median = median(rate_per_1e5, na.rm = TRUE)
  )

stringency_average <- stringency %>%
  extract_week_and_year() %>%
  filter_dates() %>%
  group_by(country_name) %>%
  filter(!is.na(stringency)) %>%
  summarise(
    .groups = "drop",
    stringency_median = median(stringency, na.rm = TRUE)
  )

countries_of_interest <- function(data) {
  data %>%
    filter(
      stringency_median %in% c(max(stringency_median), min(stringency_median)) |
        country_name %in% c("Australia", "USA", "UK", "India", "China") |
        rate_per_1e5_median %in% c(max(rate_per_1e5_median)) |
        (rate_per_1e5_median > 0 & disease == "flu")
    ) %>%
    mutate(
      vjust = case_when(
        rate_per_1e5_median == max(rate_per_1e5_median) ~ 1.5,
        country_name == "UK" & disease == "flu" ~ 1.5,
        country_name == "USA" & disease == "flu" ~ 1.5,
        TRUE ~ -0.5
      ),
      hjust = case_when(
        stringency_median == min(stringency_median) ~ 0.3,
        country_name == "UK" & disease == "flu" ~ 0,
        TRUE ~ 0.5
      )
    )
}

average_plot <- flu_covid_average %>%
  # This will only keep the data for which we have stringency
  inner_join(stringency_average, "country_name") %>%
  ggplot(aes(stringency_median, rate_per_1e5_median)) +
  ggdark::dark_theme_bw(verbose = FALSE) +
  theme(
    panel.spacing = unit(0, "lines"),
    strip.background = element_blank(),
  ) +
  facet_wrap(
    ~disease, ncol = 1, scales = "free_y", strip.position = "right",
    labeller = as_labeller(\(b) ifelse(b == "covid", "COVID", "Flu"))
  ) +
  coord_cartesian(xlim = c(0, 100)) +
  scale_x_continuous(
    "Median stringency", breaks = seq(0, 100, 10),
    expand = expansion(0, 0)
  ) +
  scale_y_continuous("Median rate per 100,000") +
  geom_point() +
  geom_point(
    shape = 0, size = 4, data = . %>% countries_of_interest(), color = "red"
  ) +
  geom_label(
    aes(label = country_name, vjust = vjust, hjust = hjust),
    data = . %>% countries_of_interest(),
    alpha = 0.5
  )

save_plot(average_plot, "average", width = 17, height = 15)
