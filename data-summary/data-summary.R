library(tidyverse)

save_plot <- function(plot, name, ...) {
  ggdark::ggsave_dark(
    glue::glue("data-summary/{name}.pdf"), plot,
    units = "cm", ...
  )
}

source("functions/monday_from_week_year.R")

# The data ====================================================================

read_data <- \(name) read_csv(glue::glue("data/{name}.csv"), col_types = cols())

# Relevant range for all summaries
# 2020-03-02 -> 2021-05-30, weeks are Mon-Sun (isoweek)
filter_dates <- \(d) d %>% filter(
  (year == 2020 & week >= 10) | (year == 2021 & week <= 21)
)

stringency <- read_data("stringency") %>%
  filter_dates()

covid <- read_data("covid") %>%
  filter_dates()

flu <- read_data("flu") %>% filter_dates()

country <- read_data("country") %>%
  select(country_name = name, population_2020) %>%
  filter(!is.na(population_2020))

# =============================================================================
# Work out averages for a simple summary. Should have 1 observation per country.

flu_weekly <- flu %>%
  group_by(country_name, year, week) %>%
  summarise(.groups = "drop", case_count = sum(count, na.rm = TRUE))

covid_weekly <- covid %>%
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

flu_covid_average <- flu_covid_weekly %>%
  inner_join(country, "country_name") %>%
  mutate(rate_per_1e5 = (count * 1e5) / population_2020) %>%
  group_by(country_name, disease) %>%
  summarise(
    .groups = "drop",
    rate_per_1e5_median = median(rate_per_1e5, na.rm = TRUE)
  )

stringency_average <- stringency %>%
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
        (rate_per_1e5_median > 0 & disease == "flu") |
        (rate_per_1e5_median > 50 & stringency_median < 40 & disease == "covid")
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
    ~disease,
    ncol = 1, scales = "free_y", strip.position = "right",
    labeller = as_labeller(\(b) ifelse(b == "covid", "COVID", "Flu"))
  ) +
  coord_cartesian(xlim = c(0, 100)) +
  scale_x_continuous(
    "Median stringency",
    breaks = seq(0, 100, 10),
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

# =============================================================================
# Average timeline across countries

covid_weekly_counts <- covid %>%
  filter(!is.na(cases_new)) %>%
  group_by(country_name, week, year) %>%
  summarise(.groups = "drop", cases = sum(cases_new, na.rm = TRUE)) %>%
  mutate(disease = "covid")

flu_weekly_counts <- flu %>%
  filter(!is.na(count)) %>%
  group_by(country_name, year, week) %>%
  summarise(.groups = "drop", cases = sum(count, na.rm = TRUE)) %>%
  mutate(disease = "flu")

plot_spag <- function(data, x, y, ylab, ylim = c(NULL, NULL)) {
  xq <- rlang::enquo(x)
  yq <- rlang::enquo(y)

  data_average <- data %>%
    group_by(!!xq) %>%
    summarise(.groups = "drop", y_average = median(!!yq, na.rm = TRUE))

  data %>%
    ggplot(aes(!!xq, !!yq)) +
    ggdark::dark_theme_bw(verbose = FALSE) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_blank()
    ) +
    coord_cartesian(ylim = ylim) +
    scale_x_date("Date", expand = expansion(0, 0), breaks = "1 month") +
    scale_y_continuous(ylab, expand = expansion(0, 0)) +
    geom_line(aes(color = country_name), alpha = 0.5) +
    geom_line(aes(y = y_average), data = data_average, size = 3)
}

covid_average_time_plot <- covid_weekly_counts %>%
  inner_join(country, "country_name") %>%
  mutate(
    date_monday = monday_from_week_year(year, week),
    rate_per_1e5 = cases * 1e5 / population_2020
  ) %>%
  plot_spag(date_monday, rate_per_1e5, "COVID rate per 100,000", c(0, 2500))

flu_average_time_plot <- flu_weekly_counts %>%
  inner_join(country, "country_name") %>%
  mutate(
    date_monday = monday_from_week_year(year, week),
    rate_per_1e5 = cases * 1e5 / population_2020
  ) %>%
  plot_spag(date_monday, rate_per_1e5, "Flu rate per 100,000")

stringency_average_time_plot <- stringency %>%
  plot_spag(date, stringency, "Stringency")

theme_no_x <- theme(
  axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  axis.ticks.x = element_blank()
)

average_time_plot <- ggpubr::ggarrange(
  covid_average_time_plot + theme_no_x + theme(plot.margin = margin(5, 5, 5, 5)),
  flu_average_time_plot + theme_no_x + theme(plot.margin = margin(0, 5, 5, 5)),
  stringency_average_time_plot + theme(plot.margin = margin(0, 5, 5, 5)),
  ncol = 1,
  align = "v"
)

save_plot(average_time_plot, "average-time", width = 20, height = 25)
