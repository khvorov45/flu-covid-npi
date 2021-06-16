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

covid_jhu <- read_data("covid-jhu") %>%
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

covid_weekly_jhu <- covid_jhu %>%
  group_by(country_name, year, week) %>%
  summarise(.groups = "drop", case_count = sum(cases_new, na.rm = TRUE))

covid_weekly %>% arrange(case_count)
covid_weekly_jhu %>% arrange(case_count)

flu_covid_weekly <- flu_weekly %>%
  mutate(disease = "flu") %>%
  bind_rows(covid_weekly %>% mutate(disease = "covid")) %>%
  bind_rows(covid_weekly_jhu %>% mutate(disease = "covid_jhu"))

flu_covid_average <- flu_covid_weekly %>%
  inner_join(country, "country_name") %>%
  mutate(rate_per_1e5 = (case_count * 1e5) / population_2020) %>%
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

plot_scatter <- function(data, ylab, ylim = c(NA, NA)) {
  data %>%
    ggplot(aes(stringency_median, rate_per_1e5_median)) +
    theme_bw() +
    theme(
      panel.spacing = unit(0, "lines"),
      strip.background = element_blank(),
    ) +
    coord_cartesian(xlim = c(0, 100), ylim = ylim) +
    scale_y_continuous(ylab) +
    scale_x_continuous(
      "Median stringency",
      breaks = seq(0, 100, 10),
      expand = expansion(0, 0)
    ) +
    geom_point() +
    geom_point(
      shape = 0, size = 4, data = . %>% countries_of_interest(), color = "red"
    ) +
    geom_label(
      aes(label = country_name, vjust = vjust, hjust = hjust),
      data = . %>% countries_of_interest(),
      alpha = 0.5
    )
}

flu_covid_average_with_stringency <- flu_covid_average %>%
  inner_join(stringency_average, "country_name")

flu_av_plot <- flu_covid_average_with_stringency %>%
  filter(disease == "flu") %>%
  plot_scatter("Flu rate per 100,000")

covid_ylim <- c(0, 300)

covid_av_plot <- flu_covid_average_with_stringency %>%
  filter(disease == "covid") %>%
  plot_scatter("COVID rate per 100,000", covid_ylim)

covid_jhu_av_plot <- flu_covid_average_with_stringency %>%
  filter(disease == "covid_jhu") %>%
  plot_scatter("COVID (JHU) rate per 100,000", covid_ylim)

theme_no_x <- theme(
  axis.text.x = element_blank(),
  axis.title.x = element_blank(),
  axis.ticks.x = element_blank()
)

av_plots <- ggpubr::ggarrange(
  covid_av_plot + theme_no_x,
  covid_jhu_av_plot + theme_no_x,
  flu_av_plot,
  ncol = 1,
  align = "v"
)

save_plot(av_plots, "average", width = 17, height = 20)

# =============================================================================
# Average timeline across countries

covid_weekly_counts <- covid %>%
  filter(!is.na(cases_new)) %>%
  group_by(country_name, week, year) %>%
  summarise(.groups = "drop", cases = sum(cases_new, na.rm = TRUE)) %>%
  mutate(disease = "covid")

covid_jhu_weekly_counts <- covid_jhu %>%
  filter(!is.na(cases_new)) %>%
  group_by(country_name, week, year) %>%
  summarise(.groups = "drop", cases = sum(cases_new, na.rm = TRUE)) %>%
  mutate(disease = "covid_jhu")

flu_weekly_counts <- flu %>%
  filter(!is.na(count)) %>%
  group_by(country_name, year, week) %>%
  summarise(.groups = "drop", cases = sum(count, na.rm = TRUE)) %>%
  mutate(disease = "flu")

weekly_counts <- bind_rows(
  covid_weekly_counts, covid_jhu_weekly_counts, flu_weekly_counts
) %>%
  inner_join(country, "country_name") %>%
  mutate(
    date_monday = monday_from_week_year(year, week),
    rate_per_1e5 = cases * 1e5 / population_2020
  ) %>%
  filter(population_2020 > 10000)

rle_but_vec <- function(country, week, year) {
  unit <- 0
  result <- c(unit)
  for (index in 2:length(country)) {
    if (country[index] != country[index - 1] |
      (year[index] == year[index - 1] & week[index] - week[index - 1] > 1)) {
      unit <- unit + 1
    }
    result[index] <- unit
  }
  result
}

weekly_outliers_with_names %>% arrange(desc(year), desc(week))

weekly_outliers_with_names <- weekly_counts %>%
  group_by(year, week, disease) %>%
  filter(rate_per_1e5 == max(rate_per_1e5)) %>%
  group_by(disease) %>%
  mutate(unit = rle_but_vec(country_name, week, year)) %>%
  group_by(disease, unit) %>%
  filter(rate_per_1e5 == max(rate_per_1e5)) %>%
  filter(rate_per_1e5 > 0, !(year == 2021 & week == 21)) %>%
  ungroup()

plot_spag <- function(data, x, y, ylab, ylim = c(NULL, NULL)) {
  xq <- rlang::enquo(x)
  yq <- rlang::enquo(y)

  data_average <- data %>%
    group_by(!!xq) %>%
    summarise(.groups = "drop", y_average = median(!!yq, na.rm = TRUE))

  data %>%
    ggplot(aes(!!xq, !!yq)) +
    theme_bw() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_blank()
    ) +
    coord_cartesian(ylim = ylim) +
    scale_x_date("Date", expand = expansion(0, 0), breaks = "1 month") +
    scale_y_continuous(ylab, expand = expansion(0, 0)) +
    geom_line(aes(color = country_name), alpha = 0.5) +
    geom_line(aes(y = y_average), data = data_average, size = 1, col = "black")
}

add_outliers <- function(plot, data) {
  plot +
    geom_text(
      aes(label = country_name),
      data = data,
      vjust = if_else(
        data$country_name == "Seychelles" & data$disease == "covid_jhu", 0, 0.5
      ),
      angle = 90,
      hjust = if_else(
        data$country_name == "Seychelles" & data$disease == "covid_jhu", 0.5, 0
      )
    )
}

covid_ylim_time <- c(0, 4000)

covid_average_time_plot <- weekly_counts %>%
  filter(disease == "covid") %>%
  plot_spag(date_monday, rate_per_1e5, "COVID rate per 100,000", covid_ylim_time) %>%
  add_outliers(weekly_outliers_with_names %>% filter(disease == "covid"))

covid_jhu_average_time_plot <- weekly_counts %>%
  filter(disease == "covid_jhu") %>%
  plot_spag(date_monday, rate_per_1e5, "COVID (JHU) rate per 100,000", covid_ylim_time) %>%
  add_outliers(weekly_outliers_with_names %>% filter(disease == "covid_jhu"))

flu_average_time_plot <- weekly_counts %>%
  filter(disease == "flu") %>%
  plot_spag(date_monday, rate_per_1e5, "Flu rate per 100,000", ) %>%
  add_outliers(weekly_outliers_with_names %>% filter(disease == "flu"))

stringency_average_time_plot <- stringency %>%
  plot_spag(date, stringency, "Stringency")

average_time_plot <- ggpubr::ggarrange(
  covid_average_time_plot + theme_no_x,
  covid_jhu_average_time_plot + theme_no_x,
  flu_average_time_plot + theme_no_x,
  stringency_average_time_plot,
  ncol = 1,
  align = "v"
)

save_plot(average_time_plot, "average-time", width = 20, height = 30)
