library(tidyverse)

save_plot <- function(plot, name, ext = "pdf", ...) {
  ggdark::ggsave_dark(
    glue::glue("data-summary/{name}.{ext}"), plot,
    units = "cm", ...
  )
}

source("functions/monday_from_week_year.R")

# SECTION The data

read_data <- \(name) read_csv(glue::glue("data/{name}.csv"), col_types = cols())

# NOTE(sen) Relevant range for all summaries
filter_dates <- \(d) d %>% filter(
  (year == 2020 & week >= 10) | (year == 2021 & week <= 30)
)

stringency <- read_data("stringency") %>%
  filter_dates()
max(stringency$date)

travel_restrictions <- read_data("travel-restrictions") %>% filter_dates()
max(travel_restrictions$date)

covid <- read_data("covid") %>%
  filter_dates()
max(covid$date)

covid_jhu <- read_data("covid-jhu") %>%
  filter_dates()
max(covid_jhu$date)

flu_no_seq <- read_data("flu") %>%
  filter_dates()
max(flu_no_seq$week_end_date)

flu <- read_csv("flu-seq/flu-seq.csv", col_types = cols()) %>%
  filter_dates() %>%
  filter(accompanied_by_sequence)
max(flu$week_end_date)

country <- read_data("country") %>%
  select(country_name = name, population_2020) %>%
  filter(!is.na(population_2020))

# SECTION Average for a simple summary. Should have 1 observation per country.

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
  filter(!is.na(stringency_index)) %>%
  summarise(
    .groups = "drop",
    stringency_median = median(stringency_index, na.rm = TRUE)
  )

travel_restrictions_average <- travel_restrictions %>%
  group_by(country_name) %>%
  filter(!is.na(international_travel_controls)) %>%
  summarise(
    .groups = "drop",
    travel_restrictions_median = median(international_travel_controls)
  )

countries_of_interest <- function(data) {
  data %>%
    filter(
      stringency_median %in% c(max(stringency_median), min(stringency_median)) |
        country_name %in% c("Australia", "USA", "UK", "India", "China") |
        rate_per_1e5_median %in% c(max(rate_per_1e5_median)) |
        (rate_per_1e5_median > 0.1 & disease == "flu") |
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

plot_scatter <- function(data, ylab, ylim = c(NA, NA),
                         x_name = stringency_median,
                         x_breaks = seq(0, 100, 10),
                         x_lab = "Median stringency",
                         xlim = c(0, 100),
                         x_expand = waiver()) {
  data %>%
    ggplot(aes(!!rlang::enquo(x_name), rate_per_1e5_median)) +
    theme_bw() +
    theme(
      panel.spacing = unit(0, "lines"),
      strip.background = element_blank(),
    ) +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    scale_y_continuous(ylab) +
    scale_x_continuous(
      x_lab,
      breaks = x_breaks,
      expand = x_expand
    ) +
    geom_point() +
    geom_point(
      shape = 0, size = 4, data = . %>% countries_of_interest(), color = "red"
    ) +
    ggrepel::geom_label_repel(
      aes(label = country_name), # vjust = vjust, hjust = hjust),
      data = . %>% countries_of_interest(), seed = 1,
      alpha = 0.5
    ) +
    ggrepel::geom_label_repel(
      aes(label = country_name), # vjust = vjust, hjust = hjust),
      data = . %>% countries_of_interest(), seed = 1,
      fill = NA
    )
}

flu_covid_average_with_stringency <- flu_covid_average %>%
  inner_join(stringency_average, "country_name") %>%
  inner_join(travel_restrictions_average, "country_name")

flu_av_data <- flu_covid_average_with_stringency %>%
  filter(disease == "flu")

flu_av_ylab <- "Flu rate per 100,000"

flu_av_plot <- flu_av_data %>%
  plot_scatter(flu_av_ylab)

plot_scatter_travel <- function(data, ylab, ylim = c(NA, NA)) {
  plot_scatter(
    data,
    ylab,
    ylim,
    x_name = travel_restrictions_median,
    x_breaks = 1:4, xlim = c(1, 4),
    x_lab = "Median international travel restrictions",
    x_expand = waiver()
  )
}

flu_av_plot_travel <- flu_av_data %>%
  plot_scatter_travel(flu_av_ylab)

covid_ylim <- c(0, 300)

covid_av_data <- flu_covid_average_with_stringency %>%
  filter(disease == "covid")

covid_av_ylab <- "COVID rate per 100,000"

covid_av_plot <- covid_av_data %>%
  plot_scatter(covid_av_ylab, covid_ylim)

covid_av_plot_travel <- covid_av_data %>%
  plot_scatter_travel(covid_av_ylab, covid_ylim)

covid_jhu_av_data <- flu_covid_average_with_stringency %>%
  filter(disease == "covid_jhu")

covid_jhu_av_ylab <- "COVID (JHU) rate per 100,000"

covid_jhu_av_plot <- covid_jhu_av_data %>%
  plot_scatter(covid_jhu_av_ylab, covid_ylim)

covid_jhu_av_plot_travel <- covid_jhu_av_data %>%
  plot_scatter_travel(covid_jhu_av_ylab, covid_ylim)

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

av_plots_travel <- ggpubr::ggarrange(
  covid_av_plot_travel + theme_no_x,
  covid_jhu_av_plot_travel + theme_no_x,
  flu_av_plot_travel,
  ncol = 1,
  align = "v"
)

save_plot(av_plots_travel, "average-travel", width = 17, height = 20)

# SECTION Average timeline across countries

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

flu_weekly_counts_no_seq <- flu_no_seq %>%
  filter(!is.na(count)) %>%
  group_by(country_name, year, week) %>%
  summarise(.groups = "drop", cases = sum(count, na.rm = TRUE)) %>%
  mutate(disease = "flu-no-seq")

weekly_counts <- bind_rows(
  covid_weekly_counts, covid_jhu_weekly_counts, flu_weekly_counts,
  flu_weekly_counts_no_seq
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
    if (country[index] != country[index - 1]) {
      unit <- unit + 1
    }
    result[index] <- unit
  }
  result
}

find_outliers <- function(data) {
  data %>%
    group_by(year, week, disease) %>%
    filter(rate_per_1e5 == max(rate_per_1e5)) %>%
    slice(1) %>%
    group_by(disease) %>%
    arrange(year, week) %>%
    mutate(unit = rle_but_vec(country_name, week, year)) %>%
    group_by(disease, unit) %>%
    filter(rate_per_1e5 == max(rate_per_1e5)) %>%
    filter(rate_per_1e5 > 0) %>%
    slice(1) %>%
    group_by(disease) %>%
    filter(rate_per_1e5 > 0.1 * max(rate_per_1e5)) %>%
    ungroup()
}

weekly_outliers_with_names <- weekly_counts %>%
  find_outliers()

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
      legend.position = "right",
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_blank()
    ) +
    coord_cartesian(ylim = ylim) +
    scale_x_date("Date", expand = expansion(0, 0), breaks = "1 month") +
    scale_y_continuous(ylab, expand = expansion(0, 0)) +
    scale_color_manual(
      "Country",
      values = c(
        "#ff0000",
        "#00ff00",
        "#0000ff",
        "#00ffff",
        "#dddd00",
        "#ff00ff",
        "#444444",
        "#990000",
        "#009900",
        "#000099",
        "#009999",
        "#999900",
        "#990099",
        "#999999"
      )
    ) +
    geom_line(aes(color = country_name), alpha = 1) +
    geom_line(aes(y = y_average), data = data_average, size = 1, col = "gray25", lty = "1111")
}

add_outliers <- function(plot, data, y_ceiling = NULL, xlims = lubridate::ymd("2020-05-01", "2021-08-01")) {
  if (is.null(y_ceiling)) {
    y_ceiling <- max(data$rate_per_1e5)
  }
  plot +
    ggtext::geom_richtext(
      aes(label = country_name),
      data = data,
      angle = 90,
      hjust = if_else(
        data$rate_per_1e5 >= 0.7 * y_ceiling |
          (data$country_name == "Nepal" & data$date_monday > lubridate::ymd("2021-07-01")),
        1, 0
      ),
      vjust = case_when(
        data$date_monday >= xlims[[2]] - lubridate::ddays(7) ~ 0,
        data$date_monday <= xlims[[1]] + lubridate::ddays(14) ~ 1,
        TRUE ~ 0.5
      ),
      size = 3.3,
      label.padding = unit(c(0.01, 0.01, 0.01, 0.01), "lines"),
      label.size = 0,
      label.color = NA,
      alpha = 0.8
    )
}

covid_ylim_time <- c(0, 4000)

# NOTE(sen) Can't have manual color scale for all the countries, so turn off
# those plots

# covid_average_time_plot <- weekly_counts %>%
#   filter(disease == "covid") %>%
#   plot_spag(date_monday, rate_per_1e5, "COVID rate per 100,000", covid_ylim_time) %>%
#   add_outliers(weekly_outliers_with_names %>% filter(disease == "covid"), covid_ylim_time[[2]])

# covid_jhu_average_time_plot <- weekly_counts %>%
#   filter(disease == "covid_jhu") %>%
#   plot_spag(date_monday, rate_per_1e5, "COVID (JHU) rate per 100,000", covid_ylim_time) %>%
#   add_outliers(weekly_outliers_with_names %>% filter(disease == "covid_jhu"), covid_ylim_time[[2]])

# flu_average_time_plot <- weekly_counts %>%
#   filter(disease == "flu") %>%
#   plot_spag(date_monday, rate_per_1e5, "Flu rate per 100,000", ) %>%
#   add_outliers(weekly_outliers_with_names %>% filter(disease == "flu"))

# stringency_average_time_plot <- stringency %>%
#   plot_spag(date, stringency_index, "Stringency")

# travel_restrictions_average_time_plot <- travel_restrictions %>%
#   plot_spag(date, international_travel_controls, "Travel restrictions")

# average_time_plot <- ggpubr::ggarrange(
#   covid_average_time_plot + theme_no_x,
#   flu_average_time_plot + theme_no_x,
#   stringency_average_time_plot + theme_no_x,
#   travel_restrictions_average_time_plot,
#   ncol = 1,
#   align = "v",
#   heights = c(1, 1, 1, 1.28)
# )

# save_plot(average_time_plot, "average-time", width = 20, height = 30)

# SECTION Look at the countries who have had flu spikes after 2020-05-01

cutoff_date_flu <- lubridate::ymd("2020-05-01")

weekly_counts_past_may2020 <- weekly_counts %>%
  filter(date_monday >= cutoff_date_flu)

countries_with_flu <- weekly_counts_past_may2020 %>%
  filter(disease == "flu", rate_per_1e5 > 0.02) %>%
  pull(country_name) %>%
  unique()

length(countries_with_flu)

# NOTE(sen) Separate Asia and Africa
countries_with_flu_asia <- c("Afghanistan", "Bangladesh", "Cambodia", "China", "Lao", "Nepal", "Timor-Leste")
all(countries_with_flu_asia %in% countries_with_flu)
countries_with_flu_africa <- setdiff(countries_with_flu, countries_with_flu_asia)

fun_average_with_flu <- function(countries_with_flu) {
  weekly_counts_countries_with_flu <- weekly_counts_past_may2020 %>%
    filter(country_name %in% countries_with_flu)

  weekly_outliers_with_names_with_flu <- weekly_counts_countries_with_flu %>%
    find_outliers()

  covid_ylim_time_with_flu <- c(0, 300)
  flu_ylim_time_with_flu <- c(0, 0.55)

  covid_average_time_plot_with_flu <- weekly_counts_countries_with_flu %>%
    filter(disease == "covid") %>%
    plot_spag(date_monday, rate_per_1e5, "COVID rate per 100,000", covid_ylim_time_with_flu) %>%
    add_outliers(weekly_outliers_with_names_with_flu %>% filter(disease == "covid"), covid_ylim_time_with_flu[[2]])

  covid_jhu_average_time_plot_with_flu <- weekly_counts_countries_with_flu %>%
    filter(disease == "covid_jhu") %>%
    plot_spag(date_monday, rate_per_1e5, "COVID (JHU) rate per 100,000", covid_ylim_time_with_flu) %>%
    add_outliers(weekly_outliers_with_names_with_flu %>% filter(disease == "covid_jhu"), covid_ylim_time_with_flu[[2]])

  flu_average_time_plot_with_flu <- weekly_counts_countries_with_flu %>%
    filter(disease == "flu") %>%
    plot_spag(date_monday, rate_per_1e5, "Flu rate per 100,000", flu_ylim_time_with_flu) %>%
    add_outliers(weekly_outliers_with_names_with_flu %>% filter(disease == "flu"), flu_ylim_time_with_flu[[2]])

  stringency_average_time_plot_with_flu <- stringency %>%
    filter(date > cutoff_date_flu, country_name %in% countries_with_flu) %>%
    plot_spag(date, stringency_index, "Stringency")

  travel_restrictions_average_time_plot_with_flu <- travel_restrictions %>%
    filter(date > cutoff_date_flu, country_name %in% countries_with_flu) %>%
    plot_spag(date, international_travel_controls, "Travel restrictions")

  average_time_plot_with_flu <- ggpubr::ggarrange(
    covid_average_time_plot_with_flu + theme_no_x,
    flu_average_time_plot_with_flu + theme_no_x,
    stringency_average_time_plot_with_flu + theme_no_x,
    travel_restrictions_average_time_plot_with_flu,
    ncol = 1,
    align = "v",
    heights = c(1, 1, 1, 1.28),
    common.legend = TRUE
  )

  average_time_plot_with_flu
}

average_time_plot_with_flu <- fun_average_with_flu(countries_with_flu)
average_time_plot_with_flu_asia <- fun_average_with_flu(countries_with_flu_asia)
average_time_plot_with_flu_africa <- fun_average_with_flu(countries_with_flu_africa)

save_plot(average_time_plot_with_flu, "average-time-with-flu", width = 20, height = 30)
save_plot(average_time_plot_with_flu_asia, "average-time-with-flu-asia", width = 20, height = 30)
save_plot(average_time_plot_with_flu_africa, "average-time-with-flu-africa", width = 20, height = 30)

one_country_plot <- function(data, covid_ylim, theme_no_x) {
  covid_average_time_plot_with_flu <- data %>%
    filter(disease == "covid") %>%
    plot_spag(date_monday, rate_per_1e5, "COVID rate per 100,000", covid_ylim)

  covid_jhu_average_time_plot_with_flu <- data %>%
    filter(disease == "covid_jhu") %>%
    plot_spag(date_monday, rate_per_1e5, "COVID (JHU) rate per 100,000", covid_ylim)

  flu_average_time_plot_with_flu <- data %>%
    filter(disease == "flu") %>%
    plot_spag(date_monday, rate_per_1e5, "Flu rate per 100,000", )

  stringency_average_time_plot_with_flu <- stringency %>%
    filter(date > cutoff_date_flu, country_name == unique(data$country_name)) %>%
    plot_spag(date, stringency_index, "Stringency", c(0, 100))

  travel_restrictions_average_time_plot_with_flu <- travel_restrictions %>%
    filter(date > cutoff_date_flu, country_name == unique(data$country_name)) %>%
    plot_spag(date, international_travel_controls, "Travel restrictions", c(0, 4))

  plot <- ggpubr::ggarrange(
    covid_average_time_plot_with_flu + theme_no_x,
    flu_average_time_plot_with_flu + theme_no_x,
    stringency_average_time_plot_with_flu + theme_no_x,
    travel_restrictions_average_time_plot_with_flu,
    ncol = 1,
    align = "v",
    heights = c(1, 1, 1, 1.28)
  )

  attr(plot, "country_name") <- unique(data$country_name)

  plot
}

country_ind_plots <- weekly_counts_countries_with_flu %>%
  group_by(country_name) %>%
  group_split() %>%
  map(one_country_plot, covid_ylim_time_with_flu, theme_no_x)

if (dir.exists("data-summary/country-ind")) {
  unlink("data-summary/country-ind", recursive = TRUE)
}
dir.create("data-summary/country-ind")
walk(
  country_ind_plots,
  ~ save_plot(
    .x, paste0("country-ind/", attr(.x, "country_name")), "png",
    width = 15, height = 20
  )
)
