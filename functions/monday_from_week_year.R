monday_from_week_year <- function(year, week) {
  start <- lubridate::ymd(glue::glue("{year}-01-01"))
  start[lubridate::isoweek(start) != 1] <-
    start[lubridate::isoweek(start) != 1] + 7
  stopifnot(all(lubridate::isoweek(start) == 1))
  lubridate::wday(start, week_start = 1) <- 1
  result <- start + 7 * (week - 1)
  #stopifnot(all(lubridate::isoyear(result) == year))
  #stopifnot(all(lubridate::isoweek(result) == week))
  result
}
