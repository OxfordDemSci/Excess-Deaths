# Global Constants ------------------------------------------------

glob <- list(
  # origin of data series on weekly deaths
  origin_date = lubridate::as_date('2010-01-01'),
  # origin year of data series
  origin_year = 2010,
  # starting iso-week of epi-year
  week_epi_year_starts = 27,
  # starting year and weeks for excess deaths calculation
  jumpoff_year = 2020,
  jumpoff_week = 10,
  # number of weeks under observation in 2020
  observed_weeks_2020 = 47,
  # total number of weeks in iso-week-year 2020
  total_weeks_2020 = 53
)
