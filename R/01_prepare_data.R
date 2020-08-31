# Init ------------------------------------------------------------

library(data.table)
library(reshape2)
library(readxl)
library(lubridate)

# Constants -------------------------------------------------------

cnst <- list(
  origin_date = as_date('2010-01-01'),
  week_epi_year_starts = 27
)

# Functions -------------------------------------------------------

#' Convert Week of Year to Date
#'
#' @param year Year integer.
#' @param week Week of year integer (1 to 53).
#' @param weekday Weekday integer (1, Monday to 7, Sunday).
#' @param offset Integer offset added to `week` before date calculation.
#'
#' @return A date object.
#'
#' @source https://en.wikipedia.org/wiki/ISO_8601
#'
#' @author Jonas Schöley
#'
#' @examples
#' # the first Week of 2020 actually starts Monday, December 30th 2019
#' ISOWeekDate2Date(2020, 1, 1)
ISOWeekDate2Date <- function(year, week, weekday = 1, offset = 0) {
  require(ISOweek)
  isoweek_string <-
    paste0(
      year, "-W",
      formatC(
        week + offset,
        flag = "0",
        format = "d",
        digits = 1
      ),
      "-", weekday
    )
  ISOweek2date(isoweek_string)
}

#' Calculate Weeks Since Some Origin Date
#'
#' @param date Date string.
#' @param origin_date Date string.
#' @param week_format Either 'integer' for completed weeks or
#' 'fractional' for completed fractional weeks.
#'
#' @return Time difference in weeks.
#'
#' @author Jonas Schöley
#'
#' @examples
#' # My age in completed weeks
#' WeeksSinceOrigin(Sys.Date(), '1987-07-03')
WeeksSinceOrigin <-
  function(date, origin_date, week_format = "integer") {
    require(ISOweek)
    fractional_weeks_since_origin <-
      as.double(difftime(
        as.Date(date),
        as.Date(origin_date),
        units = "weeks"
      ))
    switch(
      week_format,
      fractional = fractional_weeks_since_origin,
      integer = as.integer(fractional_weeks_since_origin)
    )
  }

EpiYearSequence <- function(from, to) {
  years <- from:to
  paste0(head(years, -1), "/", years[-1])
}

# Load raw data ---------------------------------------------------

# load England-Wales historical data on weekly death counts and
# annual population estimates
load('Data/Input_UK_Data.RData')

# load England-Wales weekly death counts for 2020 (updating periodically)

# download most recent file
# https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales
males.deaths.update <-
  data.table(read_excel(
    'Data/Update data/publishedweek332020.xlsx',
    sheet="Weekly figures 2020",
    range = 'B44:BC63',
    col_names = F
  ))
males.deaths.update[,sex:='m']
females.deaths.update <-
  data.table(read_excel(
    'Data/Update data/publishedweek332020.xlsx',
    sheet="Weekly figures 2020",
    range = 'B66:BC85',
    col_names = F
  ))
females.deaths.update[,sex:='f']

# Convert to long format ------------------------------------------

update.deaths <- rbind(males.deaths.update,females.deaths.update)
update.deaths <- update.deaths[, c(1,55,2:54)]
names(update.deaths) <- c('agegroup','sex',1:53)
update.deaths[,age:= c(0,1,seq(5,90,5)), by = list(sex)]

update.deaths <- data.table(melt(update.deaths, id.vars = c('age','agegroup','sex'),variable.name = 'week',value.name = 'deaths',variable.factor = 'F'))
update.deaths[, week:=as.numeric(week)]

# Merge pre 2020 data ---------------------------------------------

# regroup 2020 age groups to pre 2020 ages
age.n <- unique(Deaths.DT$age)
update.deaths[,age.n:= cut(age,c(age.n,Inf),
                           include.lowest = T,
                           right = F,
                           labels = age.n)]
update.deaths$year <- 2020
update.deaths <- update.deaths[,list(
  deaths = sum(deaths)),
  by = list(year,age.n,week,sex)]
agegroup.lab <- Deaths.DT[1:7,]$agegroup
update.deaths[, agegroup:=agegroup.lab, by = list(year,week,sex)]

# merge pre 2020 data into 2020 data
update.deaths <-
  update.deaths[,c('year' ,'age.n','agegroup','week','deaths','sex')]
names(update.deaths)[2] <- 'age'
Deaths.DT <- rbind(Deaths.DT,update.deaths)
Deaths.DT$age <- as.numeric(as.character(Deaths.DT$age))

# Calculate and add weekly exposure -------------------------------

# number of weeks by year
weeks.inter <- Deaths.DT[
  !is.na(deaths) & year %in% 2009:2020,
  .(max.week = max(week)), by = year]

# #example
# vec1        <- pop.age.groups[sex == 'm' & age.n == 15 & year]
# plot(vec1$year, vec1$population, main = "approx(.) and approxfun(.)")
# #points(approx(vec1$year, vec1$population,n = sum(weeks.inter)), col = 2, pch = "*")
# lines(spline(vec1$year, vec1$population, n = sum(weeks.inter)), col = 2,)

exposures.splines <- function(year, population, max.week.year){
  #plot(1:(sum(weeks.inter)+1),interpolation$y)
  #interpolation <- spline(year, population,n = sum(weeks.inter)+1)
  #inter.diff <- abs(diff(interpolation$y))
  #check
  #sum(inter.diff)
  #population[11]-population[1]

  interpolation <- spline(year, population, n = sum(max.week.year))

  vec.week <- unlist(lapply(max.week.year, function(x) { 1:x }))
  vec.year <- rep(year, times = max.week.year)

  results <- data.table(cbind(year = vec.year, week = vec.week,
                              exposures = interpolation$y))

  return(results)
}

exposures <- pop.age.groups[year != 2009,][,exposures.splines(
  year = year, population = population,
  max.week.year = weeks.inter[['max.week']]),
  by = list(sex, age.n)]
exposures <- exposures[order(sex,year,week,age.n)]

ages <- unique(exposures$age.n)

deaths.new <-
  Deaths.DT[,age.n := cut(age,c(ages,Inf),
                          include.lowest = T,
                          right = F,
                          labels = as.character(ages))]
deaths.new <-
  deaths.new[, list(deaths = sum(deaths,na.rm = F)),
             by = list(sex,age.n,year,week)]
deaths.new$age.n <- as.numeric(as.character(deaths.new$age.n))
deaths.new <- deaths.new[order(sex,year,week,age.n)]

## take out NA
deaths.new <- deaths.new[!(is.na(deaths))]

Data.dt <- merge(exposures,deaths.new,by = c('sex','age.n','year','week'))

Data.dt[year == 2020]

# Annualized exposure
Data.dt[,exposures := exposures*7/365.25]

# Add additional variables ----------------------------------------

# assuming weeks refer to iso weeks
Data.dt[, iso.week := week]
# weeks starting at 0 for modeling
Data.dt[, week := iso.week-1]
# date
Data.dt[, date := ISOWeekDate2Date(year, iso.week, 1)]
# month
Data.dt[, month := month(date)]
# weeks since origin
Data.dt[,time := WeeksSinceOrigin(date, cnst$origin_date)]

# date epi year starts
Data.dt[, start.of.epi.year :=
          ISOWeekDate2Date(
            ifelse(iso.week<=cnst$week_epi_year_starts, year-1, year),
            cnst$week_epi_year_starts, 1
          )]

# weeks into flu year
Data.dt[,week_ify := WeeksSinceOrigin(date, start.of.epi.year)]

# add factor variables
Data.dt[, `:=`(
  sex = as.factor(sex),
  age.n.fct = as.factor(age.n),
  week.fct = as.factor(week),
  week_ify.fct = as.factor(week_ify),
  sex_age_fct = interaction(sex, age.n)
)]

# add indicators for special days
Data.dt[, `:=`(
  week21 = week == 21,
  last_week = week == 51,
  first_week = week == 0
)]

#observed mx
Data.dt[,mx := deaths/exposures]

save(Data.dt,file = 'Data/weekly_deaths_enwa.Rdata')
