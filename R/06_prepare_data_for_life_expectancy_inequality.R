# Prepare data for life-expectancy inequality calculations

# Init ------------------------------------------------------------

library(data.table)
library(reshape2)
library(readxl)

cnst <- list(
  # lifetables for England and Wales from
  # https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/nationallifetablesenglandandwalesreferencetables
  # file nationallifetables3yearenglandandwales
  path_to_lifetables = 'Data/nationallifetables3yearenglandandwales.xls',
  # population estimates from 2020 for England and Wales from
  # https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationprojections/datasets/tablea23principalprojectionenglandandwalespopulationinagegroups
  # file ewpppsumpop18
  path_to_projections = 'Data/ewpppsumpop18.xls',
  # population estimates England and Wales 2001-2019 from
  # https://www.ons.gov.uk/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationestimatesforukenglandandwalesscotlandandnorthernireland
  # file population_2001_2019_EW_singleyear
  path_to_population = 'Data/MYEB1_detailed_population_estimates_series_UK_(2019).csv',
  # death counts by single year of age, England and Wales, 1963-2018 from
  # https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/deathsregisteredinenglandandwalesseriesdrreferencetables
  # file finalreftables2019
  path_population_by_age = 'Data/finalreftables2019.xlsx',
  # weekly death counts England and Wales from
  # https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/datasets/weeklyprovisionalfiguresondeathsregisteredinenglandandwales
  path_weekly_deaths = 'Data/Update data/publishedweek472020.xlsx',
  # number of weeks under observation in 2020
  weeks_2020 = 47
)

# Prepare life-tables ---------------------------------------------

sheet.list.names <- c(paste0(2016:1980,'-', 2018:1982))
life.tables.EW.1982.2018 <-
  data.table(do.call(rbind, lapply(sheet.list.names, function(x){
    males         <- data.table(read_excel(cnst$path_to_lifetables,
                                           sheet=x,range = 'A7:F108',
                                           col_names = T))
    males$sex     <- 'males'
    females       <- data.table(read_excel(cnst$path_to_lifetables,
                                           sheet=x,range = 'H7:L108',
                                           col_names = T))
    females$sex   <- 'females'
    females$x     <- males$x
    LT            <- rbind(males,females[,c('x','mx','qx','lx','dx','ex','sex')])
    LT$period     <- x
    LT$upper.year <- as.numeric(substr(x,1,4)) + 2
    return(LT)
  })))

# Prepare population estimates ------------------------------------

# population 2020 from projections
sheet.list.names <- c('MALES','FEMALES')
population.projections.2020  <-
  data.table(do.call(rbind, lapply(sheet.list.names, function(x){
    population    <- data.table(read_excel(cnst$path_to_projections,
                                           sheet=x,range = 'D9:E29',
                                           col_names = F))

    names(population)  <- as.character(2020:2021)
    population$sex     <- tolower(x)
    population$age     <- seq(0,100,5)
    population         <- data.table(melt(population,id.vars = c('sex','age'),
                                          variable.name = 'year',
                                          value.name = 'population'))
    population[,year:= as.numeric(as.character(year))]
    population[,population:= population * 1000]
    return(population)
  })))

# population England and Wales 2001-2019
population.2001.2019 <-
  data.table(read.csv(cnst$path_to_population, header = T,sep = ','))
names(population.2001.2019)[6:24] <- 2001:2019
population.2001.2019 <- melt.data.table(population.2001.2019[,3:24],
                                        id.vars = c('country','sex','age'),
                                        variable.name = 'year',
                                        value.name = 'population')
population.2001.2019 <- population.2001.2019[country %in% c('E','W'),
                                             list(population = sum(population)),
                                             by = list(year,sex,age)]
population.2001.2019[,sex:= ifelse(sex == 1, 'males','females')]

# merge past population counts with population projections
Population.EW.2001.2021 <-
  rbind(population.2001.2019,
        population.projections.2020[,c('year','sex','age','population')])
Population.EW.2001.2021[,age.n:= cut(age,c(0,1,seq(5,90,5),Inf),
                                     include.lowest = T,right = F,
                                     labels = c(0,1,seq(5,90,5)))]
Population.EW.2001.2021 <-
  Population.EW.2001.2021[,list(mid.population = sum(population)),
                          by = list(year,sex,age.n)]
names(Population.EW.2001.2021)[3] <- 'age'
Population.EW.2001.2021[,age:= as.numeric(as.character(age))]
Population.EW.2001.2021[,year:= as.numeric(as.character(year))]
Population.EW.2001.2021[,exposure := ifelse(year != 2020,
                                            mid.population,
                                            mid.population*(cnst$weeks_2020/52))]

# Prepare death counts --------------------------------------------

# death counts by single age in 1963-2019
sheet.list.names <- c('Table 4','Table 5')
death.counts.1963.2019  <-
  data.table(do.call(rbind, lapply(sheet.list.names, function(x){
    deaths    <- data.table(read_excel(cnst$path_population_by_age,
                                       sheet=x,range = 'B9:BF115',
                                       col_names = T))
    deaths$sex     <- ifelse(x == 'Table 4','males','females')
    deaths$age     <- 0:105
    deaths         <- data.table(melt(deaths,id.vars = c('sex','age'),
                                      variable.name = 'year',value.name = 'deaths'))
    deaths[,year:= as.numeric(as.character(year))]
    return(deaths)
  })))

# aggregate with same age groups of 2020
death.counts.1963.2019[,age.n:= cut(age,c(0,1,seq(5,90,5),Inf),
                                    include.lowest = T,right = F,
                                    labels = c(0,1,seq(5,90,5)))]
death.counts.1963.2019 <- death.counts.1963.2019[,list(deaths = sum(deaths)),
                                                 by = list(year,sex,age.n)]
names(death.counts.1963.2019)[3] <- 'age'
death.counts.1963.2019[,age:= as.numeric(as.character(age))]

# Prepare weekly death counts -------------------------------------

# now get deaths for 2020
males.deaths.update <- data.table(read_excel(cnst$path_weekly_deaths,
                                             sheet="Weekly figures 2020",
                                             range = 'B44:BC63',
                                             col_names = F))
males.deaths.update[,sex:='males']
females.deaths.update <- data.table(read_excel(cnst$path_weekly_deaths,
                                               sheet="Weekly figures 2020",
                                               range = 'B66:BC85',
                                               col_names = F))
females.deaths.update[,sex:='females']
death.counts.2020 <- rbind(males.deaths.update,females.deaths.update)
death.counts.2020 <- death.counts.2020[, c(1,55,2:54)]
names(death.counts.2020) <- c('agegroup','sex',1:53)
death.counts.2020[,age:= c(0,1,seq(5,90,5)), by = list(sex)]
death.counts.2020 <- data.table(melt(death.counts.2020,
                                     id.vars = c('age','agegroup','sex'),
                                     variable.name = 'week',
                                     value.name = 'deaths',
                                     variable.factor = 'F'))
# aggregate over weeks
death.counts.2020 <- death.counts.2020[,list(deaths = sum(deaths,na.rm = T),
                                             year = 2020), by = .(age,sex)]
# rbind with other years
Deaths.EW.1963.2020 <- rbind(death.counts.1963.2019,
                             death.counts.2020[,c('year','sex','age','deaths')])

# Checks ----------------------------------------------------------

Deaths.EW.1963.2020[,sum(deaths), by = .(year)]

Population.EW.2001.2021[,sum(exposure), by = .(year)]

Deaths.Population.EW.2001.2020 <-
  merge(Population.EW.2001.2021[year < 2021],Deaths.EW.1963.2020,all.x = T)

gdata:: keep(
  Deaths.EW.1963.2020,
  Population.EW.2001.2021,
  life.tables.EW.1982.2018,
  Deaths.Population.EW.2001.2020,
  sure = T
)
