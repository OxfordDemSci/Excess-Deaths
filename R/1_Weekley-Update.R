library(data.table)
library(reshape2)
library(readxl)
library(lubridate)
rm(list=ls())

load('Data/Input_UK_Data.RData')

#load data for 2020 (updating periodically)

#males
males.deaths.update <- data.table(read_excel('Data/Update data/publishedweek202020.xlsx',sheet="Weekly figures 2020",range = 'B44:BC63',
                                             col_names = F))
males.deaths.update[,sex:='m']
females.deaths.update <- data.table(read_excel('Data/Update data/publishedweek202020.xlsx',sheet="Weekly figures 2020",range = 'B66:BC85',
                                               col_names = F))
females.deaths.update[,sex:='f']

update.deaths <- rbind(males.deaths.update,females.deaths.update)
update.deaths <- update.deaths[, c(1,55,2:54)]
names(update.deaths) <- c('agegroup','sex',1:53)
update.deaths[,age:= c(0,1,seq(5,90,5)), by = list(sex)]

update.deaths <- melt(update.deaths, id.vars = c('age','agegroup','sex'),variable.name = 'week',value.name = 'deaths',variable.factor = 'F')
update.deaths[, week:=as.numeric(week)]

#need to regroup to original age groups
age.n <- unique(Deaths.DT$age)
update.deaths[,age.n:= cut(age,c(age.n,Inf),include.lowest = T,right = F,labels = age.n)]
update.deaths$year <- 2020

update.deaths <- update.deaths[,list(deaths = sum(deaths)), by = list(year,age.n,week,sex)]

agegroup.lab <- Deaths.DT[1:7,]$agegroup

update.deaths[, agegroup:=agegroup.lab, by = list(year,week,sex)]

update.deaths <- update.deaths[,c('year' ,'age.n','agegroup','week','deaths','sex')]

names(update.deaths)[2] <- 'age'

Deaths.DT <- rbind(Deaths.DT,update.deaths)

Deaths.DT$age <- as.numeric(as.character(Deaths.DT$age))

weeks.inter <- c(52,52,52,52,52,53,52,52,52,52,52)

# #example 
# vec1        <- pop.age.groups[sex == 'm' & age.n == 15 & year]
# plot(vec1$year, vec1$population, main = "approx(.) and approxfun(.)")
# #points(approx(vec1$year, vec1$population,n = sum(weeks.inter)), col = 2, pch = "*")
# lines(spline(vec1$year, vec1$population, n = sum(weeks.inter)), col = 2,)

exposures.splines <- function(year =vec1$year,population=vec1$population, weeks.inter = weeks.inter){
  interpolation <- spline(year, population,n = sum(weeks.inter))
  #plot(1:(sum(weeks.inter)+1),interpolation$y)
  #interpolation <- spline(year, population,n = sum(weeks.inter)+1)
  #inter.diff <- abs(diff(interpolation$y))
  
  #check
  #sum(inter.diff)
  #population[11]-population[1]
  
  #vec.week <- unlist(lapply(weeks.inter, function(x){1:x}))
  
  vec.week <- unlist(lapply(c(52,52,52,52,53,52,52,52,52,52,26), function(x){1:x}))
  vec.week <- c(27:52,vec.week)
  #rearrange week
  
  year.vec <- c(rep(year,c(26,52,52,52,52,53,52,52,52,52,52,26)))
  
  results <- data.table(cbind(year = year.vec, week = vec.week, exposures = interpolation$y))
  
  return(results)
}

exposures <- pop.age.groups[,exposures.splines(year = year,population = population ,weeks.inter = weeks.inter ), by = list(sex,age.n)]
exposures <- exposures[order(sex,year,week,age.n)]

#correct exposures, these are pop estiamtes at the middle of year, they are lagged 26 weeks.

exposures <- exposures[year %in% 2010:2020]

ages <- unique(exposures$age.n)

deaths.new <-  Deaths.DT[,age.n:= cut(age,c(ages,Inf),include.lowest = T,right = F,labels = as.character(ages))]
deaths.new <-  deaths.new[, list(deaths = sum(deaths,na.rm = F)), by = list(sex,age.n,year,week)]
deaths.new$age.n <- as.numeric(as.character(deaths.new$age.n))
deaths.new <- deaths.new[order(sex,year,week,age.n)]

## take out NA
deaths.new <- deaths.new[!(is.na(deaths))]

Data.dt <- merge(exposures,deaths.new,by = c('sex','age.n','year','week'))

Data.dt[year == 2020]

# Annualize exposure
Data.dt[,exposures := exposures*7/365.25]

origin_date <- lubridate::as_date('2010-01-01')

Data.dt[,date := as_date(parse_date_time(paste(year, formatC(week, flag = '0', width = 2),'1', sep= '-'),'Y!-W-w'))]

# Time variable from beginning
Data.dt[,time := 1:(length(deaths)), by = list(sex,age.n)]

#sex to factor
Data.dt[,sex := as.factor(sex)]

#observed mx
Data.dt[,mx := deaths/exposures]

Data.dt[,week_ify := (time+26)%%52]

#add month
months <- c(unlist(lapply(0:12, function(x){rep(x,4)})))

Data.dt[,month := months[1:length(deaths)] , by = list(sex,age.n,year)]

save(Data.dt,file = 'Data/Weekley_Deaths_UK_2010-20.Rdata')


