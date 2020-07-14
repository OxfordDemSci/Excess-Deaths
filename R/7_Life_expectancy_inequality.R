library(ggplot2)
library(data.table)

rm(list=ls())

source('R/6_Get_data for Life_expectancy_inequality.R')

### functions for life expectancy and CIs
source('R/LifeTableFUN.R')

## get estimates of life expectancy and lifespan inequality
Total <- Deaths.Population.EW.2001.2020[,list(exposure = sum(exposure), deaths = sum(deaths), sex = 'both'),
                                        by = .(year,age)]

Deaths.Population.EW.2001.2020 <- rbind(Deaths.Population.EW.2001.2020[,c('year','sex','age','exposure','deaths')],
                                        Total[,c('year','sex','age','exposure','deaths')])

Deaths.Population.EW.2001.2020[,mx:= deaths/exposure]

#nmx <- Deaths.Population.EW.2001.2020[year == 2005 & sex == 'females']$mx
#Nx <- Deaths.Population.EW.2001.2020[year == 2005 & sex == 'females']$exposure
#Dx <- Deaths.Population.EW.2001.2020[year == 2005 & sex == 'females']$deaths
#age <- unique(Deaths.Population.EW.2001.2020$age)

lifetable(x = age,Nx = Nx,Dx = Dx,sex = 'F')

CIex.sd(x = age,Nx = Nx,Dx = Dx,sex = 'F')

ex.fun <- function(x = age,Nx = Nx,Dx = Dx,sex = 'F',...){
  
  ex.stuff <- CIex.sd(x = x,Nx = Nx,Dx = Dx,sex =sex)
  
  results <- data.table(data.frame(ex =ex.stuff$ex,lower.ex = ex.stuff$CIex[1],upper.ex = ex.stuff$CIex[2],
                                   sd =ex.stuff$sd,lower.ex = ex.stuff$CIsd[1],upper.ex = ex.stuff$CIsd[2]))
  
  return(results)
}

EW.results <- Deaths.Population.EW.2001.2020[, ex.fun(x = age,Nx = exposure,Dx = deaths, sex = ifelse(sex == 'females','F','M')), 
                                             by = .(year,sex)]

EW.results <- EW.results[order(year,sex)]

### let's compare results with standard life expectancy function

save(EW.results,life.tables.EW.1982.2018,file = 'Data/LifeExpectancyInequality.RData')

## percentage of poulation 85+ in females and males

100*Deaths.Population.EW.2001.2020[year == 2020 & age >= 85, sum(mid.population), by =sex]$V1/
  Deaths.Population.EW.2001.2020[year == 2020, sum(mid.population), by =sex]$V1


