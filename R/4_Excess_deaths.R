rm(list=ls())

library(ggplot2)
library(data.table)

load('Data/Deaths_UK.RData')

### Calculate sex ratio

## Excess deaths from week 10 of 2020
w1 <- 10

Deaths.2020 <- Deaths.UK[year == 2020 & week >= w1]

unique(deaths.2020$model)

DT <- Deaths.2020[sex == 'f' & age.n == 85]

excess.deaths.fun <- function(DT = .SD){
  deaths.observed <- DT[model == 'observed']$deaths
  
  deaths.gam <- DT[model == 'gam']$deaths
  
  deaths.glm <- DT[model == 'glm']$deaths
  
  deaths.avg <- DT[model == 'average']$deaths
  
  excess.gam <- sum(deaths.observed) - sum(deaths.gam)
  excess.glm <- sum(deaths.observed)  - sum(deaths.glm)
  excess.avg <- sum(deaths.observed) - sum(deaths.avg)
  
  excess.dx <- c(excess.gam,excess.glm,excess.avg)
  
  excess.dx.prop <- c(excess.gam/sum(deaths.gam)*100,excess.glm/sum(deaths.glm)*100,excess.avg/sum(deaths.avg)*100)
  
  model.n <- c('gam','glm','average')
  
  r <- data.table(cbind(excess.dx,excess.dx.prop,model.n))
  
  r
  
}

results.2020 <- Deaths.2020[, excess.deaths.fun(DT = .SD), by = list(sex,age.n)]

results.2020 <- results.2020[order(sex,model.n,age.n)]

write.csv(results.2020, file = 'Dashboard/Data/Excess_Deaths.csv')



