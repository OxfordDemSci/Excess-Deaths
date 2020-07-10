rm(list=ls())

library(ggplot2)
library(data.table)

load('Data/Deaths_UK.RData')

### Calculate sex ratio

## Excess deaths from week 10 of 2020
w1 <- 10

results.2020 <- Deaths.UK[year %in% 2020 & week >= w1]
  
results.2020[, excess.dx :=  observed.deaths - expected.deaths]

results.2020[, cum.excess.deaths := cumsum(excess.dx), by = .(year,sex,model,age.n)]

results.2020[,age.n.fct := factor(age.n,labels = c('0 to 14', '15 to 44', '45 to 64', '65 to 74','75 to 85','85+'))]

#write.csv(results.2020, file = 'Dashboard/Data/Excess_Deaths.csv')


### Some numbers for the paper

#Total deaths registered in weeks 10:26

sum(results.2020[model %in% 'gam.final' & age.n != 0,]$observed.deaths)

#Total excess mortality in these weeks
sum(results.2020[model %in% 'gam.final'  & age.n != 0,]$excess.dx)

#Excess mortality in these weeks by sex
results.2020[model %in% 'gam.final' & age.n != 0, sum(excess.dx), by = .(sex)]

#Excess mortality by age groups
results.2020[model %in% 'gam.final' & age.n != 0, sum(excess.dx), by = .(age.n.fct)]

#Excess mortality by sex and age groups
results.2020[model %in% 'gam.final' & age.n != 0, sum(excess.dx), by = .(age.n.fct,sex)]

#Percentage above
results.2020[model %in% 'gam.final' & age.n != 0, sum(excess.dx)/sum(expected.deaths)*100]
results.2020[model %in% 'gam.final' & age.n != 0, sum(excess.dx)/sum(expected.deaths)*100, by = .(sex)]

