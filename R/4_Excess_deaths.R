rm(list=ls())

library(ggplot2)
library(data.table)

load('Data/Deaths_UK.RData')

### Calculate sex ratio

## Excess deaths from week 10 of 2020
w1 <- 10

Deaths.2020 <- Deaths.UK[year == 2020 & week >= w1]

Observed.2020  <- Deaths.2020[model == 'observed',-c(8,10:13)]
names(Observed.2020)[8] <- 'observed'

Baselines.2020 <- Deaths.2020[model != 'observed']

results.2020 <- merge(Baselines.2020,Observed.2020,all = T)
  
results.2020[, excess.dx := ifelse(observed >= lower.CI & observed <= upper.CI,0, observed - deaths)]

results.2020[, excess.dx.prop := (excess.dx/deaths)*100]

results.2020 <- results.2020[order(sex,model,age.n)]

results.2020[, cum.excess.deaths := cumsum(excess.dx), by = .(year,sex,model,age.n)]

results.2020[,age_group := factor(age.n,labels = c('0 to 14', '15 to 44', '45 to 64', '65 to 74','75 to 85','85+'))]


save(results.2020, file = 'Data/Excess_Deaths.RData')

write.csv(results.2020, file = 'Dashboard/Data/Excess_Deaths.csv')


### Some numbers for the paper

#Total deaths registered in weeks 10:26

sum(Observed.2020$observed)

#Total excess mortality in these weeks
sum(results.2020[model=='average',]$excess.dx)

#Excess mortality in these weeks by sex
results.2020[model=='gam', sum(excess.dx), by = .(sex)]

#Excess mortality by age groups
results.2020[model=='gam', sum(excess.dx), by = .(age_group)]

#Excess mortality by sex and age groups
results.2020[model=='gam', sum(excess.dx), by = .(age_group,sex)]

#Percentage above
results.2020[model=='gam', sum(excess.dx)/sum(deaths)*100]
results.2020[model=='gam', sum(excess.dx)/sum(deaths)*100, by = .(sex)]

