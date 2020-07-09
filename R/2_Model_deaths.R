rm(list=ls())

library(mgcv)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(data.table)

#run 1_Weekly-Update.R to update from excel file from ONS when needed

load('Data/Weekley_Deaths_UK_2010-20.Rdata')

# set period to include in training data

# lowest year in the data
initial.year <- 2010

# period to be forecast
last.year    <- 2020

# Create variable for training period
Data.dt[,training := ifelse(year < last.year, TRUE, FALSE)]

Data.dt[year == last.year & week %in% 0:9,]$training <- TRUE

unique(Data.dt$age.n)

#####################################

#start model list
models <- list()

#Subset for modeling data
model.data <- Data.dt[training == TRUE]

#### Gam model
models$gam.lcds <- gam( deaths ~ 1 + sex  +
                        s(week, bs = 'cp', by = sex,fx = T) + # cubic penalized splines for week
                        s(time, bs = 'ps', by = sex) + # penalized splines for long term 
                        s(age.n,bs = 'ps', by = sex) + # penalized splines for age effect
                        offset(log(exposures)),
                      data = model.data,
                      family = poisson(link = 'log'), method = 'ML')

#### Flumomo model with poisson and offset

models$Flumomo.exp <-
  glm(
    deaths ~
      time*factor(age.n)*sex + # log linear long-term-trend
      sin(2*pi*week_ify/(365.25/7))*factor(age.n)*sex + # seasonality: full year period
      cos(2*pi*week_ify/(365.25/7))*factor(age.n)*sex +
      sin(2*pi*week_ify/(365.25/2/7))*factor(age.n)*sex + # seasonality: half year period
      cos(2*pi*week_ify/(365.25/2/7))*factor(age.n)*sex+
      offset(log(exposures)),
    family = poisson(link = 'log'),
    data = model.data
  )

## get fitted values and credible intervals

#get names of the models
name.mod <- names(models)

#get deaths
for (i in name.mod) {
  
  var.name   <- paste0('dx.',i)
  #get predictions
  Data.dt[, eval(var.name) := exp(predict(models[[i]],newdata = .SD))]
  
}

#### Averaging over the last 5 years per # of week on risk

avg.mx.fun <- function(Data.dt = .SD, years = 2015:2019){

r <- NULL
  
if( max(unique(Data.dt$year)) == 2020) {
  
DT     <- Data.dt[year %in% years]

DT.avg <- mean(DT$mx)

r <- c(Data.dt[year < 2020]$mx,DT.avg)}

if (max(unique(Data.dt$year)) < 2020) {r <- Data.dt$mx}

dx <- r*Data.dt$exposures   
  
}

Data.dt[,deaths.avg.mx := avg.mx.fun(.SD, 2015:2019), by = list(sex,age.n,week)]



### create a single data.table in the long form for the dashboard

Data.dt[year < 2020]$deaths.avg.mx <- NA

#deal with deaths
Deaths.UK        <- Data.dt[,c('sex','age.n','year','week','date','time','exposures','deaths','dx.gam.lcds','dx.Flumomo.exp','deaths.avg.mx')]
names(Deaths.UK) <- c('sex','age.n','year','week','date','time','exposures','observed','gam','glm','average')
Deaths.UK        <- melt.data.table(Deaths.UK,id.vars = c('sex','age.n','year','week','date','time','exposures'),variable.name = 'model',value.name = 'deaths')

### calculate mx
Deaths.UK[, mx := deaths/exposures]
Deaths.UK[, log.mx := log10(mx)]

### calculate Predictive intervals
#get predictive intervals
Deaths.UK[, lower.CI := qpois(0.025, deaths)]
Deaths.UK[, upper.CI := qpois(0.975, deaths)]

Deaths.UK[model == 'observed', ]$lower.CI <- NA
Deaths.UK[model == 'observed', ]$upper.CI <- NA

Deaths.UK[,week:= week + 1]

### save file for dashboard
write.csv(Deaths.UK,file = 'Dashboard/Data/Deaths_UK.csv')

save(Deaths.UK,models,file = 'Data/Deaths_UK.RData')

gdata::keep(Deaths.UK, sure =T)
############# run a diagnostic plot


y1       <- 2019

m <- ggplot() +
  geom_ribbon(data = Deaths.UK[sex == 'm' & year >= y1 & model %in% c('gam','glm','average')], 
              aes(x = date, ymin = lower.CI, ymax = upper.CI, fill = model),alpha = 1/10) +
  geom_point(data = Deaths.UK[sex == 'm' & year >= y1 & model == 'observed'], aes(x = date, y = deaths), size = 0.8, col = 'black',alpha = 1/7) +
  geom_line(data = Deaths.UK[sex == 'm' & year >= y1 & model != 'observed'], aes(x = date, y = deaths, col = model))+
  geom_line(data = Deaths.UK[sex == 'm' & year == 2020 & model == 'observed'], aes(x = date, y = deaths, col = '2020'))+
  facet_wrap(~ age.n, scales = 'free_y') +
  theme_minimal()+
  theme(panel.grid.minor = element_blank())+
  scale_y_continuous(labels = scales::label_number(1)) +
  labs(
    title = 'Observed vs. expected England and Wales death counts by age and year with GAM, GLM and average',
    x = 'Week of year',
    y = 'Deaths per week',
    caption = 'Source data: ONS',
    subtitle = 'Male population. Shaded areas indicate 95% Poisson prediction intervals. 2020 are extrapolated predictions.'
  )
m

f <- ggplot() +
  geom_ribbon(data = Deaths.UK[sex == 'f' & year >= y1 & model %in% c('gam','glm',' average')], 
              aes(x = date, ymin = lower.CI, ymax = upper.CI, fill = model),alpha = 1/10) +
  geom_point(data = Deaths.UK[sex == 'f' & year >= y1 & model == 'observed'], aes(x = date, y = deaths), size = 0.8, col = 'black',alpha = 1/7) +
  geom_line(data = Deaths.UK[sex == 'f' & year >= y1 & model != 'observed'], aes(x = date, y = deaths, col = model))+
  geom_line(data = Deaths.UK[sex == 'f' & year == 2020 & model == 'observed'], aes(x = date, y = deaths, col = '2020'))+
  facet_wrap(~ age.n, scales = 'free_y') +
  theme_minimal()+
  theme(panel.grid.minor = element_blank())+
  scale_y_continuous(labels = scales::label_number(1)) +
  labs(
    title = 'Observed vs. expected England and Wales death counts by age and year with GAM, GLM and average',
    x = 'Week of year',
    y = 'Deaths per week',
    caption = 'Source data: ONS',
    subtitle = 'Female population. Shaded areas indicate 95% Poisson prediction intervals. 2020 are extrapolated predictions.'
  )
f

pdf(file = 'Figures/Model_ouptput.pdf',width = 10,height = 7,useDingbats = F)
m
f
dev.off()


###############################################################################


y1       <- 2010

m <- ggplot() +
  geom_ribbon(data = Deaths.UK[sex == 'm' & year >= y1 & model %in% c('gam','glm',' average')], 
              aes(x = date, ymin = lower.CI, ymax = upper.CI, fill = model),alpha = 1/10) +
  geom_point(data = Deaths.UK[sex == 'm' & year >= y1 & model == 'observed'], aes(x = date, y = deaths), size = 0.8, col = 'black',alpha = 1/7) +
  geom_line(data = Deaths.UK[sex == 'm' & year >= y1 & model != 'observed'], aes(x = date, y = deaths, col = model))+
  geom_line(data = Deaths.UK[sex == 'm' & year == 2020 & model == 'observed'], aes(x = date, y = deaths, col = '2020'))+
  facet_wrap(~ age.n, scales = 'free_y') +
  theme_minimal()+
  theme(panel.grid.minor = element_blank())+
  scale_y_continuous(labels = scales::label_number(1)) +
  labs(
    title = 'Observed vs. expected England and Wales death counts by age and year with GAM, GLM and average',
    x = 'Week of year',
    y = 'Deaths per week',
    caption = 'Source data: ONS',
    subtitle = 'Male population. Shaded areas indicate 95% Poisson prediction intervals.'
  )
m

f <- ggplot() +
  geom_ribbon(data = Deaths.UK[sex == 'f' & year >= y1 & model %in% c('gam','glm',' average')], 
              aes(x = date, ymin = lower.CI, ymax = upper.CI, fill = model),alpha = 1/10) +
  geom_point(data = Deaths.UK[sex == 'f' & year >= y1 & model == 'observed'], aes(x = date, y = deaths), size = 0.8, col = 'black',alpha = 1/7) +
  geom_line(data = Deaths.UK[sex == 'f' & year >= y1 & model != 'observed'], aes(x = date, y = deaths, col = model))+
  geom_line(data = Deaths.UK[sex == 'f' & year == 2020 & model == 'observed'], aes(x = date, y = deaths, col = '2020'))+
  facet_wrap(~ age.n, scales = 'free_y') +
  theme_minimal()+
  theme(panel.grid.minor = element_blank())+
  scale_y_continuous(labels = scales::label_number(1)) +
  labs(
    title = 'Observed vs. expected England and Wales death counts by age and year with GAM, GLM and average',
    x = 'Week of year',
    y = 'Deaths per week',
    caption = 'Source data: ONS',
    subtitle = 'Female population. Shaded areas indicate 95% Poisson prediction intervals.'
  )
f

pdf(file = 'Figures/Model_ouptput2.pdf',width = 10,height = 7,useDingbats = F)
m
f
dev.off()


