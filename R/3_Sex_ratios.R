rm(list=ls())

library(ggplot2)
library(data.table)

load('Data/Deaths_UK.RData')

### Calculate sex ratio

## On mortality risk

Sex.ratio <- Deaths.UK[, list(sex.ratio.deaths = deaths[sex=='m']/deaths[sex=='f'],
                              sex.ratio.mx = mx[sex == 'm']/mx[sex == 'f']), by = list(model,age.n,year,week,date,time)]

Sex.ratio[, log.sr.deaths := log(sex.ratio.deaths)]
Sex.ratio[, log.sr.mx := log(sex.ratio.mx)]
Sex.ratio[, training := ifelse(year == 2020, FALSE, TRUE)]


write.csv(Sex.ratio, file = 'Dashboard/Data/Sex_ratios.csv')

############# run a diagnostic plot
y1 <- 2016
y2 <- 2020

m <- ggplot() +
  geom_line(data = Sex.ratio[year >= y1 & year < y2 & model == 'observed'], aes(x = date, y = log.sr.deaths), col = 'grey') +
  geom_line(data = Sex.ratio[ year >= y2 & model == 'observed'], aes(x = date, y = log.sr.deaths),col = 'black') +
  geom_line(data = Sex.ratio[ year >= y1 & year < y2 & model != 'observed'], aes(x = date, y = log.sr.deaths, col = model))+
  geom_hline(yintercept = 0)+
  facet_wrap(~ age.n) +
  theme_minimal()+
  theme(panel.grid.minor = element_blank())+
  labs(
    title = 'Sex ratio (log scale) of deaths (m/f) in England and Wales by age and year with trend of GAM and GLM',
    y = 'log10(sex ratio)',
    x = 'Deaths per week',
    caption = 'Source data: ONS')
  
m

f <-   ggplot() +
  geom_line(data = Sex.ratio[year >= y1 & year < y2 & model == 'observed'], aes(x = date, y = log.sr.mx), col = 'grey') +
  geom_line(data = Sex.ratio[ year >= y2 & model == 'observed'], aes(x = date, y = log.sr.mx),col = 'black') +
  geom_line(data = Sex.ratio[ year >= y1 & year < y2 & model != 'observed'], aes(x = date, y = log.sr.mx, col = model))+
  geom_hline(yintercept = 0)+
  facet_wrap(~ age.n) +
  theme_minimal()+
  theme(panel.grid.minor = element_blank())+
  labs(
    title = 'Sex ratio (log scale) of mx (m/f) in England and Wales by age and year with trend of GAM and GLM',
    y = 'log10(sex ratio)',
    x = 'Deaths per week',
    caption = 'Source data: ONS')

f


pdf(file = 'Figures/Sex_ratio_ouptput.pdf',width = 10,height = 7,useDingbats = F)
m
f
dev.off()

