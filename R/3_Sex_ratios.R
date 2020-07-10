rm(list=ls())

library(ggplot2)
library(data.table)

load('Data/Deaths_UK.RData')

### Calculate sex ratio

## On mortality risk

Sex.ratio <- Deaths.UK[model %in% 'gam.final', list(sex.ratio.mx = observed.mx[sex == 'm']/observed.mx[sex == 'f']), by = list(model,age.n,year,week,date,time)]


ggplot(Sex.ratio[age.n >0 & year %in% 2020]) +
  geom_line(aes(x = date, y = sex.ratio.mx),size = 1)+
  geom_hline(yintercept = 1)+
  facet_grid(rows = 'age.n')+
    theme_minimal()+
    theme(panel.grid.minor = element_blank())+
    guides(color = 'none', fill = 'none')+
    # labels
    labs(
      x = 'Date in 2020',
      y = 'Sex ratio of the risk of death'
    )


