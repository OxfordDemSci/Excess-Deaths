library(ggplot2)
library(data.table)

load('Data/predicted_weekly_deaths_enwa.Rdata')
source('R/Figure_specifications.R')

### Calculate sex ratio

## On mortality risk

#unique(Deaths.UK$model)

Deaths.UK[,`:=`(
  model = factor(
    model,
    c('gam.nb', 'gam.poisson', 'glm.serfling', 'avg.mortality'),
    c('Negative-Binomial GAM', 'Poisson GAM', 'Poisson Serfling GLM',
      '2015-2019 average mortality')
  ),
  age.n =
    factor(
      age.n,
      c(0, 15, 45, 65, 75, 85),
      c('0 to 14','15 to 44','45 to 64','65 to 74','75 to 85','85+')
    )
)]

Sex.ratio <- Deaths.UK[model %in% 'Poisson GAM', list(sex.ratio.mx = observed.mx[sex == 'm']/observed.mx[sex == 'f']), by = list(model,age.n,year,week,date,time)]


mortality_sex_ratio <-
  ggplot(Sex.ratio[age.n != '0 to 14' & year %in% 2020]) +
  geom_line(aes(x = date, y = sex.ratio.mx,group= age.n),size = 1)+
  geom_hline(yintercept = 1)+
  facet_wrap('age.n', ncol = 1)+
  scale_y_continuous(breaks = c(1,1.2,1.5,1.8,2.1))+
  scale_x_date(date_breaks = '1 months', date_labels = '%b') +
  fig_spec$MyGGplotTheme(ar = 0.2, no_axes = TRUE, hgrid = TRUE) +
  guides(color = 'none', fill = 'none')+
  # labels
  labs(
    x = 'Date in 2020',
    y = 'Sex ratio of the death rate'
  )

fig_spec$ExportPDF(
  mortality_sex_ratio, filename = 'mortality_sex_ratio', path = 'Figures',
  width = fig_spec$width, height = fig_spec$width, scale = 1.21
)
fig_spec$ExportPNG(
  mortality_sex_ratio, filename = 'mortality_sex_ratio', path = 'Figures',
  width = fig_spec$width, height = fig_spec$width, scale = 1.21
)

