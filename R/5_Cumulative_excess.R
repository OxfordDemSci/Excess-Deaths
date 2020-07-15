# Init ------------------------------------------------------------

rm(list=ls())

library(tidyverse)
library(patchwork)

fig <- list()

# Constants -------------------------------------------------------

cnst <- list()

cnst$age.labels <-
  c(
    `15 to 44` = 15,
    `45 to 64` = 45,
    `65 to 74` = 65,
    `75 to 85` = 75,
    `85+` = 85
  )
cnst$color.sex <-
  c(`f` = '#72B2B4', `m` = '#B4A097')

source('R/Figure_specifications.R')

# Load data -------------------------------------------------------

load('Data/Excess_Deaths.RData')

# Plot cumulative excess deaths by sex ----------------------------

fig$figure1 <-
  results$excess.deaths.week.sex.age %>%
  filter(model == 'gam.poisson' & age.n != 0) %>%
  mutate(age.n = factor(age.n, cnst$age.labels, names(cnst$age.labels))) %>%
  group_by(date, sex) %>%
  summarise_at(
    vars(cum.excess.deaths.qlo, cum.excess.deaths.qhi, cum.excess.deaths.avg),
    sum
  ) %>%
  ggplot() +
  geom_ribbon(
    aes(
      x = date,
      ymin = cum.excess.deaths.qlo,
      ymax = cum.excess.deaths.qhi,
      fill = sex
    ),
    alpha = 0.2
  ) +
  geom_line(
    aes(x = date, y = cum.excess.deaths.avg, color = sex)
  ) +
  geom_point(
    aes(x = date, y = cum.excess.deaths.avg, color = sex),
    size = 1
  ) +
  annotate(
    'text',
    x = as.Date('2020-07-01'), y = 28e3,
    label = 'Male', color = cnst$color.sex['m']
  ) +
  annotate(
    'text',
    x = as.Date('2020-07-01'), y = 24e3,
    label = 'Female', color = cnst$color.sex['f']
  ) +
  scale_x_date(limits = as.Date(c('2020-03-01', '2020-07-07'))) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_color_manual(values = cnst$color.sex) +
  scale_fill_manual(values = cnst$color.sex) +
  labs(
    x = NULL,
    y = 'Cumulative excess deaths'
  ) +
  fig_spec$MyGGplotTheme(hgrid = TRUE, scaler = 1.3, show_legend = FALSE)

fig_spec$ExportPDF(
  fig$figure1, filename = 'Figure_1', path = 'Figures',
  width = fig_spec$width, height = 0.6*fig_spec$width
)

# Plot cumulative excess by sex and age ---------------------------

fig$figure2 <-
  results$excess.deaths.week.sex.age %>%
  filter(model == 'gam.poisson' & age.n != 0) %>%
  mutate(age.n = factor(age.n, cnst$age.labels, names(cnst$age.labels))) %>%
  ggplot() +
  geom_ribbon(
    aes(
      x = date,
      ymin = cum.excess.deaths.qlo,
      ymax = cum.excess.deaths.qhi,
      fill = sex
    ),
    alpha = 0.2
  ) +
  geom_line(
    aes(x = date, y = cum.excess.deaths.avg, color = sex)
  ) +
  geom_point(
    aes(x = date, y = cum.excess.deaths.avg, color = sex),
    size = 1
  ) +
  facet_wrap(~age.n) +
  scale_x_date(limits = as.Date(c('2020-03-01', '2020-07-07'))) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_color_manual(values = cnst$color.sex) +
  scale_fill_manual(values = cnst$color.sex) +
  labs(
    x = NULL,
    y = 'Cumulative excess deaths'
  ) +
  geom_text(
    aes(x = x, y = y, label = label),
    data =
      data.frame(
        age.n = '85+', x = as.Date('2020-07-01'), y = 13.5e3,
        label = 'Female'
      ),
    color = cnst$color.sex['f']
  ) +
  geom_text(
    aes(x = x, y = y, label = label),
    data =
      data.frame(
        age.n = '85+', x = as.Date('2020-07-01'), y = 10e3,
        label = 'Male'
      ),
    color = cnst$color.sex['m']
  ) +
  fig_spec$MyGGplotTheme(
    hgrid = TRUE,
    scaler = 1.3,
    show_legend = FALSE
  ) +
  coord_cartesian(clip = 'off')

fig_spec$ExportPDF(
  fig$figure2, filename = 'Figure_2', path = 'Figures',
  width = fig_spec$width, height = 0.8*fig_spec$width
)

