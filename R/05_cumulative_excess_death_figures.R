# Figures for cumulative excess deaths

# Init ------------------------------------------------------------

library(tidyverse)

fig <- list()

# Constants -------------------------------------------------------

cnst <- list()

cnst$age.labels <-
  c(
    `under 15` = 0,
    `15 to 44` = 15,
    `45 to 64` = 45,
    `65 to 74` = 65,
    `75 to 85` = 75,
    `85+` = 85
  )

source('R/Figure_specifications.R')

# Load data -------------------------------------------------------

load('Data/results.RData')

# Plot cumulative excess deaths by sex ----------------------------

fig$figure1 <-
  results$excess.deaths.week.sex %>%
  filter(model == 'gam.nb') %>%
  ggplot() +
  geom_ribbon(
    aes(
      x = date,
      ymin = qlo,
      ymax = qhi,
      fill = sex
    ),
    alpha = 0.2
  ) +
  geom_line(
    aes(x = date, y = avg, color = sex)
  ) +
  geom_point(
    aes(x = date, y = avg, color = sex),
    size = 1
  ) +
  geom_text(
    aes(
      x = date, y = avg,
      label = paste0(ifelse(sex == 'f', 'Female\n', 'Male\n'),
                     formatC(avg, format = 'd', big.mark = ',')),
      color = sex),
    hjust = 0, vjust = 1,
    nudge_x = 1, size = 3,
    data =
      results$excess.deaths.week.sex %>%
      filter(model == 'gam.nb') %>%
      group_by(sex) %>%
      filter(date == max(date))
  ) +
  scale_x_date(
    limits = as.Date(c('2020-03-01', '2020-12-01')),
    date_breaks = '1 month', date_labels = '%b'
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_color_manual(values = fig_spec$sex_colors) +
  scale_fill_manual(values = fig_spec$sex_colors) +
  labs(
    x = NULL,
    y = 'Cumulative excess deaths'
  ) +
  fig_spec$MyGGplotTheme(hgrid = TRUE, scaler = 1.3, show_legend = FALSE)

fig_spec$ExportPDF(
  fig$figure1, filename = 'Figure_1', path = 'Figures',
  width = fig_spec$width, height = 0.6*fig_spec$width
)
fig_spec$ExportPNG(
  fig$figure1, filename = 'Figure_1', path = 'Figures',
  width = fig_spec$width, height = 0.6*fig_spec$width
)

# Plot cumulative excess by sex and age ---------------------------

fig$figure2 <-
  results$excess.deaths.complete %>%
  filter(model == 'gam.nb') %>%
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
    size = 0.7
  ) +
  facet_wrap(~age.n) +
  scale_x_date(
    limits = as.Date(c('2020-03-01', '2020-12-01')),
    date_breaks = '1 month',
    date_labels = '%b'
  ) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_color_manual(values = fig_spec$sex_colors) +
  scale_fill_manual(values = fig_spec$sex_colors) +
  labs(
    x = NULL,
    y = 'Cumulative excess deaths'
  ) +
  geom_text(
    aes(
      x = date, y = cum.excess.deaths.avg,
      label =
        case_when(
          sex == 'f' & age.n == '85+' ~
            paste0('Female\n', formatC(cum.excess.deaths.avg,
                                       format = 'd', big.mark = ',')),
          sex == 'm' & age.n == '85+' ~
            paste0('Female\n', formatC(cum.excess.deaths.avg,
                                       format = 'd', big.mark = ',')),
          age.n %in% c('under 15', '15 to 44') ~ '',
          TRUE ~ formatC(cum.excess.deaths.avg, format = 'd', big.mark = ',')
        ),
      color = sex,
    ),
    size = 3,
    hjust = 0, vjust = 1,
    nudge_x = 3,
    data =
      results$excess.deaths.complete %>%
      filter(model == 'gam.nb' #&
      #         !(age.n %in% c(0, 15))
      ) %>%
      mutate(age.n = factor(age.n, cnst$age.labels, names(cnst$age.labels))) %>%
      group_by(sex, age.n) %>%
      filter(date == max(date))
  ) +
  fig_spec$MyGGplotTheme(
    hgrid = TRUE,
    scaler = 1.3,
    show_legend = FALSE
  ) +
  coord_cartesian(clip = 'off')

fig_spec$ExportPDF(
  fig$figure2, filename = 'Figure_2', path = 'Figures',
  width = fig_spec$width, height = 0.7*fig_spec$width, scale = 1.21
)
fig_spec$ExportPNG(
  fig$figure2, filename = 'Figure_2', path = 'Figures',
  width = fig_spec$width, height = 0.7*fig_spec$width, scale = 1.21
)
