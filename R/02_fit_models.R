# Fit expected deaths models to weekly death counts

# Init ------------------------------------------------------------

set.seed(1987)

library(data.table)
library(mgcv)

models <- list()
dat <- list()
fig <- list()

source('R/Figure_specifications.R')
source('R/00-global_constants.R')

# Constants -------------------------------------------------------

cnst <- list(
  # period for average mortality rate model
  avg.mortality.period = 2015:2019
)

# Data preparation ------------------------------------------------

load('Data/weekly_deaths_enwa.Rdata')

Data.dt[,
        # create variable for training period
        training := ifelse(
          year < glob$jumpoff_year |
            (year == glob$jumpoff_year & iso.week < glob$jumpoff_week),
          TRUE, FALSE
        )]

# subset training data
dat$training.data <- Data.dt[training == TRUE]

# GAM negative binomial -------------------------------------------

# Generalized additive model with smooth effects for long term trend,
# age, and seasonality, and interaction between age and seasonality.
# Smooth effects are stratified by sex.

models$gam.nb <-
  gam(
    deaths ~
      # special days
      first_week*sex*age.n.fct +
      last_week*sex*age.n.fct +
      week21*sex*age.n.fct +
      # log-linear long term trend
      time*sex*age.n.fct +
      # penalized spline for age effect
      s(age.n, bs = 'ps', k = 6, by = sex) +
      # penalized cyclic spline for seasonality
      s(week_ify, bs = 'cp', k = 8, by = sex) +
      # smooth interaction between age and seasonality
      ti(
        week_ify, age.n,
        bs = c('cp', 'ps'),
        k = c(8, 6),
        by = sex
      ) +
      offset(log(exposures)),
    data = dat$training.data,
    family = nb(link = 'log'),
    method = 'REML'
  )

# GAM Poisson -----------------------------------------------------

# Same as before, but with Poisson distribution.

models$gam.poisson <-
  gam(
    deaths ~
      # special days
      first_week*sex*age.n.fct +
      last_week*sex*age.n.fct +
      week21*sex*age.n.fct +
      # log-linear long term trend
      time*sex*age.n.fct +
      # penalized spline for age effect
      s(age.n, bs = 'ps', k = 6, by = sex) +
      # penalized cyclic spline for seasonality
      s(week_ify, bs = 'cp', k = 8, by = sex) +
      # smooth interaction between age and seasonality
      ti(
        week_ify, age.n,
        bs = c('cp', 'ps'),
        k = c(8, 6),
        by = sex
      ) +
      offset(log(exposures)),
    data = dat$training.data,
    family = poisson(link = 'log'),
    method = 'REML'
  )

# GLM Serfling ----------------------------------------------------

# The baseline specification of the FluMomo/Serfling model.

models$glm.serfling <-
  glm(
    deaths ~
      # special days
      first_week*sex*age.n.fct +
      last_week*sex*age.n.fct +
      week21*sex*age.n.fct +
      # log linear long-term-trend by age and sex
      time*age.n.fct*sex +
      # seasonality by age and sex
      # full year period
      sin(2*pi*week_ify/(365.25/7))*age.n.fct*sex +
      cos(2*pi*week_ify/(365.25/7))*age.n.fct*sex +
      # half year period
      sin(2*pi*week_ify/(365.25/2/7))*age.n.fct*sex +
      cos(2*pi*week_ify/(365.25/2/7))*age.n.fct*sex +
      offset(log(exposures)),
    data = dat$training.data,
    family = poisson(link = 'log')
  )

# Average mortality -----------------------------------------------

# This model and estimates the average mortality rate over
# some years within each week and stratum. The associated
# predict() method multiplies this average mortality with
# given exposures to derive death counts.

AverageMortalityModel <-
  function (df, year, week, deaths, exposures, training.years, ...) {
    require(dplyr)
    .year = enquo(year); .strata = enquos(...)
    .week = enquo(week); .deaths = enquo(deaths);
    .exposures = enquo(exposures)

    avg_mx <-
      df %>%
      filter(!!.year %in% training.years) %>%
      group_by(!!!.strata, !!.week) %>%
      summarise(
        avg_mortality = mean(!!.deaths/!!.exposures),
        .groups = 'drop'
      )

    structure(list(avg = avg_mx), class = 'avgmx')

  }

predict.avgmx <- function (object, newdata, exposures, ...) {
  require(dplyr)
  left_join(newdata, object$avg) %>%
    transmute(deaths = !!enquo(exposures) * avg_mortality) %>%
    pull(deaths)
}

models$avg.mortality <-
  AverageMortalityModel(
    dat$training.data,
    year, week,
    deaths, exposures,
    training.years = cnst$avg.mortality.period,
    sex, age.n
  )

# Extract expected death counts and prediction intervals ----------

# get names of the models
name.mod <- names(models)

# expected death counts
for (i in name.mod) {

  var.name   <- paste0('dx.',i)
  # get predictions
  Data.dt[, eval(var.name) := predict(
    models[[i]],
    newdata = .SD,
    exposures = exposures,
    type = 'response', se.fit = FALSE
  )]

}

# create a single data.table in the long form for the dashboard
output_columns <- c('sex', 'age.n','year','week','date', 'time',
                    'exposures','deaths', paste0('dx.', name.mod))
output_names <- c('sex', 'age.n', 'year', 'week', 'date', 'time',
                  'exposures', 'observed.deaths',
                  paste0('expected.', name.mod))
Deaths.UK <- Data.dt[,..output_columns]
names(Deaths.UK) <- output_names
Deaths.UK <- melt.data.table(
  Deaths.UK, id.vars =
    c('sex','age.n','year','week','date',
      'time','exposures', 'observed.deaths'),
  variable.name = 'model',
  value.name = 'expected.deaths'
)
Deaths.UK[, model := substring(model, 10)]

# calculate mx
Deaths.UK[, observed.mx := observed.deaths/exposures]
Deaths.UK[, observed.log.mx := log10(observed.mx)]

# calculate prediction intervals...
# ...for the negative binomial model
Deaths.UK[
  model == 'gam.nb', `:=`(
    lower.PI = qnbinom(
      0.025,
      mu = expected.deaths,
      size = models$gam.nb$family$getTheta(TRUE)
    ),
    upper.PI = qnbinom(
      0.975,
      mu = expected.deaths,
      size = models$gam.nb$family$getTheta(TRUE)
    )
  )]
# ...for the Poisson models
Deaths.UK[
  model != 'gam.nb', `:=`(
    lower.PI = qpois(
      0.025, lambda = expected.deaths,
    ),
    upper.PI = qpois(
      0.975, lambda = expected.deaths,
    )
  )]
Deaths.UK[,week:= week + 1]

save(Deaths.UK, models, file = 'Data/predicted_weekly_deaths_enwa.Rdata')

# Diagnostic plots ------------------------------------------------

PlotObservedVsExpectedDeaths <- function (
  df,
  date = date,
  expected_deaths = expected.deaths,
  expected_deaths_hi = lower.PI,
  expected_deaths_lo = upper.PI,
  observed_deaths = observed.deaths,
  color_by = model,
  facet_row = age.n,
  facet_col = model,
  title,
  caption_data,
  caption_note,
  date_breaks,
  date_labels,
  point_size = 0.2
) {

  require(ggplot2)
  require(rlang)

  the_plot <-
    ggplot(df) +
    list(
      # observed deaths
      geom_point(
        aes(
          x = {{date}},
          y = {{observed_deaths}}
        ),
        size = 0.2,
        color = 'grey70'
      ),
      # prediction interval
      geom_ribbon(
        aes(
          x = {{date}},
          ymin = {{expected_deaths_lo}},
          ymax = {{expected_deaths_hi}},
          fill = {{color_by}},
        ),
        alpha = 0.3
      ),
      # predicted deaths
      geom_line(
        aes(
          x = {{date}},
          y = {{expected_deaths}},
          color = {{color_by}}
        ),
        size = 0.5
      ),
      # facets
      facet_grid(
        rows = vars({{facet_row}}),
        cols = vars({{facet_col}}),
        scales = 'free_y'
      ),
      # scales
      scale_x_date(
        date_breaks =
          if (missing(date_breaks)) waiver() else date_breaks,
        date_labels =
          if (missing(date_labels)) waiver() else date_labels
      ),
      scale_y_continuous(
        # ensure integers
        labels = scales::label_number(1, big.mark = ',')
      ),
      # theming
      theme_minimal(),
      theme(panel.grid.minor = element_blank()),
      guides(color = 'none', fill = 'none'),
      # title
      if (!missing(title)) {
        ggtitle(title)
      },
      # labels
      labs(
        x = '',
        y = 'Deaths per week'
      ),
      # notes and data source
      if (any(!missing(caption_data), !missing(caption_note))) {
        labs(
          caption =
            paste0(
              ifelse(
                missing(caption_data),
                '',
                paste0('Raw Data: ', caption_data, '\n')
              ),
              ifelse(
                missing(caption_note),
                '',
                paste0('Note: ', caption_note)
              )
            )
        )
      }

    )

  print(the_plot)

}

Deaths.UK.for.plot <- copy(Deaths.UK)
Deaths.UK.for.plot[,`:=`(
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

fig$obs.vs.expected.males.short <-
  PlotObservedVsExpectedDeaths(
  Deaths.UK.for.plot[sex == 'm' & date >= '2019-06-01'],
  date_breaks = '3 months',
  date_labels = '%b %y'
) + fig_spec$MyGGplotTheme(hgrid = TRUE, scaler = 0.8)
fig_spec$ExportPDF(
  fig$obs.vs.expected.males.short,
  'obs_vs_expected_males_short',
  path = 'Figures/',
  width = fig_spec$width, height = 0.8*fig_spec$width
)
fig_spec$ExportPNG(
  fig$obs.vs.expected.males.short,
  'obs_vs_expected_males_short',
  path = 'Figures/',
  width = fig_spec$width, height = 0.8*fig_spec$width
)

fig$obs.vs.expected.females.short <-
  PlotObservedVsExpectedDeaths(
  Deaths.UK.for.plot[sex == 'f' & date >= '2019-06-01'],
  date_breaks = '3 months',
  date_labels = '%b %y'
) + fig_spec$MyGGplotTheme(hgrid = TRUE, scaler = 0.8)
fig_spec$ExportPDF(
  fig$obs.vs.expected.females.short,
  'obs_vs_expected_females_short',
  path = 'Figures/',
  width = fig_spec$width, height = 0.8*fig_spec$width
)
fig_spec$ExportPNG(
  fig$obs.vs.expected.females.short,
  'obs_vs_expected_females_short',
  path = 'Figures/',
  width = fig_spec$width, height = 0.8*fig_spec$width
)

fig$obs.vs.expected.males.long <-
  PlotObservedVsExpectedDeaths(
  Deaths.UK.for.plot[sex == 'm' & year >= (glob$jumpoff_year-5)],
  date_breaks = '1 year',
  date_labels = '%Y'
) + fig_spec$MyGGplotTheme(hgrid = TRUE, scaler = 0.8)
fig_spec$ExportPDF(
  fig$obs.vs.expected.males.long,
  'obs_vs_expected_males_long',
  path = 'Figures/',
  width = fig_spec$width, height = 0.8*fig_spec$width
)
fig_spec$ExportPNG(
  fig$obs.vs.expected.males.long,
  'obs_vs_expected_males_long',
  path = 'Figures/',
  width = fig_spec$width, height = 0.8*fig_spec$width
)

fig$obs.vs.expected.females.long <-
  PlotObservedVsExpectedDeaths(
    Deaths.UK.for.plot[sex == 'f' & year >= (glob$jumpoff_year-5)],
    date_breaks = '1 year',
    date_labels = '%Y'
  ) + fig_spec$MyGGplotTheme(hgrid = TRUE, scaler = 0.8)
fig_spec$ExportPDF(
  fig$obs.vs.expected.females.long,
  'obs_vs_expected_females_long',
  path = 'Figures/',
  width = fig_spec$width, height = 0.8*fig_spec$width
)
fig_spec$ExportPNG(
  fig$obs.vs.expected.females.long,
  'obs_vs_expected_females_long',
  path = 'Figures/',
  width = fig_spec$width, height = 0.8*fig_spec$width
)
