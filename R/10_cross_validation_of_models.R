# Cross-validation of weekly death count predictive performance
# Jonas Schöley

# Init ------------------------------------------------------------

library(tidyverse)
library(mgcv)

source('R/Figure_specifications.R')

cnst <- list(
  # number of weeks under observation in 2020
  weeks_2020 = 47
)

dat <- list()
fig <- list()
tab <- list()

# Functions -------------------------------------------------------

# This model and estimates the average mortality rate over
# some years within each week and stratum. The associated
# predict() method multiplies this average mortality with
# given exposures to derive death counts.

AverageMortalityModel <-
  function(df, week, deaths, exposures, ...) {
    require(dplyr)
    .strata <- enquos(...)
    .week <- enquo(week)
    .deaths <- enquo(deaths)
    .exposures <- enquo(exposures)

    avg_mx <-
      df %>%
      group_by(!!!.strata, !!.week) %>%
      summarise(
        avg_mortality = mean(!!.deaths / !!.exposures),
        .groups = "drop"
      )

    structure(list(avg = avg_mx), class = "avgmx")
  }

predict.avgmx <- function(object, newdata, ...) {
  require(dplyr)

  suppressMessages(left_join(newdata, object$avg)) %>%
    pull(avg_mortality)
}

EpiYearSequence <- function(from, to) {
  years <- from:to
  paste0(head(years, -1), "/", years[-1])
}

# Data preparation ------------------------------------------------

load('Data/weekly_deaths_enwa.Rdata')

dat$raw <- as_tibble(Data.dt)

# definition of cross-validation series
dat$cv <-
  # set up K-fold cross-validation
  map(2010:2016, ~.x+0:3)

# test training split for each cv series
dat$tt <-
  dat$cv %>%
  map(~filter(dat$raw, year %in% .x)) %>%
  map(~mutate(
    .x, training = ifelse(
      (year == max(year) & iso.week >= 10),
      'test', 'training')
  )) %>%
  bind_rows(.id = 'cv_id') %>%
  mutate(cv_id = as.integer(cv_id)) %>%
  filter(training == 'training' | (training == 'test' & iso.week <= cnst$weeks_2020)) %>%
  group_by(cv_id) %>%
  mutate(
    # weeks since start of series
    time =
      difftime(date, min(date), units = 'weeks') %>%
      floor() %>% as.integer()
  ) %>%
  ungroup()

# example of cross validation set for sub-population of males aged 75-85
fig$cv <-
  dat$tt %>%
  filter(age.n == 75, sex == 'm') %>%
  ggplot() +
  geom_line(
    aes(x = date, y = cv_id, group = cv_id, color = training),
    size = 2
  ) +
  scale_y_continuous(breaks = 0:10) +
  scale_x_date(breaks = '1 year', date_labels = '%Y') +
  fig_spec$MyGGplotTheme(hgrid = TRUE) +
  labs(y = 'Cross-validation series', x = NULL) +
  theme(
    legend.position = c(0.2, 0.8),
    legend.background = element_blank(),
    legend.title = element_blank()
  )

# Specifications of models to test --------------------------------

# just a big list specifying all the models to be tested

dat$mod_spec <-
  tribble(
    ~model_name, ~model_class, ~model_spec,
    'Avg. weekly mortality', 'avgmx', NA,
    'Serfling-Poisson GLM', 'glm', list(
      formula = formula(
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
          offset(log(exposures))
      ),
      family = poisson(link = 'log')
    ),
    'Poisson GAM', 'gam', list(
      formula = formula(
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
          offset(log(exposures))
      ),
      family = poisson(link = 'log')
    ),
    'Neg-Bin. GAM', 'gam', list(
      formula = formula(
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
          offset(log(exposures))
      ),
      family = nb(link = 'log')
    )
  ) %>%
  mutate(model_id = 1:n())

# Fit models to training sets -------------------------------------

dat$mod <-
  dat$tt %>%
  nest(
    data = c(-cv_id)
  ) %>%
  expand_grid(
    nest(
      dat$mod_spec,
      model_spec = c(-model_name, -model_id, -model_class)
    )
  )

dat$mod <-
  dat$mod %>%
  group_by(model_name, cv_id) %>%
  group_modify(~{

    cat('Fit ', unlist(.y)[1], ' on CV set ', unlist(.y)[2], '\n', sep = '')

    ### PREPARE INPUT DATA

    model_data <- .x$data[[1]]

    training_data <- filter(model_data, training == 'training')

    ### TRAIN MODELS

    if (.x$model_class == 'avgmx') {

      # calculate average weekly mortality rate over training data
      model <-
        AverageMortalityModel(
          training_data,
          week,
          deaths,
          exposures,
          sex, age.n.fct
        )

    }

    if (.x$model_class == 'glm') {

      model <-
        glm(
          formula = .x$model_spec[[1]][[1]][[1]]$formula,
          data = training_data,
          family = .x$model_spec[[1]][[1]][[1]]$family
        )

    }

    if (.x$model_class == 'gam') {

      model <-
        gam(
          formula = .x$model_spec[[1]][[1]][[1]]$formula,
          data = training_data,
          family = .x$model_spec[[1]][[1]][[1]]$family,
          method = 'REML'
        )

    }

    ### PREDICT FROM MODEL

    predictions <-
      model_data %>%
      # predict mortality rates
      mutate(
        predicted_mortality =
          predict(
            model,
            newdata = mutate(., exposures = 1),
            type = 'response',
            newdata.guaranteed = TRUE
          ),
        predicted_deaths =
          predicted_mortality * exposures
      )

    result <-
      tibble(
        training_data = list(training_data),
        predictions = list(predictions),
        fitted_model = list(model)
      )

    return(result)

  }) %>%
  ungroup()

# Predicted versus fitted -----------------------------------------

dat$mod %>%
  unnest(predictions) %>%
  filter(training == 'test', cv_id == 7) %>%
  group_by(model_name, age.n.fct, week) %>%
  summarise(
    observed_deaths = sum(deaths),
    predicted_deaths = sum(predicted_deaths)
  ) %>%
  ggplot(aes(x = week)) +
  geom_point(aes(y = observed_deaths)) +
  geom_line(aes(y = predicted_deaths)) +
  scale_y_continuous(labels = scales::label_comma()) +
  facet_grid(age.n.fct~model_name, scales = 'free_y') +
  fig_spec$MyGGplotTheme()

# Residual diagnostics --------------------------------------------

# various types of residuals by week and sex
dat$residuals_by_week_and_sex <-
  dat$mod %>%
  unnest(predictions) %>%
  group_by(cv_id, model_name, training,
           week, sex) %>%
  summarise(
    observed_deaths =
      sum(deaths, na.rm = T),
    predicted_deaths =
      sum(predicted_deaths, na.rm = T),
    exposures =
      sum(exposures)
  ) %>%
  ungroup() %>%
  mutate(
    observed_mortality =
      observed_deaths/exposures,
    predicted_mortality =
      predicted_deaths/exposures,
    residual_deaths_e =
      observed_deaths - predicted_deaths,
    residual_deaths_pe =
      (observed_deaths - predicted_deaths) / observed_deaths * 100,
    residual_mortality_pe =
      (observed_mortality - predicted_mortality) / observed_mortality * 100,
    residual_log_mortality_e =
      log(observed_mortality) - log(predicted_mortality)
  )

# residual summaries by model and sex
dat$summarised_residuals <-
  dat$residuals_by_week_and_sex %>%
  group_by(model_name, training, sex) %>%
  summarise(
    mpe = mean(residual_deaths_pe, na.rm = TRUE),
    me = mean(residual_deaths_e, na.rm = TRUE),
    mdape = median(abs(residual_deaths_pe), na.rm = TRUE),
    mdae = median(abs(residual_deaths_e), na.rm = TRUE)
  ) %>%
  ungroup()

# plot test residuals over weeks
dat$residuals_by_week_and_sex %>%
  pivot_longer(cols = c(residual_deaths_e:residual_log_mortality_e),
               names_to = 'residual_type', values_to = 'residual_value') %>%
  filter(training == 'test') %>%
  group_by(residual_type) %>%
  group_walk(~{
    fig[[paste0(.y$residual_type, '_by_week')]] <<-
      .x %>%
      ggplot(aes(x = week, y = residual_value)) +
      geom_point(
        color = 'grey60', size = 0.3
      ) +
      geom_smooth(
        se = FALSE, color = 'black',
      ) +
      geom_hline(yintercept = 0) +
      fig_spec$MyGGplotTheme(hgrid = TRUE) +
      facet_grid(sex~model_name) +
      scale_x_continuous(breaks = seq(1, 52, 4)) +
      labs(x = 'Week of year', y = paste0('Residual ', .y$residual_type))
  })

# plot distribution of residuals on test data
dat$residuals_by_week_and_sex %>%
  pivot_longer(cols = c(residual_deaths_e:residual_log_mortality_e),
               names_to = 'residual_type', values_to = 'residual_value') %>%
  filter(training == 'test') %>%
  group_by(residual_type) %>%
  group_walk(~{
    residual_summary <-
      .x %>%
      group_by(model_name, sex) %>%
      summarise(
        mean = mean(residual_value, na.rm = TRUE),
        median = median(residual_value, na.rm = TRUE)
      ) %>%
      ungroup()

    fig[[paste0(.y$residual_type, '_distribution')]] <<-
      ggplot(.x) +
      geom_vline(xintercept = 0, color = 'grey80') +
      geom_density(
        aes(
          x = residual_value,
          color = model_name
        )
      ) +
      geom_text(
        aes(
          x = 0, y = 0,
          label = paste0('Mean ', formatC(mean, 3)),
          color = model_name
        ),
        size = 3,
        data = residual_summary,
        hjust = 0.5,
        position = position_stack(vjust = 1)
      ) +
      scale_x_continuous() +
      facet_grid(sex~model_name) +
      guides(color = 'none') +
      fig_spec$MyGGplotTheme() +
      labs(x = .y$residual_type)
  })

# plot summary measures of predictive performance
dat$summarised_residuals %>%
  pivot_longer(
    c(mpe, me, mdape, mdae),
    names_to = 'summary_type', values_to = 'summary_value'
  ) %>%
  pivot_wider(
    names_from = training,
    values_from = summary_value
  ) %>%
  group_by(summary_type) %>%
  group_walk(~{
    fig[[paste0(.y$summary_type, '_summary')]] <<-
      .x %>%
      mutate(
        model_name =
          fct_reorder(model_name, test, .desc = FALSE, .fun = mean)
      ) %>%
      ggplot(aes(group = sex, color = sex)) +
      geom_linerange(
        aes(x = model_name, ymin = training, ymax = test),
        #arrow = arrow(angle = 10, type = 'closed', length = unit(3, 'mm')),
        position = position_dodge2(width = 0.5)
      ) +
      geom_point(
        aes(x = model_name, y = training),
        position = position_dodge2(width = 0.5),
        fill = 'white',
        shape = 21
      ) +
      geom_point(
        aes(x = model_name, y = test),
        position = position_dodge2(width = 0.5)
      ) +
      geom_text(
        aes(x = model_name, y = test+0.4,
            label = paste0(formatC(test, digits = 3))),
        position = position_dodge2(width = 0.5)
      ) +
      fig_spec$MyGGplotTheme() +
      coord_flip() +
      labs(x = NULL, y = .y$summary_type)

  })

# Make tables -----------------------------------------------------

library(flextable)

tab$mdape <-
  dat$summarised_residuals %>%
  filter(training == 'test') %>%
  select(model_name, sex, mdape, mdae, me) %>%
  pivot_wider(names_from = sex, values_from = c(mdape, mdae, me)) %>%
  select(model_name, contains('_f'), contains('_m')) %>%
  flextable() %>%
  set_formatter(
    mdape_f = function(x) formatC(x, digits = 1, format = 'f'),
    mdae_f = function(x) formatC(x, format = 'd'),
    me_f = function(x) formatC(x, format = 'f', digits = 1, flag = '+'),
    mdape_m = function(x) formatC(x, digits = 1, format = 'f'),
    mdae_m = function(x) formatC(x, format = 'd'),
    me_m = function(x) formatC(x, format = 'f', digits = 1, flag = '+')
  ) %>%
  bold(~ min(mdape_f) == mdape_f, ~ mdape_f) %>%
  bold(~ min(mdae_f) == mdae_f, ~ mdae_f) %>%
  bold(~ min(abs(me_f)) == abs(me_f), ~ me_f) %>%
  bold(~ min(mdape_m) == mdape_m, ~ mdape_m) %>%
  bold(~ min(mdae_m) == mdae_m, ~ mdae_m) %>%
  bold(~ min(abs(me_m)) == abs(me_m), ~ me_m) %>%
  set_header_labels(
    model_name = 'Model',
    mdape_f = 'MdAPE',
    mdae_f = 'MdAE',
    me_f = 'ME',
    mdape_m = 'MdAPE',
    mdae_m = 'MdAE',
    me_m = 'ME'
  ) %>%
  footnote(
    i = 1, j = 2:4,
    value = as_paragraph(
      c('Median absolute percentage error',
        'Median absolute error',
        'Mean error')
    ),
    part = 'header'
  ) %>%
  add_header_row(
    values = c('', 'Female', 'Male'),
    colwidths = c(1, 3, 3)
  ) %>%
  theme_booktabs() %>%
  font(part = 'all', fontname = 'Times New Roman') %>%
  fontsize(part = 'all', size = 10) %>%
  fontsize(part = 'footer', size = 8) %>%
  bold(part = 'header') %>%
  align(part = 'header', align = 'center') %>%
  autofit(add_w = 0, add_h = 0)
print(tab$mdape, preview = 'docx')
save_as_docx(tab$mdape, path = 'Tables/prediction_errors.docx')
