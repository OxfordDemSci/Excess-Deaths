# Calculate weekly excess deaths for England & Wales 2020

# Init ------------------------------------------------------------

set.seed(1987)

library(data.table)
library(ggplot2)

source('R/00-global_constants.R')

dat <- list()
tab <- list()

# Constants -------------------------------------------------------

cnst <- list(
  # number of simulations for predictive intervals
  n.sim = 1e3,
  model.labels =
    c(
      `gam.nb` = 'GAM Negative Binomial',
      `gam.poisson` = 'GAM Poisson',
      `glm.serfling` = 'GLM Poisson (Serfling)',
      `avg.mortality` = 'Average mortality'
    ),
  sex.labels =
    c(
      `m` = 'Male',
      `f` = 'Female',
      `t` = 'Total'
    )
)

# Load data -------------------------------------------------------

load('Data/predicted_weekly_deaths_enwa.Rdata')

# Simulate (cumulative) excess death counts -----------------------

# subset to excess deaths date range
dat$deaths.subset <-
  copy(Deaths.UK[
    year %in% glob$jumpoff_year &
      week >= glob$jumpoff_week
  ])

# add data ID, timeseries ID
dat$deaths.subset[, data.id := .I]
dat$deaths.subset[, series.id := .GRP, by = .(sex, age.n, model)]

# prepare data frame for simulations
dat$simulations <-
  dat$deaths.subset[, .(
    model = rep(model, cnst$n.sim),
    series.id = rep(series.id, cnst$n.sim),
    simulation.id = 1:cnst$n.sim,
    # rate parameter of Poisson and negative binomial
    lambda = rep(expected.deaths, cnst$n.sim)
  ), by = .(data.id)]
dat$simulations[, theta := ifelse(
  model == 'gam.nb',
  # we estimated a single theta parameter for our NB model
  models$gam.nb$family$getTheta(TRUE),
  # negative binomial with infinite theta is just Poisson
  Inf
)]

# simulate death counts
dat$simulations[,
  simulated.deaths := rnbinom(cnst$n.sim, mu = lambda, size = theta),
  by = .(data.id)
]

# join simulations back into original data
dat$simulations <-
  dat$simulations[dat$deaths.subset, on = .(data.id, model, series.id)]

# calculate (cumulative) excess deaths,
# cumulative observed deaths and cumulative expected/simulated deaths
# for stratum, model, and simulation run
dat$simulations[, excess.deaths := observed.deaths - simulated.deaths]
dat$simulations[
  ,
  `:=`(
    cum.excess.deaths = cumsum(excess.deaths),
    cum.observed.deaths = cumsum(observed.deaths),
    cum.simulated.deaths = cumsum(simulated.deaths)
  ),
  by = .(simulation.id, series.id)
]

# Calculate statistics of interest --------------------------------

# Total deaths registered end of period
dat$results$total.deaths <-
  sum(dat$deaths.subset[
    model %in% 'gam.nb', observed.deaths])

# Excess deaths by week, sex, and age
dat$results$excess.deaths.complete <-
  dat$simulations[
    ,
    .(
      exposures = exposures[1],
      observed.deaths = observed.deaths[1],
      expected.deaths = expected.deaths[1],
      expected.deaths.qlo = lower.PI[1],
      expected.deaths.qhi = upper.PI[1],
      observed.mx = observed.mx[1],
      observed.log.mx = observed.log.mx[1],
      excess.deaths.avg =
        mean(excess.deaths),
      excess.deaths.qlo =
        quantile(excess.deaths, 0.025),
      excess.deaths.qhi =
        quantile(excess.deaths, 0.975),
      cum.excess.deaths.avg =
        mean(cum.excess.deaths),
      cum.excess.deaths.qlo =
        quantile(cum.excess.deaths, 0.025),
      cum.excess.deaths.qhi =
        quantile(cum.excess.deaths, 0.975)
    ),
    by = .(model, sex, age.n, year, week, date, time)
    ]

# Cumulative excess deaths by week and sex
dat$results$excess.deaths.week.sex <-
  dat$simulations[,
    .(cum.excess.deaths = sum(cum.excess.deaths)),
    # summation over age is implicit here
    by = .(simulation.id, date, model, sex)][
      ,
      .(
        avg = mean(cum.excess.deaths),
        qlo = quantile(cum.excess.deaths, 0.025),
        qhi = quantile(cum.excess.deaths, 0.975)
      ),
      by = .(model, date, sex)
    ]

# Total excess deaths end of period both sexes
dat$results$excess.deaths.total <-
  dat$simulations[
    week == glob$observed_weeks_2020,
  .(total.excess.deaths = sum(cum.excess.deaths)),
  # summation over sex and age is implicit here
  by = .(simulation.id, model)][
    ,
    .(
      avg = mean(total.excess.deaths),
      qlo = quantile(total.excess.deaths, 0.025),
      qhi = quantile(total.excess.deaths, 0.975)
    ),
    by = .(model)
  ]

# Total excess deaths end of period, by sex
dat$results$excess.deaths.sex <-
  dat$simulations[
  week == glob$observed_weeks_2020,
  .(total.excess.deaths = sum(cum.excess.deaths)),
  # summation over age is implicit here
  by = .(simulation.id, model, sex)][
    ,
    .(
      avg = mean(total.excess.deaths),
      qlo = quantile(total.excess.deaths, 0.025),
      qhi = quantile(total.excess.deaths, 0.975)
    ),
    by = .(model, sex)
  ]

# Total excess deaths end of period both sexes, by age
dat$results$excess.deaths.age <-
  dat$simulations[
    week == glob$observed_weeks_2020,
  .(total.excess.deaths = sum(cum.excess.deaths)),
  # summation over sex is implicit here
  by = .(simulation.id, model, age.n)][
    ,
    .(
      avg = mean(total.excess.deaths),
      qlo = quantile(total.excess.deaths, 0.025),
      qhi = quantile(total.excess.deaths, 0.975)
    ),
    by = .(model, age.n)
  ]

# Total excess deaths end of period, by age and sex
dat$results$excess.deaths.age.sex <-
  dat$simulations[
    week == glob$observed_weeks_2020,
  .(total.excess.deaths = sum(cum.excess.deaths)),
  by = .(simulation.id, model, age.n, sex)][
    ,
    .(
      avg = mean(total.excess.deaths),
      qlo = quantile(total.excess.deaths, 0.025),
      qhi = quantile(total.excess.deaths, 0.975)
    ),
    by = .(model, age.n, sex)
  ]

# Percent above expected total deaths end of period
dat$results$pct.excess.total <-
  dat$simulations[
    week == glob$observed_weeks_2020,
  .(percent.above =
      (sum(cum.observed.deaths)/sum(cum.simulated.deaths)-1)*100),
  # summation over sex and age is implicit here
  by = .(simulation.id, model)][
    ,
    .(
      avg = mean(percent.above),
      qlo = quantile(percent.above, 0.025),
      qhi = quantile(percent.above, 0.975)
    ),
    by = .(model)
  ]


# percent above expected total deaths end of period by age group
dat$results$pct.excess.age <-
  dat$simulations[
    week == glob$observed_weeks_2020,
  .(percent.above =
      (sum(cum.observed.deaths)/sum(cum.simulated.deaths)-1)*100),
  by = .(simulation.id, model,age.n)][
    ,
    .(
      avg = mean(percent.above),
      qlo = quantile(percent.above, 0.025),
      qhi = quantile(percent.above, 0.975)
    ),
    by = .(model,age.n)
  ]

# percent above expected total deaths end of period, by sex
dat$results$pct.excess.sex <-
  dat$simulations[
    week == glob$observed_weeks_2020,
  .(percent.above =
      (sum(cum.observed.deaths)/sum(cum.simulated.deaths)-1)*100),
  by = .(simulation.id, model, sex)][
    ,
    .(
      avg = mean(percent.above),
      qlo = quantile(percent.above, 0.025),
      qhi = quantile(percent.above, 0.975)
    ),
    by = .(model, sex)
  ]

# Save results ----------------------------------------------------

# fully stratified excess deaths for dashboard
write.csv(dat$results$excess.deaths.complete, file = 'Dashboard/Data/Excess_Deaths.csv')

results <- dat$results
save(results, file = 'Data/results.RData')

# Export tables ---------------------------------------------------

library(dplyr)
library(flextable)

# total excess deaths at end of observation period by model
tab$supp_tab1 <-
  results$excess.deaths.total %>%
  mutate(sex = 't') %>%
  select(sex, everything()) %>%
  bind_rows(results$excess.deaths.sex) %>%
  mutate(
    model =
      factor(model, names(cnst$model.labels), cnst$model.labels),
    sex =
      factor(sex, names(cnst$sex.labels), cnst$sex.labels)
  ) %>%
  tidyr::pivot_wider(
    values_from = c(avg, qlo, qhi),
    names_from = c(sex)
  ) %>%
  select(
    model,
    contains('Female'),
    contains('Male'),
    contains('Total')
  ) %>%
  flextable() %>%
  set_header_labels(
    model = 'Model',
    avg_Female = 'Excess',
    qlo_Female = '.05 PI',
    qhi_Female = '.95 PI',
    avg_Male = 'Excess',
    qlo_Male = '.05 PI',
    qhi_Male = '.95 PI',
    avg_Total = 'Excess',
    qlo_Total = '.05 PI',
    qhi_Total = '.95 PI'
  ) %>%
  add_header_row(
    values = c(' ', 'Female', 'Male', 'Total'),
    colwidths = c(1, 3, 3, 3)
  ) %>%
  theme_booktabs() %>%
  font(part = 'all', fontname = 'Times New Roman') %>%
  fontsize(part = 'all', size = 10) %>%
  fontsize(part = 'footer', size = 8) %>%
  bold(part = 'header') %>%
  align(part = 'header', align = 'center') %>%
  autofit(add_w = 0, add_h = 0)
print(tab$supp_tab1, preview = 'docx')
save_as_docx(tab$supp_tab1, path = 'Tables/total_excess_deaths.docx')

# total excess deaths at end of observation period by model and age
tab$supp_tab2 <-
  results$excess.deaths.age.sex %>%
  mutate(
    age.n = as.integer(age.n),
    model =
      factor(model, names(cnst$model.labels), cnst$model.labels),
    sex =
      factor(sex, names(cnst$sex.labels), cnst$sex.labels)
  ) %>%
  tidyr::pivot_wider(
    values_from = c(avg, qlo, qhi),
    names_from = c(sex)
  ) %>%
  select(
    model, age.n,
    contains('Female'),
    contains('Male'),
    contains('Total')
  ) %>%
  flextable() %>%
  merge_v(j = 'model') %>%
  set_header_labels(
    model = 'Model',
    age.n = 'Age group',
    avg_Female = 'Excess',
    qlo_Female = '.05 PI',
    qhi_Female = '.95 PI',
    avg_Male = 'Excess',
    qlo_Male = '.05 PI',
    qhi_Male = '.95 PI',
    avg_Total = 'Excess',
    qlo_Total = '.05 PI',
    qhi_Total = '.95 PI'
  ) %>%
  add_header_row(
    values = c('', 'Female', 'Male'),
    colwidths = c(2, 3, 3)
  ) %>%
  theme_booktabs() %>%
  font(part = 'all', fontname = 'Times New Roman') %>%
  fontsize(part = 'all', size = 10) %>%
  fontsize(part = 'footer', size = 8) %>%
  bold(part = 'header') %>%
  align(part = 'header', align = 'center') %>%
  autofit(add_w = 0, add_h = 0)
print(tab$supp_tab2, preview = 'docx')
save_as_docx(tab$supp_tab2, path = 'Tables/excess_deaths_by_age_sex.docx')


### some results for press release
pr <- dat$results$excess.deaths.complete
pr <- pr[model %in% 'avg.mortality']
pr <- pr[,list(observed.deaths = sum(observed.deaths), expected.deaths = sum(expected.deaths)),
         by = list(year,week,date)]

#Excess deaths 20 Nov to 31 December

#R: From week 47 (starting November 16) to week 53 (starting December 28) there were:

sum(pr[week %in% 47:53]$observed.deaths) - sum(pr[week %in% 47:53]$expected.deaths)

#how many people can normally be expected to die in December and how many more people died in Dec 2020 compared to 2019?

#R: expected in december
sum(pr[week %in% 49:53]$expected.deaths)

#R: observed in december
sum(pr[week %in% 49:53]$observed.deaths)

#Boris announced the 2nd wave 18 sept, so would be good to get data for total number of people
#who have died during the 2nd wave up to Dec 31st or beyond  (from all causes) compared to deaths from all causes during the 1st wave.

#R: expected in second wave
sum(pr[week %in% 39:53]$expected.deaths)

#R: observed in seconf wave
sum(pr[week %in% 39:53]$observed.deaths)

#e 2nd wave up to Dec 31st or beyond  (from all causes) compared to deaths from all causes during the 1st wave.

#R: expected in first wave
sum(pr[week %in% 10:24]$expected.deaths)

#R: observed in first wave
sum(pr[week %in% 10:24]$observed.deaths)


#R: expected in 2020
sum(pr[week %in% 10:53]$expected.deaths) - sum(pr[week %in% 10:53]$observed.deaths)

#R: observed in 2020
sum(pr[week %in% 10:53]$observed.deaths)



