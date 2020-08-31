# Init ------------------------------------------------------------

set.seed(1987)

library(data.table)
library(ggplot2)

dat <- list()

# Constants -------------------------------------------------------

cnst <- list()
# starting year and weeks for excess deaths calculation
cnst$jumpoff.year <- 2020
cnst$jumpoff.week <- 10
cnst$final.week <- 33
# number of simulations for predictive intervals
cnst$n.sim <- 1e3

# Load data -------------------------------------------------------

load('Data/predicted_weekly_deaths_enwa.Rdata')

# Simulate (cumulative) excess death counts -----------------------

# subset to excess deaths date range
dat$deaths.subset <-
  copy(Deaths.UK[
    year %in% cnst$jumpoff.year &
      week >= cnst$jumpoff.week
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
    week == cnst$final.week,
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
  week == cnst$final.week,
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
    week == cnst$final.week,
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
    week == cnst$final.week,
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
    week == cnst$final.week,
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
    week == cnst$final.week,
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
    week == cnst$final.week,
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
