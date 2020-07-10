
library(ggplot2)
library(data.table)

rm(list=ls())

source('R/6_Get_data for Life_expectancy_inequality.R')

## get estimates of life expectancy and lifespan inequality

library(msm)

Total <- Deaths.Population.EW.2001.2020[,list(exposure = sum(exposure), deaths = sum(deaths), sex = 'both'),
                                        by = .(year,age)]

Deaths.Population.EW.2001.2020 <- rbind(Deaths.Population.EW.2001.2020[,c('year','sex','age','exposure','deaths')],
                                        Total[,c('year','sex','age','exposure','deaths')])

Deaths.Population.EW.2001.2020[,mx:= deaths/exposure]

# Empirical confidence intervals for life expectancy bootstraping 
# with exponential distribution with piecewise constant rate

#nmx <- Deaths.Population.EW.2001.2020[year == 2001 & sex == 'females']$mx
#age <- unique(Deaths.Population.EW.2001.2020$age)
# nsample <- 100
# level <- .95

CI.ex.fun <- function(nsample = 10000, mx, age,level=.95){
  
  e0.sim <- vector(mode='numeric', length = nsample)
  sd.sim <- vector(mode='numeric', length = nsample)
  
  for (i in 1:nsample){
    s         <- rpexp(nsample,mx,age)
    e0.sim[i] <- mean(s)
    sd.sim[i] <- sd(s)
  }
  
  e0 <- mean(e0.sim)
  
  sigma <- mean(sd.sim)
  
  CI.1 <- quantile(e0.sim,probs = c((1-level)/2,1 - (1-level)/2))
  
  CI.2 <- quantile(sd.sim,probs = c((1-level)/2,1 - (1-level)/2))
  
  CI <- cbind(e0 = e0,lower.e0 = CI.1[1], upper.e0 = CI.1[2], 
              sigma = sigma, lower.sigma = CI.2[1], upper.sigma = CI.2[2])
  CI <- data.table(CI)
  CI
}


EW.results <- Deaths.Population.EW.2001.2020[, CI.ex.fun(nsample = 1500,mx = mx,age = age) , by = .(year,sex)]

EW.results <- EW.results[order(year,sex)]

### let's compare results with standard life expectancy function
AKm02a0        <- function(m0, sex = "m"){
  sex <- rep(sex, length(m0))
  ifelse(sex == "m", 
         ifelse(m0 < .0230, {0.14929 - 1.99545 * m0},
                ifelse(m0 < 0.08307, {0.02832 + 3.26201 * m0},.29915)),
         # f
         ifelse(m0 < 0.01724, {0.14903 - 2.05527 * m0},
                ifelse(m0 < 0.06891, {0.04667 + 3.88089 * m0}, 0.31411))
  )
}

#sex <- 'f'

e0.frommx <- function(nmx =  mx, age = lower.age,  nax = c(nax,Inf), sex){
  n   <- c(diff(age), 999)
  
  nax[1]     <- AKm02a0(m0 = nmx[1], sex = sex)
  
  nqx          <- (n * nmx)/(1 + (n - nax) * nmx)
  nqx          <- c(nqx[-(length(nqx))], 1)
  nqx[nqx > 1] <- 1
  
  npx <- 1 - nqx
  lx <- cumprod(c(1, npx))
  ndx <- -diff(lx)
  lxpn <- lx[-1]
  nLxpn <- n * lxpn + ndx * nax
  nLx <- c(nLxpn[-length(nLxpn)], lxpn[length(lxpn)-1]/nmx[length(nmx)])
  Tx <- rev(cumsum(rev(nLx)))
  lx <- lx[1:length(age)]
  ex <- Tx/lx
  e0 <- ex[1]
  
  return(e0)
}


nax  <- diff(unique(Deaths.Population.EW.2001.2020$age))/2

EW.results.2 <- Deaths.Population.EW.2001.2020[, e0.frommx(nmx = mx,age = age,nax = c(nax,Inf),
                                                           sex = ifelse(sex == 'females','f','m') ) , by = .(year,sex)]


save(EW.results,EW.results.2,life.tables.EW.1982.2018,file = 'Data/LifeExpectancyInequality.RData')


