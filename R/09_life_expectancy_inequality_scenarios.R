library(data.table)

rm(list=ls())

source('R/LifeTableFUN.R')
source('R/00-global_constants.R')

#baseline qx based on https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/lifeexpectancies/datasets/lifetablesprincipalprojectionenglandandwales

qx.males.2020 <- c(0.00378,0.00036,0.00017,0.00009,0.00008,0.00008,0.00007
                   ,0.00007,0.00006,0.00006,0.00006,0.00007,0.00008,0.00010
                   ,0.00012,0.00015,0.00019,0.00023,0.00029,0.00034,0.00038,
                   0.00040,0.00042,0.00043,0.00043,0.00045,0.00047,0.00050,
                   0.00053,0.00057,0.00061,0.00066,0.00072,0.00077,0.00083,
                   0.00091,0.00099,0.00108,0.00118,0.00129,0.00140,0.00152,
                   0.00165,0.00178,0.00192,0.00207,0.00223,0.00240,0.00257,
                   0.00276,0.00296,0.00319,0.00343,0.00372,0.00404,0.00443,
                   0.00487,0.00538,0.00594,0.00657,0.00724,0.00795,0.00870,
                   0.00950,0.01036,0.01128,0.01228,0.01339,0.01464,0.01604,
                   0.01764,0.01946,0.02154,0.02392,0.02664,0.02972,0.03317,
                   0.03700,0.04126,0.04602,0.05139,0.05755,0.06462,0.07269,
                   0.08185,0.09215,0.10358,0.11612,0.12981,0.14471,0.16088,
                   0.17830,0.19692,0.21665,0.23733,0.25869,0.28104,0.30521,
                   0.33110,0.35789,0.38494)

qx.females.2020 <- c(0.00307,0.00030,0.00015,0.00008,0.00007,0.00007,0.00006,
                     0.00006,0.00006,0.00005,0.00005,0.00006,0.00007,0.00008,
                     0.00009,0.00011,0.00013,0.00014,0.00016,0.00017,0.00018,
                     0.00018,0.00019,0.00019,0.00020,0.00021,0.00023,0.00025,
                     0.00028,0.00031,0.00034,0.00038,0.00041,0.00045,0.00048,
                     0.00053,0.00058,0.00064,0.00070,0.00077,0.00084,0.00091,
                     0.00099,0.00107,0.00116,0.00126,0.00137,0.00149,0.00162,
                     0.00177,0.00194,0.00211,0.00231,0.00252,0.00276,0.00302,
                     0.00331,0.00363,0.00399,0.00437,0.00478,0.00523,0.00570,
                     0.00621,0.00677,0.00738,0.00808,0.00886,0.00973,0.01073,
                     0.01187,0.01317,0.01466,0.01637,0.01833,0.02055,0.02306,
                     0.02591,0.02916,0.03287,0.03712,0.04203,0.04774,0.05439,
                     0.06210,0.07098,0.08109,0.09249,0.10520,0.11922,0.13450,
                     0.15099,0.16859,0.18718,0.20661,0.22699,0.24894,0.27295,
                     0.29882,0.32586,0.35383)

x <- 0:100

############################

load('Data/LifeExpectancyInequality.RData')

ex.2020.1 <- EW.results[sex != 'both' & year == 2020]

##### Scenario 0. Baseline
#females
round(lifetable.qx(x,qx.females.2020,sex = 'F')$ex[1],2)

#males
round(lifetable.qx(x,qx.males.2020,sex = 'M')$ex[1],2)


##### Scenario 1. deaths go back to baseline
#females
round(
  ex.2020.1$ex[1]*(glob$observed_weeks_2020/glob$total_weeks_2020) +
    lifetable.qx(x,qx.females.2020,sex = 'F')$ex[1]*
    ((glob$total_weeks_2020-glob$observed_weeks_2020)/glob$total_weeks_2020),
  2)

#males
round(
  ex.2020.1$ex[2]*(glob$observed_weeks_2020/glob$total_weeks_2020) +
    lifetable.qx(x,qx.males.2020,sex = 'M')$ex[1]*
    ((glob$total_weeks_2020-glob$observed_weeks_2020)/glob$total_weeks_2020),
  2)


##### Scenario 2. deaths go back to below 10%
#females
round(
  ex.2020.1$ex[1]*(glob$observed_weeks_2020/glob$total_weeks_2020) +
    lifetable.qx(x,qx.females.2020*.9,sex = 'F')$ex[1]*
    ((glob$total_weeks_2020-glob$observed_weeks_2020)/glob$total_weeks_2020),
  2)

#males
round(
  ex.2020.1$ex[2]*(glob$observed_weeks_2020/glob$total_weeks_2020) +
    lifetable.qx(x,qx.males.2020*.9,sex = 'M')$ex[1]*
    ((glob$total_weeks_2020-glob$observed_weeks_2020)/glob$total_weeks_2020),
  2)

#.4*12

##### Scenario 2. deaths go back to above 10%
#females
#females
round(
  ex.2020.1$ex[1]*(glob$observed_weeks_2020/glob$total_weeks_2020) +
    lifetable.qx(x,qx.females.2020*1.1,sex = 'F')$ex[1]*
    ((glob$total_weeks_2020-glob$observed_weeks_2020)/glob$total_weeks_2020),
  2)

#males
round(
  ex.2020.1$ex[2]*(glob$observed_weeks_2020/glob$total_weeks_2020) +
    lifetable.qx(x,qx.males.2020*1.1,sex = 'M')$ex[1]*
    ((glob$total_weeks_2020-glob$observed_weeks_2020)/glob$total_weeks_2020),
  2)

#(83.6-82.48)

#(80.12-78.73)

