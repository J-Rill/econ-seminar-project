library(synthdid)
library(abind)
library(tidyverse)
library(reshape2)
library(data.table)

europe_data = read.csv('data/europe_df.csv')
kazakhstan_data = read.csv('data/kazakhstan_df.csv')
korea_data = read.csv('data/korea_df.csv')

attach(europe_data)
gdp_cov <- panel.matrices(europe_data, unit='country', time='year', outcome='gdp', treatment='treated')$Y
pop_cov <- panel.matrices(europe_data, unit='country', time='year', outcome='population', treatment = 'treated')$Y

# Still trying to figure out how to add covariates; the R documentation (https://rdrr.io/github/synth-inference/synthdid/man/synthdid-package.html) 
# is unclear.
X_matrix <- abind(gdp_cov, pop_cov, along=3)

X_mat <- europe_data[, c('country', 'year', 'treated', 'gdp', 'population')]
X = c(paste0("X", 1:2))

for (i in 1:2) {
  assign(X[i], panel.matrices(X_mat, unit = 1, time = 2, outcome = (i + 3), treatment = 3)$Y)
}
control <- abind(X1, X2, along = 3)


synthdid_analysis <- function(data) {
  setup <- panel.matrices(data, unit='country', time='year', outcome = 'co2', treatment = 'treated')
  tau.hat <- synthdid_estimate(setup$Y, setup$N0, setup$T0)
  point_estimate <- sprintf('point estimate: %1.2f', tau.hat)
  ci <- sprintf('95%% CI (%1.2f, %1.2f)', tau.hat - 1.96 * se, tau.hat + 1.96 * se)
  plot(tau.hat)
  return(tau.hat)
}

europe_analysis <- synthdid_analysis(data=europe_data)
europe_point_estimate <- sprintf('point estimate: %1.2f', europe_analysis)
europe_ci <- sprintf('95%% CI (%1.2f, %1.2f)', europe_analysis - 1.96 * se, europe_analysis + 1.96 * se)
plot(europe_analysis)


kazakhstan_analysis <- synthdid_analysis(data=kazakhstan_data)
kazakhstan_point_estimate <- sprintf('point estimate: %1.2f', kazakhstan_analysis)
kazakhstan_ci <- sprintf('95%% CI (%1.2f, %1.2f)', kazakhstan_analysis - 1.96 * se, kazakhstan_analysis + 1.96 * se)
plot(kazakhstan_analysis)

korea_analysis <- synthdid_analysis(data=korea_data)
korea_point_estimate <- sprintf('point estimate: %1.2f', korea_analysis)
korea_ci <- sprintf('95%% CI (%1.2f, %1.2f)', korea_analysis - 1.96 * se, korea_analysis + 1.96 * se)
plot(korea_analysis)
