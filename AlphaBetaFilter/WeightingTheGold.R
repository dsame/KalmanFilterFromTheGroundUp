#' @title Weighting the Gold
library(tidyverse)

#' @description Collection of measurements
measurements <- c(996,	994,	1021,	1000,	1002,	1010,	983,	971,	993,	1023)

ggplot() +
  labs(x = "measurements", y= "gramms") +
  geom_point(data = data.frame(measurements), aes(x = 1:length(measurements), y = measurements)) +
  geom_hline(yintercept = mean(measurements), color = "red", linetype ="dashed") +
  geom_hline(yintercept = mean(measurements) + sd(measurements), color = "red", linetype = "dashed") +
  geom_hline(yintercept = mean(measurements) - sd(measurements), color = "red", linetype = "dashed")


#' @description  dynamic model of the system
#' 
#' @param x_n_1 previous state
#' @retuns current state

dynamiModel <- function(x_n_1) {
  return(x_n_1)
}

#' @description State Update Equation
#' 
#' @param x_n_1 previous estimation
#' @param z_n current measurement
#' @returns current updated estimation

stateEquation <- function(n, x_n_1, z_n) {
  alpha_n = 1/n
  innovation = (z_n - x_n_1)
  return(x_n_1 + alpha_n*innovation)
}

#' @description process measurements
#' 
#' @param x_0 initial state
#' @param Z measurements
#' @returns array of calculated states
filter <- function(x_0, Z) {
  # predict
  results = c()
  x_n_1 = dynamiModel(x_0)
  n = 1
  for (z_n in measurements) {
    # add to estimations
    results = c(results, x_n_1)
    # estimate
    x_n = stateEquation(n, x_n_1, z_n)
    # update
    x_n_1 = dynamiModel(x_n)
    n = n + 1
  }
  return(results)
}

estimations <- filter(980, measurements)
estimations <- filter(1020, measurements)
estimations <- filter(1000, measurements)
estimations <- filter(measurements[c(1)], measurements)

ggplot() +
  geom_line(data = data.frame(measurements), aes(x = 1:length(measurements), y = measurements)) +
  geom_hline(yintercept = mean(measurements), linetype = "longdash") +
  geom_hline(yintercept = mean(measurements) + sd(measurements), linetype = "dashed") +
  geom_hline(yintercept = mean(measurements) - sd(measurements), linetype = "dashed") +
  geom_line(data = data.frame(estimations), aes(x = 1:length(estimations), y = estimations), color = "red")+
  geom_hline(yintercept = mean(estimations), color = "red", linetype = "longdash") +
  geom_hline(yintercept = mean(estimations) + sd(estimations), color = "red", linetype = "dashed") +
  geom_hline(yintercept = mean(estimations) - sd(estimations),  color = "red", linetype = "dashed")
  