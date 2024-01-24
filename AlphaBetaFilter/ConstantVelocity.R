#' @title TRACKING THE CONSTANT VELOCITY AIRCRAFT IN ONE DIMENSION
library(tidyverse)
library(scales)

#' @description Collection of measurements
measurements <- c(30171,	30353,	30756,	30799,	31018,	31278,	31276,	31379,	31748,	32175)

# a: how to set the label for an x-axis in ggplot2
ggplot() +
  labs(x = "measurements", y= "feets") +
  geom_line(data = data.frame(measurements), aes(x = 1:length(measurements), y = measurements)) +
  geom_hline(yintercept = mean(measurements), color = "red", linetype ="dashed") +
  geom_hline(yintercept = mean(measurements) + sd(measurements), color = "red", linetype = "dashed") +
  geom_hline(yintercept = mean(measurements) - sd(measurements), color = "red", linetype = "dashed")

#' @description  dynamic model of the system
#' 
#' @param dt time interval
#' @param x_n vector of previous state
#' @retuns current state

dynamicModel <- function(dt, x_n) {
  A = matrix(c(1, dt, 0, 1), 2,2, TRUE)
  return(A %*% x_n)
}

#' @description State Update Equation
#' 
#' @param dt time interval needed to derive velocity
#' @param x_n_1 previous estimation vector
#' @param z_n current measurement
#' @returns current updated estimation

stateEquation <- function(dt, x_n_1, z_n) {
  alpha = 0.2 # trust model 4 times more than radar measurement
  
  xHat_n_1 = x_n_1[1,1]
  innovationX = (z_n - xHat_n_1)
  xHat_n = xHat_n_1 + alpha*innovationX
  
  beta = 0.1 # probability of the velocity has changed 10%
  
  vHat_n_1 = x_n_1[2,1]
  innovationV = (z_n - xHat_n_1)/dt
  vHat = (vHat_n_1 + beta*innovationV)
  
  return(matrix(c(xHat_n, vHat), nrow = 2, ncol = 1))
}

#' @description The Filter
#' 
#' @param x_0 initial state
#' @param Z measurements
#' @returns array of calculated states
filterGF <- function(x_0, Z,dt) {
  # setup time interval between measurements 5 seconds
  # predict
  results = list()
  x_n_1 = dynamicModel(dt, x_0)
  for (z_n in Z) {
    # estimate
    x_n = stateEquation(dt, x_n_1, z_n)
    # add to estimations
    results = c(results, list(x_n))
    # update
    x_n_1 = dynamicModel(dt, x_n)
  }
  return(results)
}

estimations <- filterGF(matrix(c(30000, 40),2), measurements,5)

estimationsX <- unlist(sapply(estimations, function(x) x[1]))
estimationsV <- unlist(sapply(estimations, function(x) x[2]))

ggplot() +
  labs(x = "measurements", y= "feets") +
  scale_x_continuous( breaks=pretty_breaks()) +
  geom_line(data = data.frame(measurements), aes(x = 1:length(measurements), y = measurements)) +
  geom_hline(yintercept = mean(measurements), linetype = "longdash") +
  geom_hline(yintercept = mean(measurements) + sd(measurements), linetype = "dashed") +
  geom_hline(yintercept = mean(measurements) - sd(measurements), linetype = "dashed") +
  geom_line(data = data.frame(estimationsX), aes(x = 1:length(estimationsX), y = estimationsX), color = "red")+
  geom_hline(yintercept = mean(estimationsX), color = "red", linetype = "longdash") +
  geom_hline(yintercept = mean(estimationsX) + sd(estimationsX), color = "red", linetype = "dashed") +
  geom_hline(yintercept = mean(estimationsX) - sd(estimationsX),  color = "red", linetype = "dashed")

