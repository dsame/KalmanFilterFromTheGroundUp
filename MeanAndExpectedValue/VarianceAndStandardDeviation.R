#Suppose we want to compare the heights of two high school basketball teams. The
#following table provides the players' heights and the mean height of each team.

teamA <- c(1.89, 2.1, 1.75, 1.98, 1.85)
meanTeamA <- mean(teamA)
teamB <- c(1.94, 1.9, 1.97, 1.89, 1.87)
meanTeamB <- mean(teamB)

#The distance from the mean for each variable would be
distanceFromMeanTeamA <- teamA - meanTeamA
distanceFromMeanTeamB <- teamB - meanTeamB

#let's avoid negative numbers by squaring the distances
squaredDistanceFromMeanTeamA <- distanceFromMeanTeamA^2
squaredDistanceFromMeanTeamB <- distanceFromMeanTeamB^2

sampledVarianceTeamA <- sum(squaredDistanceFromMeanTeamA) / (length(teamA) - 1)
sampledVarianceTeamB <- sum(squaredDistanceFromMeanTeamB) / (length(teamB) - 1)

#The variance is the average of the squared distances from the mean
varianceTeamA <- mean(squaredDistanceFromMeanTeamA)
varianceTeamB <- mean(squaredDistanceFromMeanTeamB)

#The standard deviation is the square root of the variance
standardDeviationTeamA <- sqrt(varianceTeamA)
standardDeviationTeamB <- sqrt(varianceTeamB)

#Plot the data with ggplot
library(tidyverse)
ggplot() +
  geom_point(data = data.frame(teamA), aes(x = 1:length(teamA), y = teamA, color = "Team A")) +
  geom_point(data = data.frame(teamB), aes(x = 1:length(teamB), y = teamB, color = "Team B")) +
  geom_hline(yintercept = meanTeamA, color = "red", linetype ="dashed") +
  geom_hline(yintercept = meanTeamB, color = "blue", linetype ="longdash") +
  geom_hline(yintercept = meanTeamA + standardDeviationTeamA, color = "red", linetype = "dashed") +
  geom_hline(yintercept = meanTeamA - standardDeviationTeamA, color = "red", linetype = "dashed") +
  geom_hline(yintercept = meanTeamB + standardDeviationTeamB, color = "blue", linetype = "dashed") +
  geom_hline(yintercept = meanTeamB - standardDeviationTeamB, color = "blue", linetype = "dashed")

# q: set color of geom_point 
# a: https://stackoverflow.com/questions/29938329/how-to-set-color-of-geom-point-in-ggplot2