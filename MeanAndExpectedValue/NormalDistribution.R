# generate normal distributed data:

data <- rnorm(10000, mean = 0, sd = 1)

library(tidyverse)

ggplot() +
  geom_point(data = data.frame(data), aes(x = 1:length(data), y = data)) +
  geom_hline(yintercept = mean(data), color = "red", linetype ="dashed") +
  geom_hline(yintercept = mean(data) + sd(data), color = "red", linetype = "dashed") +
  geom_hline(yintercept = mean(data) - sd(data), color = "red", linetype = "dashed") +
  geom_hline(yintercept = mean(data) + var(data), color = "blue", linetype = "dashed") +
  geom_hline(yintercept = mean(data) - var(data), color = "blue", linetype = "dashed")

# ggplot distribution density with library function
ggplot() +
  geom_density(data = data.frame(data), aes(x = data))

# calculated distribution density manually

mean <- mean(data)
variance <- var(data)
sdeviation <- sd(data)

dencity <- function(x) {
  1 / sqrt(2 * pi * variance) * exp(-1/2 * (x - mean)^2 / variance)
}

dencityData <- dencity(data)
ggplot() +
  geom_point(data = data.frame(data, dencityData), aes(x = data, y = dencityData), color = "red")+
  geom_density(data = data.frame(data), aes(x = data))


# map each data to count of data rounded to 2 decimal places
dataCount <- data.frame(data) %>%
  mutate(data = round(data, 1)) %>%
  group_by(data) %>%
  summarise(count = n())

ggplot() +
  geom_point(data = dataCount, aes(x = data, y = count), color = "red")
  geom_density(data = data.frame(data), aes(x = data))

