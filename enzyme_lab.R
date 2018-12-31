concentrations <- c(3, 1.5, 0.75, 0.5, 0.375)
volo2_1 <- c(18, 9, 7.5, 5, 2)
volo2_2 <- c(18, 9, 8, 6, 2.5)
volo2 <- (volo2_1 + volo2_2)/2

dat <- data.frame(concentration = concentrations, volume_of_gas = volo2)

fit <- lm(volume_of_gas ~ concentration, data = dat)

fit[["coefficients"]]

ggplot(dat, aes(x=concentration, y=volume_of_gas)) +
  geom_point() +
  geom_abline(intercept = 2.024767, slope = 5.285904, color = "red") +
  labs(x = "Concentration (% substrate)", y = "Volume of Oxygen gas produced") +
  xlim(0, 4) + 
  ylim(0, 20)


temps <- c(42.9, 71.5, 102.2)
volo2 <- c(5, 10, 22.5)

dat <- data.frame(temperature = temps, volume_of_gas = volo2)

fit <- lm(volume_of_gas ~ temperature, data = dat)

fit[["coefficients"]]

ggplot(dat, aes(x=temperature, y=volume_of_gas)) +
  geom_point() +
  geom_abline(intercept = -8.9057579, slope = 0.2964786, color = "red") +
  labs(x = "Temperature", y = "Volume of Oxygen gas produced") +
  ylim(0, 25)

pH <- c(4, 6, 8)
volo2 <- c(5, 10, 22.5)

dat <- data.frame(temperature = temps, volume_of_gas = volo2)

fit <- lm(volume_of_gas ~ temperature, data = dat)

fit[["coefficients"]]

ggplot(dat, aes(x=temperature, y=volume_of_gas)) +
  geom_point() +
  geom_abline(intercept = -8.9057579, slope = 0.2964786, color = "red") +
  labs(x = "Temperature", y = "Volume of Oxygen gas produced") +
  ylim(0, 25)

