library("tidyverse")

start <- c(1.89, 1.96, 1.94, 1.94)
end <- c(2.07, 2.00, 1.75, 1.46)
delta_mass <- end - start
percent_change <- delta_mass / start * 100
concentrations <- c(5, 10, 15, 20)
data <- data.frame(concentrations, percent_change)

fit <- lm(percent_change ~ concentrations, data = data)

(-1*fit[["coefficients"]][1])/fit[["coefficients"]][2]



ggplot(data, aes(x=concentrations, y=percent_change)) +
  geom_point() +
  labs(x = "Concentration of sugar (%)", y = "Percent change in mass", title = "The effect of sugar concentration on percent mass change of a potato core") +
  geom_hline(yintercept = 0, col = "red", lty = 2) +
  geom_abline(intercept = fit[["coefficients"]][1], slope = fit[["coefficients"]][2], color = "blue") +
  NULL
