library(tidyverse)
load("data/batting_2014_2015.RData")
batting_2014_2015 <- batting_2014_2015 %>% spread(yearID, BA)

batting_2014_2015 <- batting_2014_2015 %>% ungroup()

batting_2014_2015 <-
  batting_2014_2015 %>% 
  rename(BA_2014 = `2014`,
         BA_2015 = `2015`)

ggplot(data = batting_2014_2015) +
  geom_point(aes(x = BA_2014, y = BA_2015)) +
  geom_abline(intercept = 0.141, slope = 0.485, color = "red") +
  NULL

fit <- lm(data = batting_2014_2015, BA_2015 ~ BA_2014)

cor(batting_2014_2015$BA_2014, batting_2014_2015$BA_2015)

library(modelr)

rsquare(fit, batting_2014_2015)

batting_2014_2015 <- batting_2014_2015 %>% 
  add_predictions(fit) %>% 
  add_residuals(fit)

ggplot(batting_2014_2015) +
  geom_segment(aes(x = BA_2014, xend = BA_2014, y = BA_2015, yend = pred),
               color = "dodgerblue") +
  geom_point(aes(x = BA_2014, y = BA_2015)) +
  geom_abline(intercept = 0.141, slope = 0.485, color = "red") +
  NULL

new_data <- tibble(BA_2014 = c(0.241, 0.31, 0.265))
new_pred <- predict(fit, new_data)
new_data <- new_data %>% 
  add_predictions(fit)

ggplot(batting_2014_2015) +
  geom_point(aes(x = BA_2014, y = BA_2015)) +
  geom_abline(intercept = 0.1409779, slope = 0.4851417, color = "red") +
  geom_point(data = new_data, mapping = aes(x = BA_2014, y = pred), color = "green")

grid <- batting_2014_2015 %>% 
  data_grid(BA_2014)

grid <- grid %>% 
  add_predictions(fit)

#connect points to make more complex visual fits
ggplot(data = batting_2014_2015) +
  geom_point(aes(x = BA_2014, y = BA_2015)) +
  geom_line(data = grid, aes(x = BA_2014, y = pred), colour = "red")

#glm for logistic fit
