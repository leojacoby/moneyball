library(tidyverse)
load("data/batting_2014_2015.RData")
batting_2014_2015 <-
  batting_2014_2015 %>% 
  spread(yearID, BA)
# spread? how does it work

batting_2014_2015 <- 
  batting_2014_2015 %>% 
  ungroup()

batting_2014_2015 <-
  batting_2014_2015 %>% 
  rename(BA_2014 = `2014`,
         BA_2015 = `2015`)

batting_2014_2015 <- 
  batting_2014_2015 %>% 
  mutate(yhat_0 = mean(BA_2015))

ggplot(batting_2014_2015) + 
  geom_point(aes(x = BA_2014, 
                 y = BA_2015), 
             col = "black", 
             shape = 16, 
             size = 0.9) + 
  geom_hline(yintercept = 0.273, 
             col = "red", 
             lty = 2) +
  geom_vline(xintercept = 0.272, 
             col = "red", 
             lty = 2) +
  NULL

# batting_2014_2015 <-
#   batting_2014_2015 %>% 
#   mutate(bins = cut(BA_2014, breaks = c(0.15, 0.272, 0.400))) %>% 
#   group_by(bins) %>% 
#   mutate(yhat_1 = mean(BA_2015)) %>% 
#   ungroup() %>% 
#   select(-bins)

batting_2014_2015 <-
  batting_2014_2015 %>% 
  mutate(bins = cut(BA_2014, breaks = seq(from = 0.15, to = 0.4, by = 0.05))) %>% 
  group_by(bins) %>% 
  mutate(yhat_2 = mean(BA_2015)) %>% 
  ungroup() %>% 
  select(-bins)

batting_2014_2015 <-
  batting_2014_2015 %>% 
  mutate(bins = cut(BA_2014, breaks = seq(from = 0.15, to = 0.4, by = 0.005))) %>% 
  group_by(bins) %>% 
  mutate(yhat_3 = mean(BA_2015)) %>% 
  ungroup() %>% 
  select(-bins)

ggplot(batting_2014_2015) + 
  geom_point(aes(x = BA_2014, y = BA_2015), col = "black", shape = 16, size = 0.9) + 
  geom_point(aes(x = BA_2014, y = yhat_0), col = "red", shape = 3, size = 0.75) + 
  geom_point(aes(x = BA_2014, y = yhat_1), col = "green", shape = 15, size = 0.75) +
  geom_point(aes(x = BA_2014, y = yhat_2), col = "blue", shape = 15, size = 0.75) +
  geom_point(aes(x = BA_2014, y = yhat_3), col = "purple", shape = 15, size = 0.75) +
  geom_line(aes(x = BA_2014, y = yhat_3), col = "purple")
  NULL

batting_2014_2015 %>% 
  summarize(RMSE_0 = sqrt(mean((BA_2015 - yhat_0)^2)),
            RMSE_1 = sqrt(mean((BA_2015 - yhat_1)^2)),
            RMSE_2 = sqrt(mean((BA_2015 - yhat_2)^2)),
            RMSE_3 = sqrt(mean((BA_2015 - yhat_3)^2)))







