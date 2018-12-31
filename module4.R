# install.packages("Lahman")
# piping:  var <- data %>% func1() %>% func2() %>% func3()
library(tidyverse)
library(Lahman)
Pitching <- as_tibble(Pitching) # or... Pitching <- Pitching %>% as_tibble()
pitching <- Pitching %>% 
  mutate(IP = IPouts /3) %>% 
  filter(lgID %in% c("AL", "NL") & IP >= 150) %>% 
  select(playerID, yearID, teamID, lgID, IP, ERA) %>% 
  arrange(ERA)

ggplot(data = pitching) + 
  ylim(0, 10) +
  geom_point(mapping = aes(x = yearID, y = ERA), size = 0.3)

standardize <- function(x) {
  mu <- mean(x, na.rm = TRUE)
  sigma <- sd(x, na.rm = TRUE)
  return( (x-mu)/sigma )
}

pitching <- pitching %>% mutate(zERA_all = standardize(ERA))
pitching <- pitching %>% group_by(yearID)
# how does summarize know to group by groups?

pitching_summary <- pitching %>%
  summarize(mean = mean(ERA), sd = sd(ERA))

# how tf does this work?
pitching <- 
  pitching %>% 
  mutate(zERA_year = standardize(ERA))

#for 1 function do we pipe?
pitching <- pitching %>% ungroup()

#group by league and year...What does count do here???
pitching <-
  pitching %>% 
  group_by(yearID, lgID) %>% 
  mutate(zERA_year_lg = standardize(ERA), count = n())

ggplot(pitching) +
  geom_point(aes(x = yearID, y = ERA)) +
  ylim(0, 10) +
  geom_point(mapping = aes())


