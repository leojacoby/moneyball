
# why does it want select here?
Batting <- 
  Batting %>% 
  replace_na(list(HBP = 0, SF = 0, SH = 0, IBB = 0))

# why 502.2? why does it want select not filter?
batting <-
  Batting %>% 
  filter(AB + SH + SF + BB + HBP > 502, lgID %in% c('AL', 'NL')) %>% 
  mutate(PA = AB + SH + SF + BB + HBP,
         BA = H / AB,
         OBP = (H + HBP + BB) / PA,
         SLG = (H + X2B + 2 * X3B + 3 * HR) / AB, 
         OPS = OBP + SLG,
         wOBA = (0.688 * BB - IBB 
                        + 0.719 * HBP 
                        + .880	* H - X2B - X3B - HR 
                        + 1.250 * X2B
                        + 1.583 * X3B
                        + 2.042 * HR) / (PA - IBB)
         ) %>% 
  select(playerID, yearID, teamID, lgID, PA, BA, OBP, SLG, OPS, wOBA)

standardize <- function(x) {
  return((x - mean(x)) / sd(x))
}

batting <- ungroup(batting)

batting <- 
  batting %>% 
  mutate(zBA_all = standardize(BA), 
         zOBP_all = standardize(OBP),
         zOPS_all = standardize(OPS),
         zwOBA_all = standardize(wOBA)
         )

batting <- batting %>% group_by(yearID)

batting <- 
  batting %>% 
  mutate(zBA_year = standardize(BA), 
         zOBP_year = standardize(OBP),
         zOPS_year = standardize(OPS),
         zwOBA_year = standardize(wOBA)
  )


batting <- 
  batting %>% 
  ungroup() %>% 
  group_by(yearID, lgID) %>% 
  mutate(zBA_year_lg = standardize(BA), 
         zOBP_year_lg = standardize(OBP),
         zOPS_year_lg = standardize(OPS),
         zwOBA_year_lg = standardize(wOBA)
  )

batting <- 
  batting %>% 
  ungroup() %>% 
  mutate(HIST_ERA = case_when(
    yearID <= 1892 ~ "Pioneer",
    yearID > 1892 & yearID <= 1919 ~ "Spitball",
    yearID > 1919 & yearID <= 1946 ~ "Landis",
    yearID > 1947 & yearID <= 1968 ~ "Baby Boomer",
    yearID > 1968 & yearID <= 1992 ~ "Artificial Turf",
    yearID > 1992 & yearID <= 2014 ~ "Camden Yards",
    yearID > 2014 ~ "Statcast"
  )) %>% 
  group_by(HIST_ERA) %>% 
  mutate(zBA_hist = standardize(BA), 
         zOBP_hist = standardize(OBP),
         zOPS_hist = standardize(OPS),
         zwOBA_hist = standardize(wOBA)
  ) %>% ungroup()

batting_2014_2015 <-
   batting %>%
   filter(yearID %in% c(2014, 2015)) %>%
   group_by(playerID) %>%
   filter(n() == 2) %>% 
   select(playerID, yearID, BA) %>%
   arrange(playerID)

save(batting_2014_2015, file = "data/batting_2014_2015.RData")



# *******************************************

mlb_payrolls <- read_csv("data/mlb_payrolls.csv")

relative <- function(x) {
  med <- median(x, na.rm = TRUE)
  return(x/med)
}

mlb_payrolls <- mlb_payrolls %>% 
  group_by(Year) %>% 
  mutate(relative_payroll = relative(Team_Payroll))

ggplot(data = mlb_payrolls) +
  geom_point(mapping = aes(x = relative_payroll, y = Winning_Percentage), alpha = 0.5) +
  #xlab("Relative Payroll") +
  #ylab("Winning Payroll") +
  NULL

payroll_avg <- 
  mlb_payrolls %>% 
  summarize(payroll_avg = mean(Team_Payroll), 
            relative_payroll_avg = mean(relative_payroll))  

#linear increase
ggplot(data = mlb_payrolls) +
  geom_point(mapping = aes(x = Year, y = Team_Payroll),
             alpha = 0.5) +
  geom_line(
    data = payroll_avg, 
    mapping = aes(x = Year, 
                  y = payroll_avg),
    color="red") +
  NULL

#tough to tell trend
ggplot(data = mlb_payrolls) +
  geom_point(mapping = aes(x = Year, 
                           y = relative_payroll),
             alpha = 0.5) +
  geom_line(
    data = payroll_avg, 
    mapping = aes(x = Year, 
                  y = relative_payroll_avg),
    color="red") +
  NULL

payroll_importance <- 
  mlb_payrolls %>% 
  summarize(cor = cor(relative_payroll, 
                      Winning_Percentage))

ggplot(data = payroll_importance) +
  geom_point(mapping = aes(x = Year,
                           y = cor)) +
  NULL

