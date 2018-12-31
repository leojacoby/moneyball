hitting_qualified <- read_csv("data/hitting_qualified.csv")
# find out years that this file has...
summarize(hitting_qualified, min(yearID), max(yearID))
hitting_qualified <- mutate(hitting_qualified, 
                            IBB = as.integer(IBB), 
                            HBP = as.integer(HBP),
                            SH = as.integer(SH),
                            SF = as.integer(SF),
                            GIDP = as.integer(GIDP))

select(hitting_qualified, playerID, yearID, AB, IBB, HBP, SH, SF, GIDP)
hitting_qualified <- replace_na(hitting_qualified, list(IBB = 0, HBP = 0, SH = 0, SF = 0, GIDP = 0, CS = 0, SB = 0, SO = 0))
hitting_qualified <- mutate(hitting_qualified, X1B = H - X2B - X3B - HR)
hitting_qualified <- mutate(hitting_qualified, uBB = BB - IBB)

hitting_qualified <- mutate(hitting_qualified, BBP = uBB/PA, 
                            KP = SO/PA, 
                            OBP = (H + BB + HBP) / PA, 
                            SLG = (X1B + 2 * X2B + 3 * X3B + 4 * HR) / AB, 
                            OPS = OBP + SLG, 
                            wOBA = (0.688 * uBB 
                                    + 0.719 * HBP 
                                    + .880	* X1B 
                                    + 1.250 * X2B
                                    + 1.583 * X3B
                                    + 2.042 * HR) / (PA - IBB))
hitting_qualified <- mutate(hitting_qualified, 
                            wOBA_rating = case_when(
                              wOBA < 0.310 ~ "Bad",
                              wOBA >= 0.310 & wOBA < 0.340 ~ "Average",
                              wOBA >= 0.340 & wOBA < 0.370 ~ "Above Average",
                              wOBA >= 0.370 & wOBA < 0.4 ~ "Great",
                              wOBA >= 0.4 ~ "Elite"
                            ),
                            OBP_rating = case_when(
                              OBP < 0.310 ~ "Bad",
                              OBP >= 0.310 & OBP < 0.340 ~ "Average",
                              OBP >= 0.340 & OBP < 0.370 ~ "Above Average",
                              OBP >= 0.370 & OBP < 0.390 ~ "Great",
                              OBP >= 0.390 ~ "Elite"
                            ),
                            OPS_rating = case_when(
                              OPS < 0.670 ~ "Bad",
                              OPS >= 0.670 & OPS < 0.8 ~ "Average",
                              OPS >= 0.8 & OPS < 0.9 ~ "Above Average",
                              OPS >= 0.9 & OPS < 1.0 ~ "Great",
                              OPS >= 1.0 ~ "Elite"
                            )
                            )

hitting_qualified <- mutate(hitting_qualified, 
                            KP_rating = case_when(
                              KP > 0.220 ~ "Bad",
                              KP <= 0.220 & KP > 0.160 ~ "Average",
                              KP <= 0.160 & KP > 0.125 ~ "Above Average",
                              KP <= 0.125 & KP > 0.1 ~ "Great",
                              KP <= 0.1 ~ "Elite"
                            ),
                            BBP_rating = case_when(
                              BBP < 0.07 ~ "Bad",
                              BBP > 0.07 & BBP < 0.1 ~ "Average",
                              BBP >= 0.1 & BBP < 0.125 ~ "Above Average",
                              BBP >= 0.125 & BBP < 0.150 ~ "Great",
                              BBP >= 0.150 ~ "Elite"
                            )
                            )

tmp_batting <- filter(hitting_qualified, yearID %in% 2000:2015)
batting_recent <- select(tmp_batting, 
                         playerID, 
                         yearID, 
                         teamID, 
                         lgID,
                         PA,
                         X1B, 
                         uBB, 
                         BBP, 
                         KP, 
                         OBP, 
                         OPS, 
                         wOBA, 
                         BBP_rating, 
                         KP_rating, 
                         wOBA_rating, 
                         OPS_rating, 
                         OBP_rating)

#correlation between walking and striking out? fairly linear
ggplot(data = batting_recent) + geom_point(mapping = aes(x=BBP, y=KP), alpha=0.2)

ggplot(data = batting_recent) + geom_histogram(mapping = aes(x=wOBA), binwidth = 0.01)

ggplot(data = batting_recent) + geom_point(mapping = aes(x=wOBA, y=OPS), alpha=0.04)

ggplot(data = batting_recent) + geom_bar(mapping = aes(x=ordered(wOBA_rating, levels = c("Bad","Average","Above Average","Great","Elite"))))
