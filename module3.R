raw_shooting <- read_csv(file = "data/nba_shooting.csv")
arrange(raw_shooting, desc(FGA))
filter(raw_shooting, PLAYER == "Kobe Bryant")
filter(raw_shooting, FGA > 100)
filter(raw_shooting, FGA >= 100 | TPA >= 50)
filter(raw_shooting, SEASON %in% c(1999, 1980, 1984))
# if you want all data EXCEPT for a specific season(s)...
filter(raw_shooting, !SEASON %in% c(1999, 2000))
nba_shooting_orig <- filter(raw_shooting, 
                            FGA >= 100 & FTA >= 100 & TPA >= 50 & !SEASON %in% c(1999, 2012))
mutate(nba_shooting_orig, eFGP = (FGM + 0.5 * TPM) / FGA)
full_nba_shooting <- mutate(nba_shooting_orig, eFGP = (FGM + 0.5 * TPM)/FGA)
full_nba_shooting2 <- mutate(full_nba_shooting, PTS = (FTM + 2 * FGM + TPM))
full_nba_shooting3 <- mutate(full_nba_shooting2, TSP = (PTS / (2 * FGA + 0.44 * FTA)))
nba_shooting <- mutate(full_nba_shooting3, 
                       eFGP = (FGM + 0.5 * TPM)/FGA,
                       PTS = (FTM + 2 * FGM + TPM),
                       TSP = (PTS / (2 * FGA + 0.44 * FTA))
                       )

nba_shooting <- mutate(nba_shooting, 
                       Three_ability = case_when(
                         TPP < 0.1 ~ "Hopeless",
                         0.2 <= TPP & TPP < 0.3 ~ "Below Average",
                         0.3 <= TPP & TPP < 0.35 ~  "Average",
                         0.35 <= TPP & TPP < 0.4 ~ "Roco",
                         0.4 <= TPP ~ "Hollis"
                       ))


nba_shooting_2016 <- filter(nba_shooting, SEASON == 2016)
summarize(nba_shooting_2016, FGP = mean(TPP))
summarize(nba_shooting_2016, FGP = mean(FGP), TPP = mean(TPP), FTP = mean(FTP))
# limit columns of tbl
select(nba_shooting, PLAYER, SEASON, FGP, TPP, FTP, eFGP, PTS, TSP)
# save data as .RData file
save(nba_shooting, file = "data/nba_shooting.RData")
# load data back into R
load("data/nba_shooting.RData")



