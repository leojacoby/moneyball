library(tidyverse)
pitches <- read_csv("data/pitchfx_2015.csv")
called_pitches <- pitches %>% 
  filter(Description == "Called Strike" | Description == "Ball") %>%
  mutate(Call = case_when(
    Description == "Called Strike" ~ 1,
    Description == "Ball" ~ 0
  ))

zone <- ggplot(data = called_pitches)
#stat summary...?
zone <- zone + stat_summary_2d(mapping = aes(x = X, y = Z, z = Call))
zone <- zone + scale_fill_distiller("P(Called Strike)", palette = "RdBu")
zone <-
  zone + annotate("rect",
                  xmin = -8.5,
                  xmax = 8.5,
                  ymin = 19,
                  ymax = 41.5,
                  alpha = 0,
                  color = "black")
zone <- zone + theme_classic()
zone <- zone + theme(axis.title.x = element_blank())
zone <- zone + theme(axis.title.y = element_blank())
zone <- zone + labs(title = "Estimated Strike Zone")


# *******************************************

raw_boxscore <- read_csv("data/nba_boxscore.csv")
table(raw_boxscore[["TM"]])
team_boxscore <- 
  raw_boxscore %>% 
  filter(Tm != "TOT") %>% 
  group_by(Season, Tm) %>% 
  summarize(FGA = sum(FGA),
          FGM = sum(FGM),
          TPA = sum(TPA),
          TPM = sum(TPM),
          FTA = sum(FTA),
          FTM = sum(FTM),
          PTS = sum(PTS)) %>% 
  mutate(FGP = FGM / FGA,
         TPP = TPM / TPA,
         FTP = FTM / FTA) %>% 
  ungroup()

reduced_boxscore <- 
  team_boxscore %>% 
  filter(Tm %in% c("BOS", "CLE", "DAL", "DET", "GSW", "LAL", "MIA"))

ggplot(data = reduced_boxscore) +
  geom_point(mapping = aes(x = Season, y = TPA, color = Tm)) +
  geom_line(mapping = aes(x = Season, y = TPA, color = Tm)) +
  labs(y = "Three Point Attempts", title = "Three Point Attempts by Team over Time") +
  NULL



