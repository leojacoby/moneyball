fgm <- c(805, 572, 470, 710, 698, 737, 498, 373, 215, 552)
fga <- c(1597, 1349, 1034, 1617, 1381, 1416, 1112, 886, 442, 1061)
tpm <- c(402, 115, 64, 236, 186, 87, 126, 81, 0, 2)
tpa <- c(887, 327, 206, 657, 480, 282, 342, 243, 2, 6)
ftm <- c(363, 272, 395, 720, 447, 359, 250, 201, 92, 208)
fta <- c(400, 344, 475, 837, 498, 491, 280, 240, 131, 586)
efg <- (fgm + 0.5 * tpm) / fga
names(efg) <- players
shooting_data <- tibble(
    Player = players, 
    Position = c("PG", "PG", "SG", "SG", "SF", "SF", "PF", "PF", "C", "C"), 
    FGM = fgm, 
    FGA = fga, 
    FTM = ftm, 
    FTA = fta, 
    TPM = tpm, 
    TPA = tpa
)

nba_shooting <- read_csv(file = "Data/nba_shooting.csv")
ggplot(data = nba_shooting) + geom_histogram(mapping = aes(x = FGP), binwidth = 0.01)
ggplot(data = filter(nba_shooting, FTA > 40 & TPA > 10)) + geom_point(mapping = aes(x = TPP, y = FTP), alpha = 0.07)
ggplot(data = filter(nba_shooting, FTA > 50 & TPA > 50)) + geom_hex(mapping = aes(x = TPP, y = FTP), bins = 50)
ggplot(data = nba_shooting) + geom_bin2d(aes(x = FGA, y = FGP)) + xlim(50, 2000) + ylim (0.25, 0.75)
