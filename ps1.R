wins <- c(103, 95, 95, 94, 93, 91, 89, 89, 87, 87, 86, 86, 86, 84, 84, 81, 79, 78, 78, 75, 74, 73, 71, 69, 69, 68, 68, 68, 68, 59)
losses <- 162 - wins
rs <- c(808, 765, 763, 777, 878, 725, 744, 759, 671, 715, 750, 779, 768, 680, 724, 675, 655, 729, 686, 845, 717, 671, 610, 752, 653, 649, 672, 686, 716, 722)
ra <- c(556, 757, 612, 676, 694, 638, 715, 666, 617, 631, 721, 712, 707, 702, 701, 712, 682, 758, 715, 860, 727, 733, 796, 890, 761, 779)
teams <- c("Cubs", "Rangers", "Nationals", "Indians", "Red Sox", "Dodgers", "Orioles", "Blue Jays", "Mets", "Giants", "Tigers", "Cardinals", "Mariners", "Yankees", "Astros", "Royals", "Marlins", "Pirates", "White Sox", "Rockies", "Angels", "Brewers", "Phillies", "Diamondbacks", "Athletics", "Braves", "Rays", "Padres", "Reds", "Twins")
winp <- wins / 162
james <- rs^2 / (rs^2 + ra^2)
plot(winp, james, xlab="Winning Percentage", ylab="Jamesian Algorithim")

data <- cbind(james, winp)
dataframe <- as.data.frame(data)
dataframe$teams <- rownames(dataframe)

ggplot(data = dataframe,
       aes(x=winp, y = james)) +
  #geom_point() +
  geom_smooth(method = "lm", se = F) +
  geom_text(aes(label = teams))


wp.resid <- winp - james
names(wp.resid) <- teams
sort(wp.resid)