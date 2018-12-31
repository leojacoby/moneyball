library(tidyverse)
diving <- read_csv("data/diving.csv")

hist <- ggplot(data = diving)
hist <- hist + geom_histogram(aes(x = JScore), binwidth = 0.25)
hist <- hist + labs(x = "Judge Score")
hist <- hist + facet_wrap(~ JCountry, nrow = 3)
median_score <- median(diving[["JScore"]])
hist <- 
  hist + geom_vline(xintercept = median_score, 
                    color = "red")

box <- ggplot(data = diving)
box <- box + geom_boxplot(aes(x = Round, y = JScore, fill = Round))
box <- 
  box + labs(title = "Diving scores by round", 
             x = "", 
             y = "Judge Score")

# didn't work...put y text  back in
box <- 
  box + theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank())

bar <- ggplot(data = diving)
bar <- bar + geom_bar(aes(x = JCountry, fill = JCountry))
bar <- bar + labs(x = "Judge Country", fill = "Judge Country")

scatter <- ggplot(data = diving)
scatter <- scatter + geom_point(aes(x = Rank, y = JScore, color = Country))
scatter <- scatter + geom_abline(intercept = 8.1, slope = -0.1, color = "red")

#explain how lines 1 and 2 work
judges <- ggplot(data = diving, aes(x = Judge, y = JScore))
judges <- judges + stat_summary(fun.data = mean_se)
judges <- judges + labs(y = "Judge Score")
judges <- judges + coord_flip()


scatter <- ggplot(data = diving)
scatter <- scatter + geom_point(aes(x = Rank, 
                                    y = JScore, 
                                    color = Difficulty))
scatter <- scatter + scale_color_distiller(palette = "OrRd", direction = 1)


hist2d <- ggplot(data = diving)
hist2d <- hist2d + geom_bin2d(aes(x = Rank, y = Difficulty))
hist2d <- hist2d + scale_fill_distiller(palette = "Spectral")






