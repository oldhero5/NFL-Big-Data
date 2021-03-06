file.tracking <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/tracking_gameId_2017090700.csv"
tracking.example <- read_csv(file.tracking)

file.game <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/games.csv"
games.sum <- read_csv(file.game) 

file.players <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/players.csv"
players.sum <- read_csv(file.players) 

file.plays <- "https://raw.githubusercontent.com/nfl-football-ops/Big-Data-Bowl/master/Data/plays.csv"
plays.sum <- read_csv(file.plays) 

dat1 <- select(players.sum,nflId,FirstName,LastName,PositionAbbr,Height,Weight)

tracking.example.merged <- tracking.example %>% inner_join(games.sum) %>% inner_join(plays.sum) 

example.play <- tracking.example.merged %>% filter(playId == 2756)

example.play %>% select(playDescription) %>% slice(1)

#animate a play

library(gganimate)
library(cowplot)
library(gifski)
library(png)


## General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3


## Specific boundaries for a given play
ymin <- max(round(min(example.play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example.play$x, na.rm = TRUE) + 10, -1), 120)
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

animate.play <- ggplot() +
    geom_point(data = example.play, aes(x = (xmax-y), y = x, 
                                        colour = team, group = nflId, pch = team, size = team)) + 
    geom_text(data = example.play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
              vjust = 0.36, size = 3.5) + 
    scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
    scale_shape_manual(values = c(19, 16, 19), guide = FALSE) +
    scale_colour_manual(values = c("#e31837", "#654321", "#002244"), guide = FALSE) + 
    annotate("text", x = df.hash$x[df.hash$x < 55/2], 
             y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
    annotate("text", x = df.hash$x[df.hash$x > 55/2], 
             y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) + 
    annotate("segment", x = xmin, 
             y = seq(max(10, ymin), min(ymax, 110), by = 5), 
             xend =  xmax, 
             yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
    annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
             label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
             angle = 270, size = 4) + 
    annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
             label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
             angle = 90, size = 4) + 
    annotate("segment", x = c(xmin, xmin, xmax, xmax), 
             y = c(ymin, ymax, ymax, ymin), 
             xend = c(xmin, xmax, xmax, xmin), 
             yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
    ylim(ymin, ymax) + 
    coord_fixed() +  
    theme_nothing() + 
    transition_time(frame.id)  +
    ease_aes('linear') + 
    NULL

## Ensure timing of play matches 10 frames-per-second
play.length.ex <- length(unique(example.play$frame.id))
animate(animate.play, fps = 10, nframe = play.length.ex)