# This program attempts to predict the TS% of a player for the 2020-2021
# NBA season based on a linear model. 
library(tidyverse)

model2021_TS <- function(player) {
  player_data <- nba_data_historical %>%
    filter(type == "RS", name_common == player) %>%
    select(name_common, year_id, TS) %>%
    arrange(year_id)
  player_regression <- lm(TS ~ year_id, data = player_data)
  round(player_data$TS[player_data$year_id == 2020] + unname(coef(player_regression)[2]), digits = 2)
}

model2021_TS("Stephen Curry")
model2021_TS("LeBron James")
model2021_TS("Giannis Antetokounmpo")

# > model2021_TS("Stephen Curry")
# [1] 56.13
# > model2021_TS("LeBron James")
# [1] 58.14
# > model2021_TS("Giannis Antetokounmpo")
# [1] 63.09
