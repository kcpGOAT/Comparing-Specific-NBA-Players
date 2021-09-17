# This program attempts to predict the TS% of a player for the 2020-2021 NBA season based on a linear model. 
library(tidyverse)
names(nba_data_historical)[15] <- "TS"

model2021_TS <- function(player) {
  # This creates a dataset that only includes regular season TS along with name and years
  player_data <- nba_data_historical %>%
    filter(type == "RS", name_common == player) %>%
    select(name_common, year_id, TS) %>%
    arrange(year_id)
  # This creates a linear model between year and TS%
  player_regression <- lm(TS ~ year_id, data = player_data)
  # This adds the TS% from 2020 to the slope from the regression, and then combines that result to a percentage sign. 
  paste(round(player_data$TS[player_data$year_id == 2020] 
        + unname(coef(player_regression)[2]), digits = 1), "%", sep = "")
}

model2021_TS("Trae Young")
model2021_TS("Luka Doncic")
model2021_TS("Giannis Antetokounmpo")

# > model2021_TS("Trae Young")
# [1] "65.1%"
# > model2021_TS("Luka Doncic")
# [1] "62.5%"
# > model2021_TS("Giannis Antetokounmpo")
# [1] "63.1%"
