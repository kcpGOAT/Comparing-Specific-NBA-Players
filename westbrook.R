library(tidyverse)
library(ggthemes)
  
westbrook_data <- nba_data_historical %>%
  filter(name_common == "Russell Westbrook", type == "RS") %>%
  select(year_id, TS) %>%
  add_row(year_id = 2021, TS = 50.9, .before = 1) %>%
  arrange(year_id)


ggplot(data = westbrook_data, aes(year_id, TS)) +
  geom_bar(aes(fill = year_id > 2017),
          stat = "identity",
          alpha = 0.5) + 
  labs(x = "Year", y = "",
       title = "Russell Westbrook's True Shooting Percentage Over Time",
       caption = "Source: FiveThirtyEight, StatMuse") +
  coord_cartesian(ylim = c(48, 56)) + 
  scale_fill_discrete(name = "", 
                      labels = c("Pre-MVP", "Post-MVP")) +
  scale_x_continuous(breaks = seq(2009, 2021, by = 2)) +
  theme_light() +
  theme(panel.grid = element_blank(), 
        text = element_text(face = "bold"), 
        plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(vjust = -2))

  
ggplot(westbrook_data, aes(year_id, TS)) +
  geom_line(color = "blue") +
  theme_stata()


