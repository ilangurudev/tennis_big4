library(tidyverse)
library(magrittr)
library(purrr)
library(stringr)
library(forcats)



df_tennis <- read_csv("./data/atp_matches_all.csv")

nplayers <- 2
years_to_aggregate <- 10
y_now <- 2017
y_beg <- 1920
cuts <- seq(y_now,y_beg,by = -years_to_aggregate)

df_tennis <- 
  df_tennis %>%
    mutate(tourney_name = case_when(
                        tourney_name == "Australian Chps." ~ "Australian Open",
                        tourney_name == "Us Open" ~ "US Open",
                        TRUE ~ tourney_name
                      ),
           year = as.integer(str_sub(tourney_date,1,4)),
           year_aggregated = cut(year, breaks = cuts, dig.lab = 10)
           ) 

unique(cut(df_tennis$year, breaks = cuts, dig.lab = 10, include.lowest = T))


grand_slam_wins <- df_tennis %>%
  filter(tourney_level == "G" & round == "F") 

grand_slam_wins %>%
    ggplot(aes(fct_rev(fct_infreq(winner_name, ordered = T)), fill = tourney_name)) +
      geom_bar() +
      coord_flip() +
      scale_y_continuous(breaks = seq(1,20,1)) +
      scale_fill_manual(values = c("darkgoldenrod2", "coral3", "blue3", "chartreuse4")) +
      theme_minimal() +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank())

topn_lookup <- 
  grand_slam_wins %>%
    group_by(year_aggregated, winner_name) %>%
    mutate(num_won = n()) %>%
    ungroup() %>%
    group_by(year_aggregated) %>% 
    select(year_aggregated,winner_name, num_won) %>%
    unique() %>% 
    mutate(top_nplayers = if_else(num_won >= sort(num_won, partial = n() - nplayers -1)[n() - nplayers -1], T, F)) 

 
grand_slam_wins %>%
    left_join(topn_lookup) %>%
      ggplot(aes(x = year_aggregated, fill = top_nplayers)) + 
          geom_bar(position = "fill") +
      scale_y_continuous(breaks = seq(0,1,.25), labels = 20*seq(0,1,.25))



df_tennis %>%
  filter(year > 2002) %>% 
  group_by(winner_name) %>%
    mutate(matches_won = n()) %>%
      ungroup() %>%
        filter(matches_won > 700) %>%
  group_by(year, winner_name) %>% 
  mutate(matches_won = n()) %>%
    ggplot(aes(year, matches_won, col = winner_name)) + 
  geom_line(lwd = 1.5)
  