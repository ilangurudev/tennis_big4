library(tidyverse)
library(magrittr)
library(purrr)
library(stringr)
library(forcats)
library(plotly)



df_tennis <- read_csv("./data/atp_matches_all.csv")

nplayers <- 4
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
    mutate(top_nplayers = if_else(num_won >= sort(num_won, partial = n() - nplayers + 1)[n() - nplayers + 1], T, F)) 

 

grand_slam_wins %>%
    left_join(topn_lookup) %>%
      ggplot(aes(x = year_aggregated, fill = top_nplayers)) + 
          geom_bar(position = "fill") +
      scale_y_continuous(breaks = seq(0,1,.25), labels = 20*seq(0,1,.25))



total_matches_won <- 
  df_tennis %>%
    filter(year > 2002 & year < 2017) %>% 
    group_by(winner_name) %>%
      mutate(matches_won = n()) %>%
        ungroup()

# filter(matches_won <= 700) %>%

others <- 
  total_matches_won %>%
    group_by(year, winner_name) %>% 
    mutate(matches_won = n()) %>% 
    ungroup() %>% 
    group_by(year) %>% 
    summarise(matches_won = mean(matches_won)) %>% 
    mutate(winner_name = "Rest")

total_matches_won %>%
        filter(matches_won > 700) %>%
  group_by(year, winner_name) %>% 
  mutate(matches_won = n()) %>%
    ggplot(aes(year, matches_won)) + 
  geom_line(aes(col = winner_name), lwd = 1.5) + 
  theme_minimal() +
  geom_line(data = others, aes(year, matches_won, col = "Other Players"), lwd = 1.5) +
  scale_colour_manual(values = c("Roger Federer" = "chartreuse4", "Rafael Nadal" = "coral3", 
                                 "Novak Djokovic" = "blue3", "Other Players" = "grey"))
  



p <- 
df_tennis %>%
  gather(result,player, winner_name,loser_name) %>%
  mutate(result = str_extract(result, "[a-z]")) %>%
  filter(player %in% c("Roger Federer", "Novak Djokovic", "Rafael Nadal")) %>%
  group_by(player) %>%
  arrange(tourney_date, match_num) %>%
  mutate(match_num_i = row_number(),
         running_win_mean = cummean(result == "w")) %>%
  ggplot(aes(x = match_num_i, y = running_win_mean, frame = year)) +
  geom_path(lwd = 1, aes(group = player, col = player, cumulative = TRUE)) +
  labs(x = "Match Number", 
       y = "Running match win percent", 
       title = "Matches Vs Win Percent") +
  xlim(c(20, 1200)) +
  scale_y_continuous(labels = scales::percent) +                   
  theme_minimal()


gganimate::gganimate(p,"output.gif")



p <- 
  grand_slam_wins %>%
  arrange(year) %>% 
  group_by(winner_name) %>%
  mutate(n = n()) %>%
  filter(n>3) %>% 
  mutate(slam = 1,
         slams_until = cumsum(slam)) %>% 
  ggplot(aes(x = year, y = slams_until)) +
  geom_path(lwd = 1, aes(group = winner_name, col = winner_name, cumulative = TRUE)) +
  scale_x_continuous(breaks = seq(1967,2017,5), labels = seq(1967,2017,5)) +
  scale_y_continuous(breaks = seq(1,20,3), labels = seq(1,20,3)) +
  labs(x = "Year", 
       y = "Cumulative number of slams won") +
  theme_minimal()

pl <- plotly::ggplotly(p)
# plotly_POST(pl)

htmlwidgets::saveWidget(as_widget(pl), "chart.html")

gganimate::gganimate(p)
