# Top 20 football players
# Kaggle Kernel 
# author: "Eryk Walczak"
# date: "13 July 2016"

# This Version's Code by Grayson VanderLinde

# Required Packages
install.packages("dplyr")
install.packages("RSQLite")
install.packages("DBI")
install.packages("magrittr")
install.packages("DT")
install.packages("DescTools")
install.packages("qtlcharts")
install.packages("ggvis")
install.packages("radarchart")
install.packages("tidyr")

library(dplyr)
library(RSQLite)
library(DBI)
library(magrittr)
library(DT)
library(DescTools)
library(qtlcharts)
library(ggvis)
library(radarchart)
library(tidyr)

# Loading Data - Data is stored in SQLite file, must connect to the data base.
con <- dbConnect(SQLite(), dbname="database.sqlite")
dbListTables(con)

player <- tbl_df(dbGetQuery(con,"SELECT * FROM player"))
player_stats <- tbl_df(dbGetQuery(con,"SELECT * FROM player_stats"))

player_stats <-  player_stats %>%
  rename(player_stats_id = id) %>%
  left_join(player, by = "player_api_id")

latest_ps <- 
  player_stats %>% 
  group_by(player_api_id) %>% 
  top_n(n = 1, wt = date_stat) %>%
  as.data.frame()

top3 <- 
  latest_ps %>% 
  arrange(desc(overall_rating)) %>% 
  head(n = 3) %>%
  as.data.frame()

# Explore Data 
str(player_stats)

top3 %>% 
  select(player_name, birthday, height, weight, preferred_foot, overall_rating) %>% 
  datatable(., options = list(pageLength = 10))

# Charts
Desc(top3$overall_rating, plotit = TRUE)

iplotCorr(top3[,10:42], reorder=TRUE)

measures <- names(top3[,10:42])

top3 %>% 
  ggvis(x = input_select(measures, label = "Choose the x-axis:", map = as.name)) %>% 
  layer_points(y = ~overall_rating, fill = ~player_name)

radarDF <- top3 %>% select(player_name, 10:42) %>% as.data.frame()

radarDF <- gather(radarDF, key=Label, value=Score, -player_name) %>%
  spread(key=player_name, value=Score)

chartJSRadar(scores = radarDF, maxScale = 100, showToolTipLabel = TRUE)
