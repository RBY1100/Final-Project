knitr::opts_chunk$set(error = TRUE)
library(tidyverse)
library(rvest)
library(scales)

#Scrape Data from each Server and join them into one dataset
wot_tableeu <-"https://wot-news.com/stat/server/eu/norm/en/" %>%
  read_html() %>%
  html_nodes("table#stat_veh_all4")  %>% 
  html_table() %>% 
  .[[1]] %>%
  rename("Total Played EU" = `Total played`, "Wins EU" = `Win`, "Win% EU" = `Win %`, "Unique Players EU" = `Vehicles amount`)

wot_tableus <-"https://wot-news.com/stat/server/us/norm/en/" %>%
  read_html() %>%
  html_nodes("table#stat_veh_all4")  %>% 
  html_table() %>% 
  .[[1]] %>%
  rename("Total Played US" = `Total played`, "Wins US" = `Win`, "Win% US" = `Win %`, "Unique Players US" = `Vehicles amount`)

wot_tablesea <-"https://wot-news.com/stat/server/sea/norm/en/" %>%
  read_html() %>%
  html_nodes("table#stat_veh_all4")  %>% 
  html_table() %>% 
  .[[1]] %>%
  rename("Total Played SEA" = `Total played`, "Wins SEA" = `Win`, "Win% SEA" = `Win %`, "Unique Players SEA" = `Vehicles amount`)

wot_tableru <-"https://wot-news.com/stat/server/ru/norm/en/" %>%
  read_html() %>%
  html_nodes("table#stat_veh_all4")  %>% 
  html_table() %>% 
  .[[1]] %>%
  rename("Total Played RU" = `Total played`, "Wins RU" = `Win`, "Win% RU" = `Win %`, "Unique Players RU" = `Vehicles amount`)

wot_tablesearu <- inner_join(wot_tablesea, wot_tableru, by = "Name")
wot_tablesuseu <- inner_join(wot_tableus, wot_tableeu, by = "Name")
tank_stats <- inner_join(wot_tablesuseu, wot_tablesearu, by = "Name")

#Read in Premium Information and merge it with main dataset
other_info <- read_csv("~/STA 518/Final-Project/tank_stats.csv")
tank_stats <- tank_stats %>% inner_join(other_info, by = "Name")

#Remove Unnessesary Variables and Rename Remaining ones
tank_stats <- tank_stats %>%
  select(-Tier.y.x, -Tier.x.y, -Tier.y.y, -Nation.y.x, -Nation.x.y, -Nation.y.y, -Type.y.x, -Type.x.y, -Type.y.y, -`Total Played SEA.y`) %>%
  rename("Tier" = Tier.x.x, "Nation" = Nation.x.x, "Type" = Type.x.x, "Total Played SEA" = `Total Played SEA.x`)

#Get Values for the total World of Tanks player base by adding up all the servers
tank_stats <- tank_stats %>%
  mutate("Total Played" = `Total Played US` + `Total Played EU` + `Total Played SEA` + `Total Played RU`, "Total Wins" = `Wins US` + `Wins EU` + `Wins SEA` + `Wins RU`, "Total Unique Players" = 
           `Unique Players US` + `Unique Players EU` + `Unique Players SEA` + `Unique Players RU`, "Win Rate %" = (`Total Wins`/`Total Played`)*100)

#Filter out Repeat Values and Low Observation Values (tanks in beta testing)
tank_stats <- tank_stats[!duplicated(tank_stats$Name),] %>%
  filter(`Total Played` > 6000)

#Convert "Tier" and "Premium" into a character values and order them 
tank_stats$`Tier` <- as.character(tank_stats$`Tier`) %>%
  factor(levels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
tank_stats$`Premium` <- as.character(tank_stats$`Premium`)

#Rename Premium
tank_stats <- tank_stats%>%
  mutate(Type = fct_recode(Type,
                           "Medium Tank" = "Medium Tanks",
                           "Heavy Tank" = "Heavy Tanks",
                           "Light Tank" = "Light Tanks",
                           "Tank Destroyer" = "TD",
                           "Artillery" = "SPG")) %>%
  
  mutate(Nation = fct_recode(Nation,
                             "United States" = "Usa",
                             "USSR" = "Ussr",
                             "United Kingdom" = "UK")) %>%
  
  mutate(Premium = fct_recode(Premium,
                              "No" = "0",
                              "Yes" = "1"))

#View Data
print(tank_stats)




#Some Basic Plots for Data

#Damage Based on Tier
tank_stats %>%
  ggplot(mapping=aes(x=`Win Rate %`, group=Tier, color=Tier)) +
  geom_boxplot()

#Winrate by Premium
tank_stats %>%
  ggplot(mapping=aes(x=`Win Rate %`,group=Premium, color=Premium)) +
  geom_boxplot()

#Winrate by Nation
tank_stats %>%
  ggplot(mapping=aes(x=`Win Rate %`,group=Nation, color=Nation)) +
  geom_boxplot()
