knitr::opts_chunk$set(error = TRUE)
library(tidyverse)
library(rvest)
library(scales)
library(purrr)


#Writes a function to scrape data from the different servers
wotscrape <- function(x,y){
  tableda <- x %>%
    read_html() %>%
    html_nodes("table#stat_veh_all4")  %>% 
    html_table() %>% 
    .[[1]] %>%
    setNames(c('Name','Tier','Type','Nation',paste('Total Played',y, sep=" "),paste('Wins',y, sep=" "), paste('Win %',y, sep=" "),paste('Unique Players',y, sep=" "), 'Region'))
  }

wotscrape2 <- function(x,y){
  tableda <- x %>%
    read_html() %>%
    html_nodes("table#stat_veh_all4")  %>% 
    html_table() %>% 
    .[[1]] %>%
    mutate(region=y)
}

wot_tableeu2 <- wotscrape2("https://wot-news.com/stat/server/eu/norm/en/", "EU")
wot_tableus2 <- wotscrape2("https://wot-news.com/stat/server/us/norm/en/", "US") 
wot_tableru2 <- wotscrape2("https://wot-news.com/stat/server/ru/norm/en/", "RU") 
wot_tablesea2 <- wotscrape2("https://wot-news.com/stat/server/sea/norm/en/", "SEA") 

wot_list2 <- list(wot_tableeu2, wot_tableus2, wot_tableru2, wot_tablesea2)
test <- rbind(wot_list2)







wot_tableeu <- wotscrape("https://wot-news.com/stat/server/eu/norm/en/", "EU")
wot_tableus <- wotscrape("https://wot-news.com/stat/server/us/norm/en/", "US") 
wot_tableru <- wotscrape("https://wot-news.com/stat/server/ru/norm/en/", "RU") 
wot_tablesea <- wotscrape("https://wot-news.com/stat/server/sea/norm/en/", "SEA") 
other_info <- read_csv("~/STA 518/Final-Project/tank_stats.csv") %>%
  select("Name", "Premium")

wot_list <- list(wot_tableeu, wot_tableus, wot_tableru, wot_tablesea, other_info)

#Join the datasets together

tank_stats <- wot_list %>% reduce(inner_join, by="Name")

#Remove Unnessesary Variables and Rename Remaining ones
tank_stats <- tank_stats %>%
  select(-ends_with(".y"),-ends_with("x.x")) %>%
  rename("Tier"=`Tier.x`, "Nation"=`Nation.x`, "Type"=`Type.x`)


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

#Winrate by Type
tank_stats %>%
  ggplot(mapping=aes(x=`Win Rate %`,group=Type, color=Type)) +
  geom_boxplot()

#Winrate by Region
tank_stats %>%
  ggplot(mapping=aes(x=`Total Played EU`)) +
  geom_boxplot()


