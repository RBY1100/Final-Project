knitr::opts_chunk$set(error = TRUE)
library(tidyverse)
library(rvest)
library(scales)
library(purrr)
library(stats)

#Writes a function to scrape data from the different servers
wotscrapewr <- function(x,y){
  tableda <- x %>%
    read_html() %>%
    html_nodes("table#stat_veh_all4")  %>% 
    html_table() %>% 
    .[[1]] %>%
    setNames(c('Name','Tier','Type','Nation',paste('Total Played',y, sep=" "),paste('Wins',y, sep=" "), paste('Win %',y, sep=" "),paste('Unique Players',y, sep=" "), 'Region'))
  }

wotscrapereg <- function(x,y){
  tableda <- x %>%
    read_html() %>%
    html_nodes("table#stat_veh_all4")  %>% 
    html_table() %>% 
    .[[1]] %>%
    mutate(region=y)
}


#Creates datasets and join together for analyzing winrates across the board
wot_tableeu <- wotscrapewr("https://wot-news.com/stat/server/eu/norm/en/", "EU")
wot_tableus <- wotscrapewr("https://wot-news.com/stat/server/us/norm/en/", "US") 
wot_tableru <- wotscrapewr("https://wot-news.com/stat/server/ru/norm/en/", "RU") 
wot_tablesea <- wotscrapewr("https://wot-news.com/stat/server/sea/norm/en/", "SEA") 
other_info <- read_csv("~/STA 518/Final-Project/tank_stats.csv") %>%
  select("Name", "Premium")

wot_list <- list(wot_tableeu, wot_tableus, wot_tableru, wot_tablesea, other_info)
tank_statstot <- wot_list %>% reduce(inner_join, by="Name")


#Creates datasets and join together for analyzing differences in servers
wot_tableeu2 <- wotscrapereg("https://wot-news.com/stat/server/eu/norm/en/", "EU")
wot_tableus2 <- wotscrapereg("https://wot-news.com/stat/server/us/norm/en/", "US") 
wot_tableru2 <- wotscrapereg("https://wot-news.com/stat/server/ru/norm/en/", "RU") 
wot_tablesea2 <- wotscrapereg("https://wot-news.com/stat/server/sea/norm/en/", "SEA") 

wot_list2 <- list(wot_tableeu2, wot_tableus2, wot_tableru2, wot_tablesea2)
tank_statsreg <- do.call(rbind, wot_list2)


#Remove Unnessesary Variables and Rename Remaining ones
tank_statstot <- tank_statstot %>%
  select(-ends_with(".y"),-ends_with("x.x")) %>%
  rename("Tier"=`Tier.x`, "Nation"=`Nation.x`, "Type"=`Type.x`)


#Get Values for the total World of Tanks player base by adding up all the servers
tank_statstot <- tank_statstot %>%
  mutate("Total Played" = `Total Played US` + `Total Played EU` + `Total Played SEA` + `Total Played RU`, "Total Wins" = `Wins US` + `Wins EU` + `Wins SEA` + `Wins RU`, "Total Unique Players" = 
           `Unique Players US` + `Unique Players EU` + `Unique Players SEA` + `Unique Players RU`, "Win Rate %" = (`Total Wins`/`Total Played`)*100)

#Filter out Repeat Values and Low Observation Values (tanks in beta testing)
tank_statstot <- tank_statstot[!duplicated(tank_statstot$Name),] %>%
  filter(`Total Played` > 6000)

#Convert "Tier" and "Premium" into a character values and order them 
tank_statstot$`Tier` <- as.character(tank_statstot$`Tier`) %>%
  factor(levels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))
tank_statstot$`Premium` <- as.character(tank_statstot$`Premium`)

#Rename Premium
tank_statstot <- tank_statstot%>%
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
print(tank_statstot)




#Some Basic Plots for Data

#Damage Based on Tier
tank_statstot %>%
  ggplot(mapping=aes(x=`Win Rate %`, group=Tier, color=Tier)) +
  geom_boxplot()

tank_statstot %>%
  group_by(Tier) %>%
  summarise("Mean Percent by Tank Type" = mean(`Win Rate %`), "Total Number of Tier" = length(`Win Rate %`))

aov(`Win Rate %` ~ Tier, data = tank_statstot) %>%
  summary()

#Winrate by Premium
tank_statstot %>%
  ggplot(mapping=aes(x=`Win Rate %`,group=Premium, color=Premium)) +
  geom_boxplot()

tank_statstot %>%
  group_by(Premium) %>%
  summarise("Mean Percent by Tank Type" = mean(`Win Rate %`), "Total Number of Premium" = length(`Win Rate %`))

aov(`Win Rate %` ~ Premium, data = tank_statstot) %>%
  summary()

#Winrate by Nation
tank_statstot %>%
  ggplot(mapping=aes(x=`Win Rate %`,group=Nation, color=Nation)) +
  geom_boxplot()

tank_statstot %>%
  group_by(Nation) %>%
  summarise("Mean Percent by Tank Type" = mean(`Win Rate %`), "Total Number of Nation" = length(`Win Rate %`))

aov(`Win Rate %` ~ Nation, data = tank_statstot) %>%
  summary()

#Winrate by Type
tank_statstot %>%
  ggplot(mapping=aes(x=`Win Rate %`,group=Type, color=Type)) +
  geom_boxplot() 

tank_statstot %>%
  group_by(Type) %>%
  summarise("Mean Percent by Tank Type" = mean(`Win Rate %`), "Total Number of Type" = length(`Win Rate %`))

aov(`Win Rate %` ~ Type, data = tank_statstot) %>%
  summary()

