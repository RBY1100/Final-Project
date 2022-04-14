knitr::opts_chunk$set(error = TRUE)
library(tidyverse)
library(rvest)
library(scales)
library(purrr)
library(stats)

#Create First Data Set

#Writes a function to scrape data from the different servers creating one data set for tank performance on a specific server
wotscrapewr <- function(x,y){
  tableda <- x %>%
    read_html() %>%
    html_nodes("table#stat_veh_all4")  %>% 
    html_table() %>% 
    .[[1]] %>%
    setNames(c('Name','Tier','Type','Nation',paste('Total Played',y, sep=" "),paste('Wins',y, sep=" "), paste('Win %',y, sep=" "),paste('Unique Players',y, sep=" "), 'Region'))
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


#Remove Unnecessary Variables and Rename Remaining ones
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
                             "USA" = "Usa",
                             "USSR" = "Ussr")) %>%
  
  mutate(Premium = fct_recode(Premium,
                              "No" = "0",
                              "Yes" = "1"))


#View Data
print(tank_statstot)



#Plots/Tables for Tank Data



#WinrateBased on Tier
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


#Tank Nation Preferences
tank_statstot %>%
  group_by(Nation) %>%
  summarise(`Nation Played` = sum(`Total Played`)) %>%
  mutate(Percent = `Nation Played` / sum(`Nation Played`)) %>%
  mutate(labels = scales::percent(Percent)) %>%
  group_by(Nation)%>%
  ggplot(aes(x = "", y = Percent, fill = Nation)) +
  geom_col(color="black") +
  geom_text(aes(x=1.5, label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())


#Tank Tier Preferences
tank_statstot %>%
  group_by(Tier) %>%
  summarise(`Tier Played` = sum(`Total Played`)) %>%
  mutate(Percent = `Tier Played` / sum(`Tier Played`)) %>%
  mutate(labels = scales::percent(Percent)) %>%
  group_by(Tier)%>%
  ggplot(aes(x = "", y = Percent, fill = Tier)) +
  geom_col(color="black") +
  geom_text(aes(x=1.5, label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())


#Tank Type Preferences
tank_statstot %>%
  group_by(Type) %>%
  summarise(`Type Played` = sum(`Total Played`)) %>%
  mutate(Percent = `Type Played` / sum(`Type Played`)) %>%
  mutate(labels = scales::percent(Percent)) %>%
  group_by(Type)%>%
  ggplot(aes(x = "", y = Percent, fill = Type)) +
  geom_col(color="black") +
  geom_text(aes(x=1.5, label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())


#Tank Premium Games Played
tank_statstot %>%
  group_by(Premium) %>%
  summarise(`Premium Played` = sum(`Total Played`)) %>%
  mutate(Percent = `Premium Played` / sum(`Premium Played`)) %>%
  mutate(labels = scales::percent(Percent)) %>%
  group_by(Premium)%>%
  ggplot(aes(x = "", y = Percent, fill = Premium)) +
  geom_col(color="black") +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())


#Top 10 most Popular Tanks
tank_statstot %>%
  arrange(desc(`Total Played`)) %>%
  select(Name,`Total Played`,`Win Rate %`,Type,Tier,Nation,Premium) %>%
  head(10)


#Top 10 Highest Winrate Tanks
tank_statstot %>%
  arrange(desc(`Win Rate %`)) %>%
  select(Name,`Win Rate %`,`Total Played`,Type,Tier,Nation,Premium) %>%
  head(10)


#Yoh Tanks (New Tank Line)
tank_statstot %>%
  filter(Name %in% c("A142 Pawlack Tank", "A147 M II Y", "A139 M III Y", "A144 M VI Y", "A143 M V Y", "T32 FL")) %>%
  arrange(Tier)%>%
  select(Name,`Win Rate %`,`Total Played`,Type,Tier,Nation,Premium)


#Test For Balance
`%!in%` <- Negate(`%in%`)
tank_statstot <-   tank_statstot %>%
  mutate(`Yoh Tank` = case_when(Name %in% c("A142 Pawlack Tank", "A147 M II Y", "A139 M III Y", "A144 M VI Y", "A143 M V Y", "T32 FL") ~ 'Yes',
                                Name %!in% c("A142 Pawlack Tank", "A147 M II Y", "A139 M III Y", "A144 M VI Y", "A143 M V Y", "T32 FL") ~ 'No'))

tank_statstot %>%
  ggplot(mapping=aes(x=`Win Rate %`, group=`Yoh Tank`, color=`Yoh Tank`)) +
  geom_boxplot()

tank_statstot %>%
  group_by(`Yoh Tank`) %>%
  summarise("Mean Percent by Yoh Tank Status" = mean(`Win Rate %`), "Total Number in Group" = length(`Win Rate %`))

aov(`Win Rate %` ~ `Yoh Tank`, data = tank_statstot) %>%
  summary()


#Writes a second function to scrape data from the different servers creating one data set for that specific server
wotscrapereg <- function(x,y){
  tableda <- x %>%
    read_html() %>%
    html_nodes("table#stat_veh_all4")  %>% 
    html_table() %>% 
    .[[1]] %>%
    mutate(Region=y, Winrate = Win/`Total played`*100) %>%
    select(-`Win %`)
}


#Creates datasets and join together for analyzing differences in servers
wot_tableeu2 <- wotscrapereg("https://wot-news.com/stat/server/eu/norm/en/", "EU")
wot_tableus2 <- wotscrapereg("https://wot-news.com/stat/server/us/norm/en/", "US") 
wot_tableru2 <- wotscrapereg("https://wot-news.com/stat/server/ru/norm/en/", "RU") 
wot_tablesea2 <- wotscrapereg("https://wot-news.com/stat/server/sea/norm/en/", "SEA") 

wot_list2 <- list(wot_tableeu2, wot_tableus2, wot_tableru2, wot_tablesea2)
tank_statsreg <- do.call(rbind, wot_list2) %>%
  filter(`Total played` > 160) %>%
  inner_join(other_info) %>%
  na.omit()


#Convert "Premium" into a character value
tank_statsreg$`Premium` <- as.character(tank_statsreg$`Premium`)


#Rename Premium and Nation
tank_statstot <- tank_statsreg%>%
  mutate(Nation = fct_recode(Nation,
                             "USA" = "Usa",
                             "USSR" = "Ussr")) %>%
  
  mutate(Premium = fct_recode(Premium,
                              "No" = "0",
                              "Yes" = "1"))


#Plots/Tables for Region Data


#Total Number of Plays
total <- tank_statsreg %>%
  summarise(`Total Plays` = sum(`Total played`))
print(paste("There are a total of", total, "plays and approximately", round(total/30), "games ran in the last 60 days."))


#Winrate based on Region
tank_statsreg %>%
  ggplot(mapping=aes(x=Winrate, group=Region, color=Region)) +
  geom_boxplot()

tank_statsreg %>%
  group_by(Region) %>%
  summarise("Mean Percent by Region Played" = mean(Winrate), "Total Number of Tanks" = length(Winrate))

aov(Winrate ~ Region, data = tank_statsreg) %>%
  summary()


#Total Server Population Breakdown
tank_statsreg %>%
  group_by(Region) %>%
  summarise(playedr = sum(`Total played`)) %>%
  mutate(Percent=playedr / sum(playedr)) %>%
  mutate(labels = scales::percent(Percent)) %>%
  group_by(Region)%>%
  ggplot(aes(x = "", y = Percent, fill = Region)) +
  geom_col(color="black") +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())
  
tank_statsreg %>%
  group_by(Region) %>%
  summarise(`Total Plays` = sum(`Total played`)) %>%
  mutate(Percent = scales::percent(`Total Plays` / sum(`Total Plays`))) %>%
  print()


#Creating a Function to Get Preference for a Variable by Region
region_pct <- function(x, y){
  tank_statsreg %>%
    filter(Region == x) %>%
    group_by({{y}}) %>%
    summarise(hold = sum(`Total played`), Region = {{x}}) %>%
    mutate(Percent = hold / sum(hold)) %>%
    mutate(labels = scales::percent(Percent))
}


#Pie Chart and Table for Tank Preferences by Region
NSEA <- region_pct("SEA", Nation)
NRU <- region_pct("RU", Nation)
NUS <- region_pct("US", Nation)
NEU <- region_pct("EU", Nation)

regnat <- list(NSEA, NRU, NUS, NEU)
regpnat <- do.call(rbind, regnat)

regpnat %>%
  ggplot(aes(x = "", y = Percent, fill = Nation, group = Region)) +
  geom_col(color="black") +
  coord_polar(theta = "y") +
  facet_grid(.~ Region) + theme_void() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())

regpnat %>%
  setNames(c("Nation","Players","Server","hold","Percent")) %>%
  select(Server,Nation,Players,Percent) %>%
  print()


#Creating a Pie Chart and Table for Premium Percentage by Region
PSEA <- region_pct("SEA", Premium)
PRU <- region_pct("RU", Premium)
PUS <- region_pct("US", Premium)
PEU <- region_pct("EU", Premium)

regpre <- list(PSEA, PRU, PUS, PEU)
regpred <- do.call(rbind, regpre)

regpred %>%
  ggplot(aes(x = "", y = Percent, fill = Premium, group = Region)) +
  geom_col(color="black") +
  geom_text(aes(x=1.5, label = labels),
            position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +
  facet_grid(.~ Region) + theme_void() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid  = element_blank())