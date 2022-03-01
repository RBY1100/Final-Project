knitr::opts_chunk$set(error = TRUE)
library(tidyverse)
tankstats <- read_csv("~/STA 518/Final-Project/tankdata.csv")

#Changing tank tier to character values instead of numerical
tankstats$tier <- as.character(tankstats$tier)

#Changing names for type variable
tankstats <- tankstats%>%
  mutate(type = fct_recode(type,
                           "Medium Tank" = "mediumTank",
                           "Heavy Tank" = "heavyTank",
                           "Light Tank" = "lightTank",
                           "Tank Destroyer" = "AT-SPG",
                           "Artillery" = "SPG"))

#Changing the order for tier responses for better display in the future
tankstats$tier <- factor(tankstats$tier , levels=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10"))

#Tidy up Tank names by removing unnecessary frontal designation and removing the _'s
Tank <- tankstats %>%
  pull(tag)
TankM <-   Tank %>%
  str_remove("^(.*?_)") %>%
  str_replace_all("_"," ")
tankstats$tag <- TankM

#Rename Variables for better display
tankstats <- tankstats %>%
  rename("Average Damage" = dmg, "Average Kills" = frg, "Average Spots" = spo, "Average Defense Points" = def, "Winrate" = win, "Tank" = tag)

#Remove unnecessary decimal from "Average Damage" 
tankstats$`Average Damage` <- round(tankstats$`Average Damage`, digits=0)

#View Data
print(tankstats)



#Some Basic Plots for Data

#Damage Based on Tier
tankstats %>%
  ggplot(mapping=aes(x=`Average Damage`,group=tier, color=tier)) +
  geom_boxplot()

#Winrate by Tier
tankstats %>%
  ggplot(mapping=aes(x=Winrate,group=tier, color=tier)) +
  geom_boxplot()

#Histogram of all tanks Winrates
tankstats %>%
  ggplot(mapping=aes(x=Winrate)) +
  geom_histogram()

#Scatter for winrate and average damage with points colored by tank tier
tankstats %>%
  ggplot(mapping=aes(x=`Average Damage`,y=Winrate, color=tier)) +
  geom_jitter()

#Scatter for winrate and average kills with points colored by tank tier
tankstats %>%
  ggplot(mapping=aes(x=`Average Kills`,y=Winrate, color=tier)) +
  geom_jitter()

tankstats %>%
  ggplot(mapping=aes(x=Winrate,group=type, color=type)) +
  geom_boxplot()

tankstats %>%
  ggplot(mapping=aes(x=`Average Damage`,group=type, color=type)) +
  geom_boxplot()
