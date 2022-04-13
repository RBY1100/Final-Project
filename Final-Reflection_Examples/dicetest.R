install.packages("random")
library(random)
library(tidyverse)
library(purrr)

#For a normal roll with one dice

normal <- function(x,y){
  Normal_2d <- 1:1000; Normal_2d
  for(l in 1:1000){
    sumofall2d <- 1:1000; sumofall2d
    for(i in 1:1000){
      sum2d <- 0
      roll2d <- sample(1:x, y, replace=TRUE)
      sum2d <- sum(roll2d)
      sumofall2d[i] <- sum2d 
    }
    Normal_2d[l] <- sum(sumofall2d)
  }
  return(Normal_2d)
}

#For rolls with with the GWF perk with one dice

gwf_rolls  <- function(x){
With_GWF <- 1:1000; With_GWF
for(l in 1:1000){
  sum <- 0
  roll <- sample(1:x, 1000, replace=TRUE)
  for(i in 1:1000){
    if(roll[i]<3){
      roll[i] <- sample(1:x, 1, replace=TRUE)
    }
  }  
  sum <- sum(roll)
  With_GWF[l] <- sum
}
return(With_GWF)
}

#For 2 dice with GWF rules:

gwf_2dice <- function(x){
totalroll <- 1:1000; totalroll
With_GWF_2d <- 1:1000; With_GWF_2d
for(l in 1:1000){
  for(i in 1:1000){
    roll2d <-sample(1:x, 2, replace=TRUE)
    if(roll2d[1]<3){
      roll2d[1] <-sample(1:x, 1, replace=TRUE)
    }
    if(roll2d[2]<3){
        roll2d[2] <-sample(1:x, 1, replace=TRUE)
    }
    sumroll <- sum(roll2d)
    totalroll[i] <- sumroll
  }
    finalroll <- sum(totalroll)
    With_GWF_2d[l] <- finalroll
}
return(With_GWF_2d)
}

Normal_d6 <- normal(6,1)
With_GWF_d6 <- gwf_rolls(6)
Normal_d8 <-normal(8,1)
With_GWF_d8 <-gwf_rolls(8)
Normal_d10 <-normal(10,1)
With_GWF_d10 <-gwf_rolls(10)
Normal_2d6 <- normal(6,2)
With_GWF_2d6 <- gwf_2dice(6)

#Arrange and Plot
dataframe <- data.frame(Normal_d6, With_GWF_d6, Normal_d8, With_GWF_d8, Normal_d10, With_GWF_d10, Normal_2d6, With_GWF_2d6)
dataframe <- dataframe %>% 
  pivot_longer(
    cols = c(Normal_d6, With_GWF_d6, Normal_d8, With_GWF_d8, Normal_d10, With_GWF_d10, Normal_2d6, With_GWF_2d6),
    names_to = "Roll_Type",
    values_to = "Sum_of_Rolls"
  )
dataframe$Roll_Type <- factor(dataframe$Roll_Type , levels=c("Normal_d6", "With_GWF_d6", "Normal_d8", "With_GWF_d8", "Normal_d10", "With_GWF_d10", "Normal_2d6", "With_GWF_2d6"))

dataframe %>%
  ggplot(mapping=aes(x=Sum_of_Rolls, by=Roll_Type, color=Roll_Type)) +
  geom_boxplot()

summarize(dataframe, mean(Normal_d6), mean(With_GWF_d6), mean(Normal_d8), mean(With_GWF_d8), mean(Normal_d10), mean(With_GWF_d10), mean(Normal_2d6), mean(With_GWF_2d6))
dataframe %>%
  summarize(d6improvement=(mean(With_GWF_d6)-mean(Normal_d6))/mean(With_GWF_d6),d8improvement=(mean(With_GWF_d8)-mean(Normal_d8))/mean(With_GWF_d8),d10improvement=(mean(With_GWF_d10)-mean(Normal_d10))/mean(With_GWF_d10), d2d6improvement=(mean(With_GWF_2d6)-mean(Normal_2d6))/mean(With_GWF_2d6), d6dmgincrease=(mean(With_GWF_d6)-mean(Normal_d6))/mean(With_GWF_d6)*6,d8dmgincrease=(mean(With_GWF_d8)-mean(Normal_d8))/mean(With_GWF_d8)*8,d10dmgincrease=(mean(With_GWF_d10)-mean(Normal_d10))/mean(With_GWF_d10)*10, d2d6dmgincrease=(mean(With_GWF_2d6)-mean(Normal_2d6))/mean(With_GWF_2d6)*12)
