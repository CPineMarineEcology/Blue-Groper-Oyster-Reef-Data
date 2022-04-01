###Loading in packages
library(tidyverse)
library(readxl)


#loading in Blue Groper Data
dat.bg <- read_xlsx("BG Data Raw.xlsx")

#cleaning data
dat.bg <- na.omit(dat.bg)

#average MaxN when bgs occured
avMaxN <- dat.bg %>%
  filter(MaxN != 0) %>% #removing all 0 observations from the dataset as it is the average MaxN when they OCCURED
  group_by(Site) %>%
  summarise(AvgMaxN = mean(MaxN))

##calculating standard error of MaxN
SDmaxN <- dat.bg %>%
  group_by(Site) %>%
  summarise(SD = sd(MaxN))

#adding SD to the AvGmaxN summary sheet
avMaxN$Sd <- SDmaxN$SD

#calculating SE
SE <- avMaxN %>%
  group_by(Site)%>%
  summarise(SE= Sd/sqrt(length(AvgMaxN)))

#adding SE to the AvgMaxN Summary Sheet
avMaxN$SE <- SE$SE


