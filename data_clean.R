#data clean---------------------------------------------------------------------
library(dplyr)

#read data and choose the needed column
happiness_2015 <- read.csv("Happiness/2015.csv", header = T)
happiness_2015 <- happiness_2015[,c(1,2,4,6:11)]
happiness_2016 <- read.csv("Happiness/2016.csv", header = T)
happiness_2016 <- happiness_2016[,c(1,2,4,7:12)]
happiness_2017 <- read.csv("Happiness/2017.csv", header = T)
happiness_2017 <- happiness_2017[,c(1,3,6:11)]
happiness_2018 <- read.csv("Happiness/2018.csv", header = T)
happiness_2018 <- happiness_2018[,c(2:9)]
happiness_2019 <- read.csv("Happiness/2019.csv", header = T)
happiness_2019 <- happiness_2019[,c(2:9)]

#inner join the region variables
region_indicator <- happiness_2015[,1:2]
happiness_2017 <- inner_join(region_indicator, happiness_2017, by = "Country")
colnames(happiness_2018)[1] <- "Country"
happiness_2018 <- inner_join(region_indicator, happiness_2018, by = "Country")
colnames(happiness_2019)[1] <- "Country"
happiness_2019 <- inner_join(region_indicator, happiness_2019, by = "Country")

#add year
happiness_2015$year <- 2015
happiness_2016$year <- 2016
happiness_2017$year <- 2017
happiness_2018$year <- 2018
happiness_2019$year <- 2019

#the order of the columns
happiness_2015 <- happiness_2015[,c(10,1:9)]
happiness_2016 <- happiness_2016[,c(10,1:9)]
happiness_2017 <- happiness_2017[,c(10,1:7,9,8)]
happiness_2018 <- happiness_2018[,c(10,1:7,9,8)]
happiness_2019 <- happiness_2019[,c(10,1:7,9,8)]

#change the name of the columns
colnames(happiness_2015) <- c("year", "Country", "Region", "score", "GDP", "social", 
                              "life_ex", "freedom", "trust", "generosity")
colnames(happiness_2016) <- c("year", "Country", "Region", "score", "GDP", "social", 
                              "life_ex", "freedom", "trust", "generosity")
colnames(happiness_2017) <- c("year", "Country", "Region", "score", "GDP", "social", 
                              "life_ex", "freedom", "trust", "generosity")
colnames(happiness_2018) <- c("year", "Country", "Region", "score", "GDP", "social", 
                              "life_ex", "freedom", "trust", "generosity")
colnames(happiness_2019) <- c("year", "Country", "Region", "score", "GDP", "social", 
                              "life_ex", "freedom", "trust", "generosity")

#bind the data
happiness <- rbind(happiness_2015, happiness_2016,happiness_2017,happiness_2018,happiness_2019)
happiness$trust <- as.numeric(happiness$trust)
happiness <- na.omit(happiness)

# W_Eur <- happiness %>% filter(Region == "Western Europe")
# CE_Eur <- happiness %>% filter(Region == "Central and Eastern Europe")
# N_Amer <- happiness %>% filter(Region == "North America")
# L_Amer <- happiness %>% filter(Region == "Latin America and Caribbean")
# S_Asia <- happiness %>% filter(Region == "Southern Asia")
# E_Asia <- happiness %>% filter(Region == "Eastern Asia")
# SE_Asia <- happiness %>% filter(Region == "Southeastern Asia")
# MN_Africa <- happiness %>% filter(Region == "Middle East and Northern Africa")
# SS_Africa <- happiness %>% filter(Region == "Sub-Saharan Africa")
# Aus_New <- happiness %>% filter(Region == "Australia and New Zealand")



