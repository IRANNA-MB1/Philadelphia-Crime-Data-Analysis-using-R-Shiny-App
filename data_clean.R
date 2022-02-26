library(tidyverse)
library(ggplot2)
library(ggmap)
library(stringr)
library(jpeg)
library(grid)
library(shiny)
library(rsconnect)
library(data.table)

rawdat <- read.csv("C:/R Studio_and_Git_and_Github/R shiny app for Philadelphia Crime Data Analysis/Philadelphia-Crime-Data-Analysis-using-R-Shiny-App/crime.csv", 
                   stringsAsFactors = FALSE, 
                   header = TRUE, 
                   na.strings = c("", " ", "NA"))

newdat <- rawdat

colnames(newdat)[colnames(newdat) == "Text_General_Code"] <- "Crime"

newdat <- 
  newdat %>% 
  select(Dispatch_Date, Hour, Crime, Lon, Lat)

newdat <-
  newdat %>% 
  filter(!is.na(Crime)) %>% 
  filter(!is.na(Lon))

newdat$Crime <- as.factor(newdat$Crime)

newdat <-
  newdat %>% 
  filter(Crime != "All Other Offenses") %>% 
  filter(Crime != "Other Assaults") %>%
  droplevels()

newdat <-
  newdat %>%
  separate(col = Dispatch_Date, into = c("Year", "Month"), sep = "-", remove = FALSE, extra = "drop")

newdat$Dispatch_Date <- as.Date(newdat$Dispatch_Date, "%Y-%m-%d")

newdat$Dayofweek <- weekdays(newdat$Dispatch_Date)

newdat <-
  newdat %>%
  mutate(Weekendornot = ifelse(Dayofweek == "Saturday" | Dayofweek == "Sunday", "Weekend", "Weekday"))

newdat <-
  newdat %>% 
  filter(Year != "2017") %>% 
  droplevels()

newdat$Month <- as.numeric(newdat$Month)

newdat <-
  newdat %>%
  mutate(Season = ifelse(Month >= 3 & Month <= 5, "Spring", 
                         ifelse(Month >= 6 & Month <= 8, "Summer",
                                ifelse(Month >= 9 & Month <= 11, "Autumn", "Winter"))))

newdat <- 
  newdat %>% 
  select(-Dispatch_Date, -Dayofweek, -Month)

newdat$Year <- as.numeric(newdat$Year)

newdat<- newdat[-seq(nrow(newdat),nrow(newdat)-199999),]

write.csv(newdat,"C:/R Studio_and_Git_and_Github/R shiny app for Philadelphia Crime Data Analysis/Philadelphia-Crime-Data-Analysis-using-R-Shiny-App/crime2.csv")
