
# Thesis R script

# this script corresponds to the graphs in the thesis
 
# install & load packages
install.packages("tidyverse")
installed.packages(reshape2)
library(readxl)
library(haven)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(dslabs)
library(reshape2)

# set working directory
setwd("C:/Users/lucas/Desktop")

# import data on exports and imports
Ex_im <- read_excel("C:Exports_and_imports_to_GDP_India.xlsx")

# add variable for the sum of exports and imports
# create graph for sum over years
Ex_im %>%
    mutate(sum=exports+imports)%>%
    filter(year<=2015 & year>=2000)%>%
    ggplot(aes(year,sum))+ geom_point()+
    xlim(2000,2015)+ylab("%")+ 
    ggtitle("Exports and imports of goods and services (% of GDP)")

# import forest cover data

forest<-read_dta("forest_data.dta")

# keep only relevant variables in data frame
forest_data<-forest%>%
    select(shrid,num_cells,total_forest2000,total_forest2001,total_forest2002,total_forest2003,total_forest2004,total_forest2005,total_forest2006,total_forest2007,total_forest2008,total_forest2009,total_forest2010,total_forest2011,total_forest2012,total_forest2013,total_forest2014)

# reshape data from wide to long 
# divide total_forest variable by its number of pixel cells
forest_data<-melt(forest_data,variable_name = "year",value.names = "forest",id.vars = c("shrid","num_cells"))
forest_data<-forest_data%>%
    mutate(forest=value/num_cells)

# aggregate value of forest for each year-group
forest_data%>% group_by(variable)%>% summarize(sum(forest))
forest_data<-forest_data%>% group_by(variable)%>% mutate(total=sum(forest))

# check levels of variable
# 'variable' is a factor variable
forest_data<-levels(forest_data$variable)

# iterate over lines in original data frame
years <- c()
totals <- c()
year <- -1
total <- -1
index <- 0
for(i in forest_data$variable) {
  index <- index + 1
  # save current line into new vectors
  # discard all following frames with same year tag
  if (i != year){
    year <- i
    total <- forest_data$total[index]
    # append new values to lists
    years <- c(years,
               as.integer(substr(year,13,16)))
    totals <- c(totals, total)
  }
}

# create new data frame from the two new vectors
new_frame1<- data.frame(years, totals)

# create graph
new_frame1%>%
  ggplot(aes(years,totals))+ geom_point()+
  xlim(2000,2015)+ylab("forest cover")+ xlab("year")+
  ggtitle("Development of forest area in India")


# import night light data
light<-read_dta("light.dta")

# keep relevant data in data frame
light_data<-light%>%
  select(shrid,num_cells,total_light2000,total_light2001,total_light2002,total_light2003,total_light2004,total_light2005,total_light2006,total_light2007,total_light2008,total_light2009,total_light2010,total_light2011,total_light2012,total_light2013)

# reshape data to long
light_data<-melt(light_data,variable_name = "year",value.name = "light",id.vars = c("shrid","num_cells"))
light_data<-light_data%>%
  mutate(lights=light/num_cells)

# create Variable conditional on group
light_data%>% group_by(variable)%>% 
  summarize(sum(lights))
  light_data<-light_data%>% group_by(variable)%>% 
  mutate(total=sum(lights))


# iterate over lines in original data frame
years <- c()
totals <- c()
year <- -1
total <- -1
index <- 0
for(i in light_data$variable) {
  index <- index + 1
  # save current line into new vectors
  # discard all following frames with same year tag
  if (i != year){
    year <- i
    total <- light_data$total[index]
    # Append new values to lists
    years <- c(years,
               as.integer(substr(year,12,15)))
    totals <- c(totals, total)
  }
}
# create new data frame from the two new vectors
new_frame2 <- data.frame(years, totals)

# plot graph
new_frame2%>%
  ggplot(aes(years,totals))+ geom_point()+
  xlim(2000,2015)+ylab("night light luminosity")+ xlab("year")+
  ggtitle("Development of night lights above India")
