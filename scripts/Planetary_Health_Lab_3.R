#Planetary health labs -- Peru dengue data:

rm(list=ls())
library(tidyverse)
library(lubridate)
library(doBy)
library(mosaic)
library(car)
library(sf)
library(grDevices)
library(sjPlot)
library(here)

#################################################
#Lab 3: Modeling environment-health relationships
#################################################

#read in dengue case data:
Den <- read.csv(here("data/Dengue_Confirmed_Peru.csv")) # change file path for your context
head(Den)
lapply(Den, class)

#make Date a date object
Den$Date <- as.Date(Den$Week_Yr)

#floor dates for later merging with environment data:
head(Den)
Den$Date_floored <- floor_date(Den$Date, "week")
head(Den)

Den_plot <- ggplot(Den, aes(x=Date_floored, y=Cases, col=as.factor(District))) +
  geom_point() + theme(legend.position="none")
Den_plot

####################################
# read in temperature data from GEE:
MDD_temp <- read.csv(here("data/MDD_Temperature.csv")) # change file path for your context
head(MDD_temp)

#convert system.index to date:
MDD_temp$Date <- substr(MDD_temp$system.index, 1, 8)
head(MDD_temp)
MDD_temp$Date <- gsub('(.{4})(.{2})(.*)', '\\1-\\2-\\3', MDD_temp$Date)
head(MDD_temp)
MDD_temp <- MDD_temp[,c(2:4)]
MDD_temp$Date <- as.Date(MDD_temp$Date)
head(MDD_temp)

# floor dates to week to summarize:
MDD_temp$Date_floored <- floor_date(MDD_temp$Date, "week")
head(MDD_temp)
length(unique(MDD_temp$Date))
length(unique(MDD_temp$Date_floored))

MDD_temp_weekly <- summaryBy(mean ~ IDDIST + Date_floored, data=MDD_temp, FUN=mean)
head(MDD_temp_weekly)
colnames(MDD_temp_weekly)[1] <- "Dist_Code"
colnames(MDD_temp_weekly)[3] <- "Mean_Temp_C"

### merge dengue and temperature data by floored dates:
Dengue_Temp <- merge(Den, MDD_temp_weekly, 
                     by=c("Dist_Code", "Date_floored"), 
                     all.x=F, all.y=F)
head(Dengue_Temp)
unique(Dengue_Temp$Year)

# standardize variables:
Dengue_Temp$Mean_Temp_C_std <- zscore(Dengue_Temp$Mean_Temp_C, na.rm=T)
hist(Dengue_Temp$Mean_Temp_C, n=50)
hist(Dengue_Temp$Mean_Temp_C_std, n=50)
Dengue_Temp$Cases_std <- zscore(Dengue_Temp$Cases, na.rm=T)
hist(Dengue_Temp$Cases, n=50)
hist(Dengue_Temp$Cases_std, n=50)
Dengue_Temp$log.Mean_Temp_C <- log(Dengue_Temp$Mean_Temp_C)
hist(Dengue_Temp$Mean_Temp_C, n=50)
hist(Dengue_Temp$log.Mean_Temp_C, n=50)
Dengue_Temp$log.Cases <- log(Dengue_Temp$Cases + 1)
hist(Dengue_Temp$Cases, n=50)
hist(Dengue_Temp$log.Cases, n=50)

#model dengue cases by temperature:
head(Dengue_Temp)

Dengue_lm <- lm(
  log.Cases ~
    log.Mean_Temp_C
  + I(log.Mean_Temp_C^2)
  + factor(Year)
  + factor(Dist_Code)
  ,
  data=Dengue_Temp,
  na.action=na.exclude
)
summary(Dengue_lm)
crPlots(Dengue_lm)
plot_model(Dengue_lm, type = "pred", terms = "log.Mean_Temp_C", pred.type = "fe")

#find at what temperature dengue peaks:
vertex <- -26.75177/(2*-4.17434)
vertex
plot(Dengue_Temp$Mean_Temp_C, Dengue_Temp$log.Mean_Temp_C)
abline(h=3.2)
abline(v=24.6)
#looks like ~25 C, close to predicted thermal optimum for dengue transmitted by Aedes albopictus (25.4-27.6 C) 

#exploratory plots
Den_plot2 <- ggplot(Dengue_Temp, aes(x=Mean_Temp_C, y=Cases, col=as.factor(District))) +
  geom_point() + theme(legend.position="none")
Den_plot2

Den_plot3 <- ggplot(Dengue_Temp, aes(x=Date_floored, y=Cases, col=as.factor(District))) +
  geom_point() + theme(legend.position="none")
Den_plot3

Den_plot4 <- ggplot(Dengue_Temp, aes(x=Date_floored, y=Mean_Temp_C, col=as.factor(District))) +
  geom_line() #+ theme(legend.position="none")
Den_plot4


##############################
# repeat the above either by adding in the additional Peru environment data from lab 2
# or using your health data and associated environment data from labs 1 and 2
# depending on what you did in earlier labs






