# Load required packages
if(require(pacman)==FALSE) 
  install.packages("pacman")
pacman::p_load(tidyverse,hexbin)

# Get data
library(curl)
URL1="https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
URL2="https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
destloc=paste0(getwd(),".curltmp")
curl_download(url=URL1,destfile=destloc,quiet=F,mode="wb")
curl_download(url=URL2,destfile=destloc,quiet=F,mode="wb")

# Data preparation
# load data
wineRed = read.csv("winequality-red.csv",sep = ";")
wineWhite = read.csv("winequality-white.csv",sep = ";")
# Take a look at the data
head(wineRed)
head(wineWhite)
# SUmmary of data
summary(wineRed)
summary(wineWhite)
# Create dummy Variable
wineRed$wine.type <- 1
wineWhite$wine.type <- 0
# Combining the dataframes into one
wine = rbind(wineRed, wineWhite)
head(wine)
# Check to see if the merge was successful
dim(wineRed)
dim(wineWhite)
dim(wine)
summary(wine)
# Check missing data
is.na(wine)
# get a report for wine dataset
library(DataExplorer)
create_report(wine)
# Get a report with response variable "quality"
library(ggplot2)
create_report(wine, y = "quality")
# save data
saveRDS(wine, "wine.RDS")





