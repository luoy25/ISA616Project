# Load required packages
if(require(pacman)==FALSE) 
  install.packages("pacman")
pacman::p_load(tidyverse,hexbin,caret,car)

# # Get data
# library(curl)
# URL1="https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
# URL2="https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
# destloc=paste0(getwd())
# curl_download(url=URL1,destfile=destloc,quiet=F,mode="wb")
# curl_download(url=URL2,destfile=destloc,quiet=F,mode="wb")

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
wine$wine.type <- as.factor(wine$wine.type)
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
# create_report(wine)
# Get a report with response variable "quality"
library(ggplot2)
# create_report(wine, y = "quality")
# save data
saveRDS(wine, "wine.RDS")
wine=readRDS("wine.RDS")

# set seed
set.seed(13)
trainIndex = sample(1:nrow(wine), size = round(0.75*nrow(wine)), replace=FALSE)

train<-wine[trainIndex, ]
valid<-wine[-trainIndex, ]
nrow(train)
nrow(valid)

# StepWise Model
library(MASS)
# Fit the full model 
full <- lm(quality ~., data = train)
# Stepwise regression model
step <- stepAIC(full, direction = "both", trace = FALSE)
summary(step)
varImp(step)
library(car)
vif(step)

# output the summary as a table
# load package
library(sjPlot)
library(sjmisc)
library(sjlabelled)

tab_model(step)

p.valid<-predict(step, newdata=valid)
head(p.train)

library(caret)
RMSE(p.valid, valid$quality)
R2(p.valid, valid$quality)

# 
# citation("dplyr")
# citation("caret")
# citation("DataExplorer")
# citation("kableExtra")
# citation("MLmetrics")
# citation("MASS")

plot(train$quality)

train$pred_quality=step$fitted.values
valid$pred_quality=predict(step, newdata=valid)

p=ggplot(data = train)+
  geom_point(aes(quality,pred_quality,col="blue"),size=3,data=train)
p=p+xlim(2,10)+ylim(2,10)
p=p+labs(x="Quality",y="Predicted Quality")+ggtitle("In sample performance")
p=p+scale_color_manual(labels = c("In Sample Performance"),values = c("blue"))
p=p+geom_abline(slope = 1,intercept = 0)
p

p = ggplot(data = valid)+
  geom_point(aes(quality,pred_quality,col="red"),size=3)
p=p+xlim(2,10)+ylim(2,10)
p=p+labs(x="Quality",y="Predicted Quality")+ggtitle("In sample performance")
p=p+scale_color_manual(labels = c("Out of Sample Performance"),values = c("red"))
p=p+geom_abline(slope = 1,intercept = 0)
p

plot_histogram(wine$quality)
plot(wine$quality)
