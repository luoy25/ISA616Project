# Load required packages
if(require(pacman)==FALSE) 
  install.packages("pacman")
pacman::p_load(tidyverse,hexbin,caret)

# Get data
library(curl)
URL1="https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
URL2="https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
destloc=paste0(getwd())
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
create_report(wine)
# Get a report with response variable "quality"
library(ggplot2)
create_report(wine, y = "quality")
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

# feature importance
set.seed(13)
library(caret)
rPartMod <- train(quality ~ ., data=train, method="rpart")
rpartImp <- varImp(rPartMod)
print(rpartImp)

# Stepwise
step0 = lm(formula = quality ~ ., data = train)
summary(step0)

step1 = lm(formula = quality ~ fixed.acidity+volatile.acidity+residual.sugar+chlorides+ 
                    free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+
                    alcohol+wine.type, data = train)
summary(step1)

step2 = lm(formula = quality ~ fixed.acidity+volatile.acidity+residual.sugar+ 
             free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+
             alcohol+wine.type, data = train)
summary(step2)

step3 = lm(formula = quality ~ volatile.acidity+ 
             free.sulfur.dioxide+density+
             alcohol, data = train)
summary(step3)

library(MASS)
# Fit the full model 
full <- lm(quality ~., data = train)
# Stepwise regression model
step <- stepAIC(full, direction = "both", trace = FALSE)
summary(step)

p.train<-predict(step)
p.valid<-predict(step, newdata=valid)
head(p.train)

library(caret)
RMSE(p.valid, valid$quality)
R2(p.valid, valid$quality)



