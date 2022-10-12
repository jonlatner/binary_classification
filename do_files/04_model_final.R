# Top commands -----
# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
        
}
detachAllPackages()
rm(list=ls(all=TRUE))

## Assignment
# The task at hand is a binary classification problem, for which both a df_training and a df_validation data set are provided as csv files.
# We would like you to:
# 1) explore both data sets, note down your key observations along with a kind of summary
# 2) build a classifier – a prediction model based only on the df_training data, with the goal of achieving the best performance possible on the df_validation data.
# 3) visualize results and the work on this classification task.
# You are allowed to process data in any way you see fit to achieve this goal.
# You are free to choose any programming to complete this task.
# We would like to get any code you write for this project, and would appreciate it if it was at least informally commented so that it’s easier to read for us.


## Folders - adapt this pathway
setwd("/Users/jonathanlatner/OneDrive/job_market/current/gfk/assignment/")

data_files = "data_files/"
tables = "tables/"
graphs = "graphs/"

## Importing libraries
library(caret) # Package for machine learning algorithms / CARET stands for Classification And REgression df_training
library(tidyverse)

## Functions
options(scipen = 999) # disable scientific notation

set.seed(1234)

# Load data ----
# read.csv2 assumes sep = ";" and delim = ",".  this is good.  else some numerical variables are imported as character variables.
# df is short for data frame

df_training <- read.csv2(paste0(data_files,"training.csv"))
df_validation <- read.csv2(paste0(data_files,"validation.csv"))

# Clean variables ----
## training data
df_training_01 <- df_training # preserve original
df_training_01$v17 <- NULL # equal to classLabel
df_training_01$v16 <- NULL # v9, but without missing
df_training_01$v15 <- NULL # perfectly correlated with v13
df_training_01$v3 <- NULL # mean, median, mode, and sd = 0

df_training_01 <- df_training_01 %>%
        mutate(v12=ifelse(v12=="s", yes = "p", no = v12))
df_training_01 <- droplevels(df_training_01)

## validation data
df_validation <- df_validation %>%
        mutate(ID = row_number())
df_validation_01 <- df_validation # preserve original
df_validation_01$v17 <- NULL # equal to classLabel
df_validation_01$v16 <- NULL # v9, but without missing
df_validation_01$v15 <- NULL # perfectly correlated with v13
df_validation_01$v3 <- NULL # mean, median, mode, and sd = 0

df_validation_01 <- df_validation_01 %>% # preserve original
        mutate(v12=ifelse(v12=="s", yes = "p", no = v12)) %>%
        filter(v12!="o")

df_validation_01 <- df_validation_01[complete.cases(df_validation_01), ]

# Model 01 - Random Forest ----

# fit the model
model_rf_01 = train(classLabel ~ .,
                    data=df_training_01,
                    na.action = "na.omit",
                    method = 'ranger',
                    trControl = trainControl(method = "cv"))

#use model to predict probability of default
predicted <- predict(model_rf_01, df_validation_01)

#create new data frame with predicted output on validation data
df_validation_01_output <- df_validation_01
df_validation_01_output$predict <- predicted 
df_validation_01_output <- df_validation_01_output %>%
        select(ID,predict)
df_output <- merge(df_validation,df_validation_01_output,all.x = TRUE) %>%
        select(-ID)

with(df_output, table(classLabel,predict,useNA = "ifany"))

# Save ----

write.csv2(df_output,paste0(data_files,"solution.csv"))
