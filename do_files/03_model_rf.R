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
library(xtable) # confusion matrix output
library(doParallel)

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

## vlidation data
df_validation_01 <- df_validation # preserve original
df_validation_01$v17 <- NULL # equal to classLabel
df_validation_01$v16 <- NULL # v9, but without missing
df_validation_01$v15 <- NULL # perfectly correlated with v13
df_validation_01$v3 <- NULL # mean, median, mode, and sd = 0

df_validation_01 <- df_validation_01[complete.cases(df_validation_01), ]
df_validation_01 <- df_validation_01 %>%
        filter(v12!="o")

# Model 01 - Random Forest ----

# fit the model
start_time <- Sys.time()
model_rf_01 = train(classLabel ~ .,
                 data=df_training_01,
                 na.action = "na.omit",
                 method = 'ranger',
                 importance = 'impurity',
                 trControl = trainControl(method = "cv"))

end_time <- Sys.time()
time <- difftime(end_time,start_time, units = "secs")
model_rf_01_time <- round(time[[1]],2)

#use model to predict probability of default
predicted <- predict(model_rf_01, df_validation_01)
predicted <- ifelse(predicted=="yes.", "1", "0")

#convert defaults from "Yes" and "No" to 1's and 0's
reality <- ifelse(df_validation_01$classLabel=="yes.", "1", "0")

## create confusion matrix
model_rf_01_cm <- confusionMatrix(table(reality, predicted))

table_rf_01_cm <- data.frame(as.table(model_rf_01_cm))
table_rf_01_cm <- table_rf_01_cm %>% 
        group_by(predicted) %>%
        mutate(sum = sum(Freq),
               Pct = Freq/sum) %>%
        ungroup() %>%
        arrange(predicted) %>%
        select(-sum)
model_rf_01_accuracy <- round(as.matrix(model_rf_01_cm,what="overall")[1],3)


hline_top <- ("\\toprule \n")
columns_header_top <- c("& & 
                        \\multicolumn{2}{l}{Model 2a: Base} 
                        \\\\ \n")
columns_header_mid <- c("\\cmidrule(lr){3-4} 
                        \n ")
columns_header_bot <- c("reality & predicted & 
                        Freq & Pct 
                        \\\\ \\hline \\\\[-1.8ex]  \n ")
accuracy <- paste0("\\hline \\\\[-1.8ex]  \n
              \\multicolumn{2}{l}{Accuracy} & 
                   \\multicolumn{2}{c}{",model_rf_01_accuracy,"}
                   \\\\ \n")
duration <- paste0("
                   \\multicolumn{2}{l}{Duration (secs)} & 
                   \\multicolumn{2}{c}{",model_rf_01_time,"}
                   \\\\ \n")
hline_bot <- ("\\bottomrule \n")

t <- xtable(table_rf_01_cm, digits = 2)
print(t,
      file = paste0(tables,"table_rf_model_rf_01_cm.tex"),
      hline.after = NULL,
      include.rownames = FALSE,
      include.colnames = FALSE,
      floating="FALSE",
      add.to.row = list(
              pos = list(0,0,0,0,4,4,4),
              command = c(hline_top,
                          columns_header_top,
                          columns_header_mid,
                          columns_header_bot,
                          accuracy,
                          duration,
                          hline_bot)),
      comment = FALSE
)

Importance <- varImp(model_rf_01)
Importance$var <- row.names(Importance)

ggplot(data=Importance, aes(x=var,y=Overall)) +
        # coord_flip() +
        geom_bar(stat="identity") +
        theme(panel.grid.minor = element_blank(), 
              axis.title = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_model_rf_01_importance.pdf"), height = 6, width = 9, plot = last_plot())

# Model 02 - Random Forest ----

df_training_02 <- df_training_01 %>%
        mutate(v12=ifelse(v12=="s", yes = "p", no = v12))
df_training_02 <- droplevels(df_training_02)

df_validation_02 <- df_validation_01 %>%
        mutate(v12=ifelse(v12=="s", yes = "p", no = v12))
df_validation_02 <- droplevels(df_validation_02)

# fit the model
start_time <- Sys.time()
model_rf_02 = train(classLabel ~ .,
                    data=df_training_02,
                    na.action = "na.omit",
                    method = 'ranger',
                    importance = 'impurity',
                    trControl = trainControl(method = "cv"))
end_time <- Sys.time()
time <- difftime(end_time,start_time, units = "secs")
model_rf_02_time <- round(time[[1]],2)
model_rf_02_time

#use model to predict probability of default
predicted <- predict(model_rf_02, df_validation_02)
predicted <- ifelse(predicted=="yes.", "1", "0")

#convert defaults from "Yes" and "No" to 1's and 0's
reality <- ifelse(df_validation_02$classLabel=="yes.", "1", "0")

## create confusion matrix
model_rf_02_cm <- confusionMatrix(table(reality, predicted))

table_rf_02_cm <- data.frame(as.table(model_rf_02_cm))
table_rf_02_cm <- table_rf_02_cm %>% 
        group_by(predicted) %>%
        mutate(sum = sum(Freq),
               Pct = Freq/sum) %>%
        ungroup() %>%
        arrange(predicted) %>%
        select(-sum, -reality, -predicted)
table_rf_02_cm <- cbind(table_rf_01_cm,table_rf_02_cm)
table_rf_02_cm

model_rf_02_accuracy <- round(as.matrix(model_rf_02_cm,what="overall")[1],3)

hline_top <- ("\\toprule \n")
columns_header_top <- c("& & 
                        \\multicolumn{2}{l}{Model 2a: Base} &
                        \\multicolumn{2}{l}{Model 2b: v12} 
                        \\\\ \n")
columns_header_mid <- c("\\cmidrule(lr){3-4} 
                        \\cmidrule(lr){5-6} 
                        \n ")
columns_header_bot <- c("reality & predicted & 
                        Freq & Pct &
                        Freq & Pct 
                        \\\\ \\hline \\\\[-1.8ex]  \n ")
accuracy <- paste0("\\hline \\\\[-1.8ex]  \n
              \\multicolumn{2}{l}{Accuracy} & 
                   \\multicolumn{2}{c}{",model_rf_01_accuracy,"} &
                   \\multicolumn{2}{c}{",model_rf_02_accuracy,"}
                   \\\\ \n")
duration <- paste0("
                   \\multicolumn{2}{l}{Duration (secs)} & 
                   \\multicolumn{2}{c}{",model_rf_01_time,"} &
                   \\multicolumn{2}{c}{",model_rf_02_time,"}
                   \\\\ \n")
hline_bot <- ("\\bottomrule \n")

t <- xtable(table_rf_02_cm, digits = 2)
print(t,
      file = paste0(tables,"table_rf_model_rf_02_cm.tex"),
      hline.after = NULL,
      include.rownames = FALSE,
      include.colnames = FALSE,
      floating="FALSE",
      add.to.row = list(
              pos = list(0,0,0,0,4,4,4),
              command = c(hline_top,
                          columns_header_top,
                          columns_header_mid,
                          columns_header_bot,
                          accuracy,
                          duration,
                          hline_bot)),
      comment = FALSE
)


Importance <- varImp(model_rf_02)
Importance$var <- row.names(Importance)

ggplot(data=Importance, aes(x=var,y=Overall)) +
        # coord_flip() +
        geom_bar(stat="identity") +
        theme(panel.grid.minor = element_blank(), 
              axis.title = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_model_rf_02_importance.pdf"), height = 6, width = 9, plot = last_plot())

# Model 03 - Random Forest ----

df_training_03 <- df_training_02 %>%
        mutate(v4=ifelse(v4=="y", yes = "u", no = v4)) %>%
        select(-v10)
df_training_03 <- droplevels(df_training_03)

df_validation_03 <- df_validation_02 %>%
        mutate(v4=ifelse(v4=="y", yes = "u", no = v4)) %>%
        select(-v10)
df_validation_03 <- droplevels(df_validation_03)

# fit the model
start_time <- Sys.time()
model_rf_03 = train(classLabel ~ .,
                    data=df_training_03,
                    na.action = "na.omit",
                    method = 'ranger',
                    importance = 'impurity',
                    trControl = trainControl(method = "cv"))
end_time <- Sys.time()
time <- difftime(end_time,start_time, units = "secs")
model_rf_03_time <- round(time[[1]],2)
model_rf_03_time

#use model to predict probability of default
predicted <- predict(model_rf_03, df_validation_03)
predicted <- ifelse(predicted=="yes.", "1", "0")

#convert defaults from "Yes" and "No" to 1's and 0's
reality <- ifelse(df_validation_03$classLabel=="yes.", "1", "0")

## create confusion matrix
model_rf_03_cm <- confusionMatrix(table(reality, predicted))

table_rf_03_cm <- data.frame(as.table(model_rf_03_cm))
table_rf_03_cm <- table_rf_03_cm %>% 
        group_by(predicted) %>%
        mutate(sum = sum(Freq),
               Pct = Freq/sum) %>%
        ungroup() %>%
        arrange(predicted) %>%
        select(-sum, -reality, -predicted)
table_rf_03_cm <- cbind(table_rf_02_cm,table_rf_03_cm)
table_rf_03_cm

model_rf_03_accuracy <- round(as.matrix(model_rf_03_cm,what="overall")[1],3)

hline_top <- ("\\toprule \n")
columns_header_top <- c("& & 
                        \\multicolumn{2}{l}{Model 2a: Base} &
                        \\multicolumn{2}{l}{Model 2b: v12} &
                        \\multicolumn{2}{l}{Model 2c: v4/v10} 
                        \\\\ \n")
columns_header_mid <- c("\\cmidrule(lr){3-4} 
                        \\cmidrule(lr){5-6} 
                        \\cmidrule(lr){7-8} 
                        \n ")
columns_header_bot <- c("reality & predicted & 
                        Freq & Pct &
                        Freq & Pct &
                        Freq & Pct 
                        \\\\ \\hline \\\\[-1.8ex]  \n ")
accuracy <- paste0("\\hline \\\\[-1.8ex]  \n
              \\multicolumn{2}{l}{Accuracy} & 
                   \\multicolumn{2}{c}{",model_rf_01_accuracy,"} &
                   \\multicolumn{2}{c}{",model_rf_02_accuracy,"} &
                   \\multicolumn{2}{c}{",model_rf_03_accuracy,"}
                   \\\\ \n")
duration <- paste0("
                   \\multicolumn{2}{l}{Duration (secs)} & 
                   \\multicolumn{2}{c}{",model_rf_01_time,"} &
                   \\multicolumn{2}{c}{",model_rf_02_time,"} &
                   \\multicolumn{2}{c}{",model_rf_03_time,"}
                   \\\\ \n")
hline_bot <- ("\\bottomrule \n")

t <- xtable(table_rf_03_cm, digits = 2)
print(t,
      file = paste0(tables,"table_rf_model_rf_03_cm.tex"),
      hline.after = NULL,
      include.rownames = FALSE,
      include.colnames = FALSE,
      floating="FALSE",
      add.to.row = list(
              pos = list(0,0,0,0,4,4,4),
              command = c(hline_top,
                          columns_header_top,
                          columns_header_mid,
                          columns_header_bot,
                          accuracy,
                          duration,
                          hline_bot)),
      comment = FALSE
)


Importance <- varImp(model_rf_03)
Importance$var <- row.names(Importance)

ggplot(data=Importance, aes(x=var,y=Overall)) +
        # coord_flip() +
        geom_bar(stat="identity") +
        theme(panel.grid.minor = element_blank(), 
              axis.title = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_model_rf_03_importance.pdf"), height = 6, width = 9, plot = last_plot())
