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
library(tidyverse)
library(ggcorrplot) # ggcorrplot
library(xtable) # confusion matrix output

## Functions
options(scipen = 999) # disable scientific notation

is_binary <- function(x) {
        x0 <- na.omit(x)
        is.numeric(x) && length(unique(x0)) %in% 1:2 && all(x0 %in% 0:1)
}

# Load data ----
# read.csv2 assumes sep = ";" and delim = ",".  this is good.  else some numerical variables are imported as character variables.
# df is short for data frame

df_training <- read.csv2(paste0(data_files,"training.csv"))
df_validation <- read.csv2(paste0(data_files,"validation.csv"))

# Data cleaning ----

glimpse(df_training) 

## Transform all character variables into factor variables
df_training[sapply(df_training, is.character)] <- lapply(df_training[sapply(df_training, is.character)], as.factor)
df_validation[sapply(df_validation_01, is.character)] <- lapply(df_validation[sapply(df_validation_01, is.character)], as.factor)

## Transform all binary variables into factor variables
# https://stackoverflow.com/questions/65026376/automatically-code-binary-variables-as-factors

df_training <- df_training %>% 
        mutate(across(where(is_binary), ~ factor(., levels = 0:1)))
df_validation <- df_validation %>% 
        mutate(across(where(is_binary), ~ factor(., levels = 0:1)))

# Determine if variables are numeric or factor ----
num_cols <- unlist(lapply(df_training, is.numeric)) 
fctr_cols <- unlist(lapply(df_training, is.factor)) 

## Create one df for each set of numeric or factor variables
df_training_num <- df_training[ , num_cols]
df_training_fctr <- df_training[ , fctr_cols]

df_validation_num <- df_validation[ , num_cols]
df_validation_fctr <- df_validation[ , fctr_cols]

# Factor variables: summarize and compare data sets ----

## Create loop for factor variables for summary statistics ----
fctr_cols <- colnames(df_training_fctr) # Create vector of variable names for each set of factor variables 
df_training_fctr_summary <- data.frame()
for (v in fctr_cols) {
        t <- data.frame(prop.table(table(df_training_fctr[[v]], useNA = "ifany")))
        t$var <- v
        t$type <- "Training"
        df_training_fctr_summary <- rbind(df_training_fctr_summary,t)
}

df_validation_fctr_summary <- data.frame()
for (v in fctr_cols) {
        t <- data.frame(prop.table(table(df_validation_fctr[[v]], useNA = "ifany")))
        t$var <- v
        t$type <- "Validation"
        df_validation_fctr_summary <- rbind(df_validation_fctr_summary,t)
}

df_factor_summary <- rbind(df_training_fctr_summary,df_validation_fctr_summary)

## Compare variables (bar chart) ----
ggplot(data=df_factor_summary, aes(x=Var1, y=Freq,fill=type)) +
        facet_grid(~var, scales = "free_x") + 
        geom_bar(stat="identity", position=position_dodge()) +
        geom_text(aes(label = n), hjust = -1) +
        scale_y_continuous(limits = c(0,1)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.title = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

# ggsave(paste0(graphs,"graph_vars_fctr.pdf"), height = 6, width = 9, plot = last_plot())

## Graph correlation matrix ----
model.matrix(~0+., data=df_training_fctr) %>% 
        cor(use="pairwise.complete.obs") %>% 
        ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

ggsave(paste0(graphs,"graph_corrplot_fctr.pdf"), height = 6, width = 9, plot = last_plot())

# Clean R environment
rm(fctr_cols,df_factor_summary,df_validation_fctr_summary,df_training_fctr_summary,df_training_fctr,df_validation_fctr,t,num_cols,v)

# Numerical variables: summarize and compare data sets ----

## prepare data for graph by transforming from wide to long
df_training_num_long <- pivot_longer(df_training_num, cols = everything())
df_training_num_long$type <- "Training"
df_validation_num_long <- pivot_longer(df_validation_num, cols = everything())
df_validation_num_long$type <- "Validation"

df_numerical_summary <- rbind(df_training_num_long,df_validation_num_long)

## Graph numerical variables (box and whiskers chart) ----
ggplot(data=df_numerical_summary, aes(x=name, y=value, fill=type)) +
        facet_wrap(~name, scales = "free") + 
        geom_boxplot(position=position_dodge()) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.title = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_vars_num.pdf"), height = 6, width = 9, plot = last_plot())

## Graph correlation matrix ----

model.matrix(~0+., data=df_training_num) %>% 
        cor(use="pairwise.complete.obs") %>% 
        ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)

ggsave(paste0(graphs,"graph_corrplot_num.pdf"), height = 6, width = 9, plot = last_plot())

## Graph numerical variables for missing values (bar chart) ----

df_training_num$type <- "Training"
df_validation_num$type <- "Validation"

# combine training and validation data frames and calculate percent missing values per variable
df_numerical_summary_missing <- rbind(df_training_num,df_validation_num) %>%
        group_by(type) %>%
        summarise_all(funs(sum(is.na(.))/length(.))) %>%
        ungroup()

# Rotate data frame in preparation for graph
df_numerical_summary_missing <- pivot_longer(df_numerical_summary_missing, cols = !type)

ggplot(data=df_numerical_summary_missing, aes(x=name, y=value,fill=type)) +
        facet_grid(~name, scales = "free_x") + 
        geom_bar(stat="identity", position=position_dodge()) +
        scale_y_continuous(limits = c(0,.1)) +
        theme(panel.grid.minor = element_blank(), 
              legend.position = "bottom",
              legend.title = element_blank(),
              legend.key.width=unit(2, "cm"),
              axis.title = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5)
        )

ggsave(paste0(graphs,"graph_vars_num_msng.pdf"), height = 6, width = 9, plot = last_plot())

# Clean R environment
rm(df_training_num_long,df_validation_num_long,df_numerical_summary,df_numerical_summary_missing,df_training_num,df_validation_num)
