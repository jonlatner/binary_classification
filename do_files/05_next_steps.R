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
# The task at hand is a binary classification problem, for which both a df_class_no and a df_class_yes data set are provided as csv files.
# We would like you to:
# 1) explore both data sets, note down your key observations along with a kind of summary
# 2) build a classifier – a prediction model based only on the df_class_no data, with the goal of achieving the best performance possible on the df_class_yes data.
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

# Clean data ----
df_training$v17 <- NULL # equal to classLabel
df_training$v16 <- NULL # v9, but without missing
df_training$v15 <- NULL # perfectly correlated with v13
df_training$v3 <- NULL # mean, median, mode, and sd = 0

df_training <- df_training %>%
        mutate(v12=ifelse(v12=="s", yes = "p", no = v12))
df_training <- droplevels(df_training)

df_class_no <- df_training %>%
        filter(classLabel == "no.")

df_class_yes <- df_training %>%
        filter(classLabel == "yes.")

## validation data
df_validation <- df_validation %>%
        mutate(ID = row_number())
df_validation_01 <- df_validation # preserve original
df_validation_01$v17 <- NULL # equal to classLabel
df_validation_01$v16 <- NULL # v9, but without missing
df_validation_01$v15 <- NULL # perfectly correlated with v13
df_validation_01$v3 <- NULL # mean, median, mode, and sd = 0

df_validation_01 <- df_validation_01 %>% # preserve original
        mutate(v12=ifelse(v12=="s", yes = "p", no = v12),
        ) %>%
        filter(v12!="o")

# Data cleaning ----

## Transform all character variables into factor variables
df_class_no[sapply(df_class_no, is.character)] <- lapply(df_class_no[sapply(df_class_no, is.character)], as.factor)
df_class_yes[sapply(df_class_yes, is.character)] <- lapply(df_class_yes[sapply(df_class_yes_01, is.character)], as.factor)

## Transform all binary variables into factor variables
# https://stackoverflow.com/questions/65026376/automatically-code-binary-variables-as-factors

df_class_no <- df_class_no %>% 
        mutate(across(where(is_binary), ~ factor(., levels = 0:1)))
df_class_yes <- df_class_yes %>% 
        mutate(across(where(is_binary), ~ factor(., levels = 0:1)))

# Determine if variables are numeric or factor ----
num_cols <- unlist(lapply(df_class_no, is.numeric)) 
fctr_cols <- unlist(lapply(df_class_no, is.factor)) 

## Create one df for each set of numeric or factor variables
df_class_no_num <- df_class_no[ , num_cols]
df_class_no_fctr <- df_class_no[ , fctr_cols]

df_class_yes_num <- df_class_yes[ , num_cols]
df_class_yes_fctr <- df_class_yes[ , fctr_cols]

# Factor variables: summarize and compare data sets ----

## Create loop for factor variables for summary statistics ----
fctr_cols <- colnames(df_class_no_fctr) # Create vector of variable names for each set of factor variables 
df_class_no_fctr_summary <- data.frame()
for (v in fctr_cols) {
        t <- data.frame(prop.table(table(df_class_no_fctr[[v]], useNA = "ifany")))
        t$var <- v
        t$type <- "No"
        df_class_no_fctr_summary <- rbind(df_class_no_fctr_summary,t)
}

df_class_yes_fctr_summary <- data.frame()
for (v in fctr_cols) {
        t <- data.frame(prop.table(table(df_class_yes_fctr[[v]], useNA = "ifany")))
        t$var <- v
        t$type <- "Yes"
        df_class_yes_fctr_summary <- rbind(df_class_yes_fctr_summary,t)
}

df_factor_summary <- rbind(df_class_no_fctr_summary,df_class_yes_fctr_summary)

## Compare variables (bar chart) ----
ggplot(data=df_factor_summary, aes(x=Var1, y=Freq,fill=type)) +
        facet_grid(~var, scales = "free_x") + 
        geom_bar(stat="identity", position=position_dodge()) +
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

# Clean R environment
rm(fctr_cols,df_class_yes_fctr_summary,df_class_no_fctr_summary,df_class_no_fctr,df_class_yes_fctr,t,num_cols,v)

# Numerical variables: summarize and compare data sets ----

## prepare data for graph by transforming from wide to long
df_class_no_num_long <- pivot_longer(df_class_no_num, cols = everything())
df_class_no_num_long$type <- "No"
df_class_yes_num_long <- pivot_longer(df_class_yes_num, cols = everything())
df_class_yes_num_long$type <- "Yes"

df_numerical_summary <- rbind(df_class_no_num_long,df_class_yes_num_long)

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

# ggsave(paste0(graphs,"graph_vars_num.pdf"), height = 6, width = 9, plot = last_plot())


# Clean R environment
rm(df_class_no_num_long,df_class_yes_num_long,df_numerical_summary,df_class_no_num,df_class_yes_num)

df_summary_stats <- data.frame()
vars = c("v10", "v13", "v14")
for(v in vars){
        t <- df_training %>% 
                group_by(classLabel) %>% 
                filter(!is.na(get(v))) %>%
                summarise(min = min(get(v)),
                          p25 = quantile(get(v), probs = .25),
                          mean = mean(get(v)),
                          median = median(get(v)),
                          sd = sd(get(v)),
                          p75 = quantile(get(v), probs = .75),
                          p90 = quantile(get(v), probs = .90),
                          max = max(get(v)),
                          ) %>%
                ungroup() %>%
                mutate(var = v) %>%
                select(var, everything())
        df_summary_stats <- rbind(df_summary_stats,t)
}

df_summary_stats

library(Hmisc)
describe(df_class_no$v14)
describe(df_class_yes$v14)
