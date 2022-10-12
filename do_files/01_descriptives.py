###########################
# TOP COMMANDS
###########################
# create empty session
def clear_all():
    """Clears all the variables from the workspace."""
    gl = globals().copy()
    for var in gl:
        if var[0] == '_': continue
        if 'func' in str(globals()[var]): continue
        if 'module' in str(globals()[var]): continue

        del globals()[var]
if __name__ == "__main__":
    clear_all()

# load libraries
import pandas as pd
import numpy as np
import os
import matplotlib.pyplot as plt

# beginning commands
pd.set_option('display.float_format', str) # drop scientific notation
pd.set_option('display.max_columns', None) # display max columns
plt.clf() # this clears the figure

# file paths - adapt main_dir pathway
main_dir = "/Users/jonathanlatner/Desktop/gfk_assignment/"
data_files = "data_files/"
graphs = "graphs/"
tables = "tables/"

## Assignment
# The task at hand is a binary classification problem, for which both a df_training and a df_validation data set are provided as csv files.
# We would like you to:
# 1) explore both data sets, note down your key observations along with a kind of summary
# 2) build a classifier – a prediction model based only on the df_training data, with the goal of achieving the best performance possible on the df_validation data.
# 3) visualize results and the work on this classification task.
# You are allowed to process data in any way you see fit to achieve this goal.
# You are free to choose any programming to complete this task.
# We would like to get any code you write for this project, and would appreciate it if it was at least informally commented so that it’s easier to read for us.

###########################
# LOAD DATA
###########################

df_training = pd.read_csv(os.path.join(main_dir,data_files,"training.csv"),sep=";", decimal=",", thousands=".")
df_validation = pd.read_csv(os.path.join(main_dir,data_files,"validation.csv"),sep=";", decimal=",", thousands=".")

###########################
# Data cleaning 
###########################

# descriptives
df_training.columns
df_training.describe()
df_training.info()

# missing values?
df_training.isnull().sum() 
df_training["v9"].value_counts(dropna=False).sort_index() 

## Create one df for each set of numeric or factor variables
df_training_num = df_training.select_dtypes(exclude = ['object'])
df_training_fctr = df_training.select_dtypes(include = ['object'])

# count unique factors for each variable
print(df_training_fctr.nunique(axis=0))

df_validation_num = df_validation.select_dtypes(exclude = ['object'])
df_validation_fctr = df_validation.select_dtypes(include = ['object'])

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
