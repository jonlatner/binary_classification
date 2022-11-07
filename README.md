# binary_classification
 Binary classification problem

A. Most relevant folders/files are:

    1) presentation/results_latner.pdf -- this summarizes the analysis and results
    2) data_files/solution.csv -- predict variable is my solution for the exercise and represents the predicted output of the best model on the validation data set, useful for benchmarking
    3) do_files/ -- files in this folder are used to create all relevant tables, graphs, and output files from the training and validation data files

B. File includes five main folders (in alphabetical order):

1) data_files includes three data files: 

    a) training.csv
    b) validation.csv
    c) solution.csv -- prediction variable is my solution for the exercise

2) do_files includes five files (in alphabetical order):
    
    01_descriptives.R -- creates descriptive graphs to examine variable counts and correlation for numerical and factor variables

    02_model_compare.R -- compares and contrasts model fit across four types of models (glm, decision tree, random forest, and naive bayes)

    03_model_rf.R -- compares and contrasts model fit across three types of random forest models

    04_model_solution.R -- creates solution.csv

    05_next_steps.R -- preliminary analysis to improve model fit from the best random forest model.  question: what predicts 1?

3) graphs includes graphs in .pdf form

4) presentation includes three files (in alphabetical order)

    a) results_latner.pdf -- this is the presentation that summarizes the findings from the assignment

    b) results_latner.sh -- this is the shell file for the LaTeX file results_latner.tex

    c) results_latner.tex -- this is the .tex file for the presentation

5) tables includes tables in .tex form

    