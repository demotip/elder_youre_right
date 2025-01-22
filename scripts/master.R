# Groundhog etc
library(groundhog)

master <- c("haven", "labelled", "tidyverse", #clean_and_combine_data
            "haven", "lubridate",  #append_age_differences
            #none for variable_labels
            "xtable", "gridExtra", "fastDummies", "devtools", "TOSTER", #balance_tables
            "kableExtra", "showtext", "sysfonts", "ggtext", #descriptive_statistics.R
            "glue", "lfe", "lmtest", "sandwich", "modelsummary", #contrasts.R
            "cowplot", #plots.R
            #none for comparison_plots.R(?)
            "boot", "caret", "rms", "htmlTable", #cross_validation.R
            #none for presentable_table.R
            "flextable", "gt", "janitor", "kableExtra")

groundhog.library(master, "2021-11-01")

# Running everything all at once

# clean_and_combine_data.R combines and cleans Afrobarometer rounds 3, 4, and 7
# and outputs afpr_append.rds. For round 7, there are three datasets to combine: 
# Mauritius, Uganda, and the full Round 7 Afrobaromter dataset. 
# These three datasets are combined before merging into rounds 3 and 4.
source("scripts/clean_and_combine_data.R")

# append_age_differences.R appends age difference variables to the Afrobarometer
# data based on the ages of interviewers and respondents and saves this as a new dataset: afpr_ages.rds.
source("scripts/append_age_differences.R")

# variable_labels.R generates a dataframe of variable names, variable descriptions, 
# and variable groupings. This dataframe is used in subsequent files to group and relabel outcomes.
source("scripts/variable_labels.R")

# balance_tables.R uses afpr_ages.rds to generate the covariate balance table found in Table 3 of the paper.
source("scripts/balance_tables.R")

# descriptive_statistics.R uses afpr_ages.rds to create tables and figures that compare the age distributions 
# of respondents and interviewers in the Afrobarometer data (Table 2 and Figure 1 in the paper). 
# This file also creates tables that compare the responses of older and younger interviewers 
# (Table 4 and Table 5 in the paper).
source("scripts/descriptive_statistics.R")

# contrasts.R runs the main models (with 35-years old age cutoff) 
# as well as the appendix models (with 40-years old age cutoff and 10-year age difference) 
# using afpr_ages.rds. Model estimates are tidied into a dataframe and saved as age_diff_models.rds.
source("scripts/contrasts.R")

# plots.R creates plots of our main findings (Figures 2 to 5) and the plots 
# found in the appendix of the paper (Figures 6 to 13).
source("scripts/plots.R")

# comparison_plots is the code for the predicted probabilities plots. 
# Same dependencies as contrasts.R. 
source("scripts/comparison_plots.R")

# cross_validation.R conducts cross-validations between logistic/ordered and linear regressions.
# Same dependencies as contrasts.R. 
source("scripts/cross_validation.R")

# presentable_table.R makes tables for the main paper which only show the direction 
# and significance of the results. Same dependencies as plots.R. 
source("scripts/presentable_table.R")

# regression_tables.R creates full regression tables to go into the appendix. 
# Also runs the full logistic and ordered logistic models,
# Organized by outcome group. 
# Same dependencies as contrasts.R. 
source("scripts/regression_tables.R")

# results_tables.R creates tables with the estimate and standard error. 
# Same dependencies as plots.R. 
source("scripts/results_tables.R")

