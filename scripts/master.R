# Groundhog etc
library(groundhog)

master <- c("glue", "lfe", "tidyverse", "lmtest", "sandwich", "modelsummary",
            "boot", "caret", "rms", "htmlTable", "cowplot", 
            "showtext", "sysfonts", "flextable", "gt", "janitor", "kableExtra")

groundhog.library(master, "2021-11-01")

# Running everything all at once

source("scripts/clean_and_combine_data.R")
source("scripts/append_age_differences.R")
source("scripts/variable_labels.R")
source("scripts/balance_tables.R")
source("scripts/descriptive_statistics.R")
source("scripts/contrasts.R")
source("scripts/plots.R")
source("scripts/comparison_plots.R")
source("scripts/contrasts_logistic.R")
source("scripts/cross_validation.R")
source("scripts/presentable_table.R")
source("scripts/regression_table.R")
source("scripts/results_tables.R")

