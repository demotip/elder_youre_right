source("scripts/contrasts.R")
source("scripts/variable_labels.R")
age_diff_models_og <- readRDS("data_clean/age_diff_models.rds")

# LOAD NECESSARY PACKAGES ----

library(groundhog)

contrasts <- c("glue", "lfe", "tidyverse", "lmtest", "sandwich", "modelsummary")
groundhog.library(contrasts, "2021-11-01")

# Comparing original fixed effects to country fixed effects ----

# Comparing polr/glm to lm

