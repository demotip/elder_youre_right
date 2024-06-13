# Using modelsummary to make tables for the results of the regressions,
# separated by group. 
# Also included tables for the results of the regressions w/ country fixed effects - 
# can include results for the logistic/ordered logistic regressions later

plot_30_yr <- c("cowplot", "tidyverse", "showtext", "sysfonts", "modelsummary", "flextable")
groundhog.library(plot_30_yr, "2021-11-01")

source("scripts/variable_labels.R")

age_diff_models_results <- readRDS("data_clean/age_diff_models.rds")

age_diff_models_results <- age_diff_models_results %>%
  filter(country == "All") %>%
  filter(term != "noncoeth") %>%
  filter(age_variable != "coarsened_age_10") %>%
  filter(age_variable != "coarsened_age_35_originalscale") %>% 
  mutate(age =
         case_when(grepl("old", ignore.case = T, term) ~
                     "Older interviewer on younger respondents",
                   grepl("young", ignore.case = T, term) ~
                     "Younger interviewer on older respondents",
                   TRUE ~ term))

results_tables <- lapply(unique(age_diff_models_results$group[!(age_diff_models_results$group == "youth_outcomes")]), function(x) {
  
  table <- age_diff_models_results %>%
    filter(group == x) %>%
    select(-outcome_variable, -age_variable, -group, -country,
           -term, -std.error, -statistic, -n_obs, -upper, -lower) %>%
    datasummary_df(., paste0("tables/", x, "results.docx"))
  
  return(table) }) %>%
  "names<-"(unique(age_diff_models_results$group[!(age_diff_models_results$group == "youth_outcomes")]))

### COUNTRY FIXED EFFECTS ----

age_diff_models_results_countryfe <- readRDS("data_clean/age_diff_models_countryfe.rds")

age_diff_models_results_countryfe <- age_diff_models_results_countryfe %>%
  filter(country == "All") %>%
  filter(term != "noncoeth") %>%
  filter(age_variable != "coarsened_age_10") %>%
  filter(age_variable != "coarsened_age_35_originalscale") %>% mutate(age =
         case_when(grepl("old", ignore.case = T, term) ~
                     "Older interviewer on younger respondents",
                   grepl("young", ignore.case = T, term) ~
                     "Younger interviewer on older respondents",
                   TRUE ~ term))

results_tables_countryfe <- lapply(unique(age_diff_models_results_countryfe$group[!(age_diff_models_results_countryfe$group == "youth_outcomes")]), function(x) {
  
  table <- age_diff_models_results_countryfe %>%
    filter(group == x) %>%
    select(-outcome_variable, -age_variable, -group, -country,
           -term, -std.error, -statistic, -n_obs, -upper, -lower) %>%
    datasummary_df(., paste0("tables/", x, "results_countryfe.docx"))
  
  return(table) }) %>%
  "names<-"(unique(age_diff_models_results_countryfe$group[!(age_diff_models_results_countryfe$group == "youth_outcomes")]))

### LOGISTIC/ORDERED LOGISTIC ----
# 
# age_diff_models_results_logistic <- readRDS("data_clean/age_diff_models_logistic.rds")
# 
# age_diff_models_results_logistic <- age_diff_models_results_logistic %>%
#   filter(country == "All") %>%
#   filter(term != "noncoeth") %>%
#   filter(age_variable != "coarsened_age_10") %>%
#   filter(age_variable != "coarsened_age_35_originalscale")
# 
# results_tables_logistic <- lapply(unique(age_diff_models_results_logistic$group[!(age_diff_models_results_logistic$group == "youth_outcomes")]), function(x) {
#   
#   table <- age_diff_models_results_logistic %>%
#     filter(group == x) %>%
#     select(-outcome_variable, -age_variable, -group, -country) %>%
#     datasummary_df(., paste0("tables/", x, "results_logistic.docx"))
#   
#   return(table) }) %>%
#   "names<-"(unique(age_diff_models_results_logistic$group[!(age_diff_models_results_logistic$group == "youth_outcomes")]))
