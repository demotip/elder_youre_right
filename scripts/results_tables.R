# Using modelsummary to make tables for the results of the regressions,
# separated by group. 
# Also included tables for the results of the regressions w/ country fixed effects - 
# can include results for the logistic/ordered logistic regressions later
library(groundhog)
plot_30_yr <- c("cowplot", "tidyverse", "showtext", "sysfonts", "modelsummary", "flextable", "gt")
groundhog.library(plot_30_yr, "2021-11-01")

source("scripts/variable_labels.R")

age_diff_models_results <- readRDS("data_clean/age_diff_models.rds")

age_diff_models_results <- age_diff_models_results %>%
  filter(country == "All") %>%
  filter(term != "noncoeth") %>%
  filter(age_variable != "coarsened_age_10") %>%
  filter(age_variable != "coarsened_age_35_originalscale") %>% 
  filter(age_variable != "coarsened_age_30") %>%
  filter(age_variable != "coarsened_age_40") %>%
  mutate(age =
         case_when(grepl("old", ignore.case = T, term) ~
                     "Older interviewer on younger respondents",
                   grepl("young", ignore.case = T, term) ~
                     "Younger interviewer on older respondents",
                   TRUE ~ term))

results_tables <- lapply(unique(age_diff_models_results$group[!(age_diff_models_results$group == "youth_outcomes")]), function(x) {
  
  table <- age_diff_models_results %>%
    filter(group == x) %>%
    select(-outcome_variable, -group, -country, -age_variable,
            -p.value, -statistic, -n_obs, -upper, -lower) %>%
      pivot_wider(names_from = c(age, term),
    values_from = c(estimate, std.error),
    names_sep = ".") %>%
    # gt() %>%
    # tab_spanner(., label = "Young int. on old resp.", columns = contains("Younger interviewer")) %>%
    # tab_spanner(., label = "Old int. on young resp.", columns = contains("Older interviewer")) %>%
    # fmt_number(., columns = contains("respondents"),
    #            rows = everything(),
    #            decimals = 2) 
  # %>%
  #   # as_word() %>% #not compatible with this version of gt
  #   gtsave(., filename = paste0("tables/", x, "results.html"))
  #   
  return(table) }) %>%
  "names<-"(unique(age_diff_models_results$group[!(age_diff_models_results$group == "youth_outcomes")]))

saveRDS(results_tables, "tables/results_tables_adidafe")

### COUNTRY FIXED EFFECTS ----

age_diff_models_results_countryfe <- readRDS("data_clean/age_diff_models_countryfe.rds")

age_diff_models_results_countryfe <- age_diff_models_results_countryfe %>%
  filter(country == "All") %>%
  filter(term != "noncoeth") %>%
  filter(age_variable != "coarsened_age_10") %>%
  filter(age_variable != "coarsened_age_35_originalscale") %>% 
  filter(age_variable != "coarsened_age_30") %>%
  filter(age_variable != "coarsened_age_40") %>%
  mutate(age =
           case_when(grepl("old", ignore.case = T, term) ~
                       "Older interviewer on younger respondents",
                     grepl("young", ignore.case = T, term) ~
                       "Younger interviewer on older respondents",
                     TRUE ~ term))

results_tables_countryfe <- lapply(unique(age_diff_models_results_countryfe$group[!(age_diff_models_results_countryfe$group == "youth_outcomes")]), function(x) {
  
  table <- age_diff_models_results_countryfe %>%
    filter(group == x) %>%
    select(-outcome_variable, -group, -country, -age_variable,
           -p.value, -statistic, -n_obs, -upper, -lower) %>%
    pivot_wider(names_from = c(age, term),
                values_from = c(estimate, std.error),
                names_sep = ".") %>%
    # gt() %>%
    # tab_spanner(., label = "Young int. on old resp.", columns = contains("Younger interviewer")) %>%
    # tab_spanner(., label = "Old int. on young resp.", columns = contains("Older interviewer")) %>%
    # fmt_number(., columns = contains("respondents"),
    #            rows = everything(),
    #            decimals = 2) %>%
    # # as_word() %>% #not compatible with this version of gt
    # gtsave(., filename = paste0("tables/", x, "results_countryfe.html"))
  
  return(table) }) %>%
  "names<-"(unique(age_diff_models_results_countryfe$group[!(age_diff_models_results_countryfe$group == "youth_outcomes")]))

saveRDS(results_tables_countryfe, "tables/results_tables_countryfe")