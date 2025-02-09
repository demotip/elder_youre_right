# Using modelsummary to make tables for the results of the regressions,
# separated by group and using the data frame format calculated in contrasts.R.

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

results_tables <- lapply(unique(age_diff_models_results$group), function(x) {
  
  table <- age_diff_models_results %>%
    filter(group == x) %>%
    dplyr::select(-outcome_variable, -group, -country, -age_variable,
           -statistic, -n_obs, -upper, -lower) %>%
    pivot_wider(names_from = c(age, term),
                values_from = c(estimate, std.error, p.value),
                names_sep = ".") %>%
    gt() %>%
    tab_spanner(., label = "Young int. on old resp.", columns = contains("Younger interviewer")) %>%
    tab_spanner(., label = "Old int. on young resp.", columns = contains("Older interviewer")) %>%
    fmt_number(., columns = contains("respondents"),
               rows = everything(),
               decimals = 3) %>%
    # as_word() %>% #not compatible with this version of gt
    gtsave(., filename = paste0("tables/results_tables/", x, "results_adidafe.html"))
  
  return(table) }) %>%
  "names<-"(unique(age_diff_models_results$group))

# saveRDS(results_tables, "tables/results_tables/results_tables_adidafe")

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

results_tables_countryfe <- lapply(unique(age_diff_models_results_countryfe$group), function(x) {
  
  table <- age_diff_models_results_countryfe %>%
    filter(group == x) %>%
    dplyr::select(-outcome_variable, -group, -country, -age_variable,
            -statistic, -n_obs, -upper, -lower) %>%
    pivot_wider(names_from = c(age, term),
                values_from = c(estimate, std.error, p.value),
                names_sep = ".") %>%
    gt() %>%
    tab_spanner(., label = "Young int. on old resp.", columns = contains("Younger interviewer")) %>%
    tab_spanner(., label = "Old int. on young resp.", columns = contains("Older interviewer")) %>%
    fmt_number(., columns = contains("respondents"),
               rows = everything(),
               decimals = 3) %>%
    # as_word() %>% #not compatible with this version of gt
    gtsave(., filename = paste0("tables/results_tables/", x, "results_countryfe.html"))
  
  return(table) }) %>%
  "names<-"(unique(age_diff_models_results_countryfe$group))

# saveRDS(results_tables_countryfe, "tables/results_tables/results_tables_countryfe")

# Saving a csv of the country coefficients ----

countrycoefs <- age_diff_models_results_countryfe %>%
  dplyr::select(label, estimate, age_variable, age) %>%
  write.csv(., "tables/country_coefs.csv")
