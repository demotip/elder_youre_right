
library(groundhog)

contrasts <- c("glue", "lfe", "tidyverse", "lmtest", "sandwich", "modelsummary", "janitor")
groundhog.library(contrasts, "2021-11-01")

glance_custom.lm <- function(x, ...) {
  country_levels <- levels(as.factor(afpr$country))
  country_levels_formatted <- paste0("country",country_levels)
  
  # tribe_levels <- levels(as.factor(afpr$tribe))
  # tribe_levels_formatted <- paste0("tribe",tribe_levels)
  # 
  # enumeth_levels <- levels(as.factor(afpr$enumeth))
  # enumeth_levels_formatted <- paste0("enumeth",enumeth_levels)
  # 
  # region_levels <- levels(as.factor(afpr$region))
  # region_levels_formatted <- paste0("region",region_levels)
  
  if (country_levels_formatted %in% names(coef(x))) {
    out <- data.frame(Country = "✓")
  } else {
    out <- data.frame(Country = "✗")
  }
  
  # if (tribe_levels_formatted %in% names(coef(x))) {
  #   out_2 <- data.frame(Tribe = "✓")
  # } else {
  #   out_2 <- data.frame(Tribe = "✗")
  # }
  # 
  # if (region_levels_formatted %in% names(coef(x))) {
  #   out_3 <- data.frame(Region = "✓")
  # } else {
  #   out_3 <- data.frame(Region = "✗")
  # }
  # 
  # if (enumeth_levels_formatted %in% names(coef(x))) {
  #   out_4 <- data.frame(Enumeth = "✓")
  # } else {
  #   out_4 <- data.frame(Enumeth = "✗")
  # }
  return(out)
}

glance_custom.glm <- glance_custom.lm

### REGRESSION TABLES FOR ADIDA FIXED EFFECTS ----

outcome_age_combos_grouped <- outcome_age_combos %>%
  mutate(label = plyr::mapvalues(outcome, 
                                 paste0(variable_labels$var),
                                 as.character(variable_labels$label))) %>%
  # Make sure factor levels are correct for order of variables in plots
  mutate(label = factor(label, levels = variable_labels$label)) %>%
  # Create variable grouping column (i.e. stat_outcomes, pol_outcomes, etc.)
  mutate(group = plyr::mapvalues(outcome, 
                                 paste0(variable_labels$var),
                                 variable_labels$group)) #need the group specification here for the next function

model_function <- function(data_frame) { pmap(data_frame, function(outcome, age_variable, round) {
    
    if(round == "7") include_round <- 7 else include_round <- 3:4
    
    outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
    
    # Run model
    if(age_variable == "coarsened_age_10") {
      # Note: no contrasts for 10-year age difference model
      # because we only have one baseline of interest: same age.
      print(1)
      model <- felm(as.formula(glue(form_base)), data = afpr[afpr$round %in% include_round, ])
    } else {
      
      contrasts_matrix_list <- list(x = contrasts_matrix)
      names(contrasts_matrix_list) <- age_variable
      print(2)
      model <- felm(as.formula(glue(form_base)), data = afpr[afpr$round %in% include_round, ], 
                    contrasts = contrasts_matrix_list)
    }
    
    return(model)
  }) } 

cm <- c("(Intercept)" = "Constant",
        "coarsened_age_30older_int" = "30 split, interviewer older",
        "coarsened_age_35older_int" = "35 split, interviewer older",
        "coarsened_age_40older_int" = "40 split, interviewer older",
        "coarsened_age_30younger_int" = "30 split, interviewer younger",
        "coarsened_age_35younger_int" = "35 split, interviewer younger",
        "coarsened_age_40younger_int" = "40 split, interviewer younger",
        "coarsened_age_30not_relevant" = "30 split, both younger",
        "coarsened_age_35not_relevant" = "35 split, both younger",
        "coarsened_age_40not_relevant" = "40 split, both younger",
        "noncoeth" = "Non-coethnic",
        "age" = "Age",
        "gender" = "Male",
        "edu" = "Education level",
        "urban" = "Urban",
        "minority" = "Ethnic minority",
        "round" = "Round 4",
        "inhomelang" = "In-home language")

names_stat <- rep(stat_outcomes, each = 4)
names_stat <- janitor::make_clean_names(names_stat)

names_pol <- rep(pol_outcomes, each = 4)
names_pol <- janitor::make_clean_names(names_pol)

names_pro <- rep(pro_outcomes, each = 4)
names_pro <- janitor::make_clean_names(names_pro)

names_eth <- rep(eth_outcomes, each = 4)
names_eth <- janitor::make_clean_names(names_eth)

names_youth <- rep(youth_outcomes, each = 4)
names_youth <- janitor::make_clean_names(names_youth)

regression_output_stat <- outcome_age_combos_grouped %>%
    filter(group == "stat_outcomes") %>%
    select(-group, -label) %>%
    model_function() %>%
  saveRDS(., file = "data_clean/regression_output_stat")

regression_output_pol <- outcome_age_combos_grouped %>%
  filter(group == "pol_outcomes") %>%
  select(-group, -label) %>%
  model_function() %>%
  setNames(., names_pol) %>%
  saveRDS(., file = "data_clean/regression_output_pol")

regression_output_pro <- outcome_age_combos_grouped %>%
  filter(group == "pro_outcomes") %>%
  select(-group, -label) %>%
  model_function()  %>%
  setNames(., names_pol) %>%
  saveRDS(., file = "data_clean/regression_output_pro")

regression_output_eth <- outcome_age_combos_grouped %>%
  filter(group == "eth_outcomes") %>%
  select(-group, -label) %>%
  model_function()  %>% # Error: cannot allocate vector of size 49.6 Mb
  setNames(., names_eth) %>%
  saveRDS(., file = "data_clean/regression_output_eth")

regression_output_youth <- outcome_age_combos_grouped %>%
  filter(group == "youth_outcomes") %>%
  select(-group, -label) %>%
  model_function() %>%
  setNames(., names_youth) %>%
  saveRDS(., file = "data_clean/regression_output_youth")

# regression_output_list <- lapply(unique(outcome_age_combos_grouped$group), function(x) {
#   table <- outcome_age_combos_grouped %>%
#     filter(group == x) %>%
#     select(-group, -label) %>%
#     model_function()
# }) %>%
#   "names<-"(unique(outcome_age_combos_grouped$group))

# modelsummary(., coef_omit = "enumeth|region|tribe|coarsened_age_10Interviewer older (10 year)|coarsened_age_10Interviewer younger (10 year)",
#              coef_map = cm,
#              stars = TRUE)

#### REGRESSION TABLES FOR COUNTRY FIXED EFFECTS ----
#### This is the original code without splitting up the data by group - recode later 
# 
# model_function_countryfe <- function(data_frame) {
#   pmap(data_frame, function(outcome, age_variable, round) {
#     
#     if(round == "7") include_round <- 7 else include_round <- 3:4
#     
#     outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
#     
#     # Run model
#     if(age_variable == "coarsened_age_10") {
#       # Note: no contrasts for 10-year age difference model
#       # because we only have one baseline of interest: same age.
#       print(outcome_variable)
#       model <- felm(as.formula(glue(form_base_country)), data = afpr[afpr$round %in% include_round, ])
#     } else {
#       
#       contrasts_matrix_list <- list(x = contrasts_matrix)
#       names(contrasts_matrix_list) <- age_variable
#       print(outcome_variable)
#       model <- felm(as.formula(glue(form_base_country)), data = afpr[afpr$round %in% include_round, ], 
#                     contrasts = contrasts_matrix_list)
#     }
#     
#     return(model)
#   }) }
# 
# regression_tables_countryfe <- lapply(unique(outcome_age_combos_grouped$group), function(x) {
#   table <- outcome_age_combos_grouped %>%
#     filter(round == "3 and 4") %>% 
#     filter(group == x) %>%
#     select(-group, -label) %>%
#     model_function_countryfe() %>%
#     modelsummary(., coef_omit = "country|coarsened_age_10Interviewer older (10 year)|coarsened_age_10Interviewer younger (10 year)")
#   }) 
# 
# #### REGRESSION TABLES FOR LOGISTIC/ORDERED LOGISTIC FIXED EFFECTS ----
# 
# form_base_factor  <- paste0("as.factor({outcome_variable}) ~ {age_variable} + ",
#                             "noncoeth +", 
#                             "age + gender + edu + ",
#                             "urban + minority +",
#                             "round + inhomelang + country") #same as in cross_validated.R - defined again here for convenience
# 
# form_base_factor_round7  <- paste0("as.factor({outcome_variable}) ~ {age_variable} + ",
#                             "noncoeth +", 
#                             "age + gender + edu + ",
#                             "urban + minority +",
#                             "round + inhomelang") #round 7 outcomes will run without country fe
# 
# # splitting outcome_age_combos by round because the youth_outcomes can't run with country fixed effects
# # (Ask if I should exclude the round 7 question that was asked in all countries? addressing needs of youth?)
# 
# outcome_age_combos_grouped_3_4 <- outcome_age_combos_grouped %>%
#   filter(round == "3 and 4")
# 
# outcome_age_combos_grouped_7 <- outcome_age_combos_grouped %>%
#   filter(round == "7")
# 
# # This is the function for rounds 3 and 4
# model_function_logistic <- function(data_frame) { 
#   pmap(data_frame, function(outcome, age_variable, round) {
#   
#   if(round == "7") include_round <- 7 else include_round <- 3:4
#   
#   outcome_variable <- outcome # not standardizing variables
#   
#   print(outcome_variable)
#   
#   # Run model
#   if(age_variable == "coarsened_age_10") {
#     # Note: no contrasts for 10-year age difference model
#     # because we only have one baseline of interest: same age.
#     
#     if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
#       
#       print(2)
#       
#       model <- glm(as.formula(glue(form_base_factor)),
#                    data = afpr[afpr$round %in% include_round, ],
#                    family = "binomial")
#       
#     } else {
#       # Note: no contrasts for 10-year age difference model
#       # because we only have one baseline of interest: same age.
#       
#       print(3)
#       
#       model <- MASS::polr(as.formula(glue(form_base_factor)),
#                           method = "logistic",
#                           data = afpr[afpr$round %in% include_round, ],
#                           Hess = T)
#       }
#     
#   } else {
#     
#     contrasts_matrix_list <- list(x = contrasts_matrix)
#     names(contrasts_matrix_list) <- age_variable
#     
#     if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
#       print(5)
#       
#       model <- glm(as.formula(glue(form_base_factor)),
#                    data = afpr[afpr$round %in% include_round, ],
#                    # contrasts = contrasts_matrix_list
#                    family = "binomial")
#       
#     } else {
#       
#       print(6)
#       
#       model <- MASS::polr(as.formula(glue(form_base_factor)),
#                           method = "logistic",
#                           data = afpr[afpr$round %in% include_round, ],
#                           # contrasts = contrasts_matrix_list
#                           Hess = T)
#       
#       }
#     
#   }
#   
#   return(model)
# }) }
# 
# # This is the function for round 7
# model_function_logistic_7 <- function(data_frame) { 
#   pmap(data_frame, function(outcome, age_variable, round) {
#     
#     if(round == "7") include_round <- 7 else include_round <- 3:4
#     
#     outcome_variable <- outcome # not standardizing variables
#     
#     print(outcome_variable)
#     
#     # Run model
#     if(age_variable == "coarsened_age_10") {
#       # Note: no contrasts for 10-year age difference model
#       # because we only have one baseline of interest: same age.
#       
#       if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
#         
#         print(2)
#         
#         model <- glm(as.formula(glue(form_base_factor_round7)),
#                      data = afpr[afpr$round %in% include_round, ],
#                      family = "binomial")
#         
#       } else {
#         # Note: no contrasts for 10-year age difference model
#         # because we only have one baseline of interest: same age.
#         
#         print(3)
#         
#         model <- MASS::polr(as.formula(glue(form_base_factor_round7)),
#                             method = "logistic",
#                             data = afpr[afpr$round %in% include_round, ],
#                             Hess = T)
#       }
#       
#     } else {
#       
#       contrasts_matrix_list <- list(x = contrasts_matrix)
#       names(contrasts_matrix_list) <- age_variable
#       
#       if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
#         print(5)
#         
#         model <- glm(as.formula(glue(form_base_factor_round7)),
#                      data = afpr[afpr$round %in% include_round, ],
#                      # contrasts = contrasts_matrix_list
#                      family = "binomial")
#         
#       } else {
#         
#         print(6)
#         
#         model <- MASS::polr(as.formula(glue(form_base_factor_round7)),
#                             method = "logistic",
#                             data = afpr[afpr$round %in% include_round, ],
#                             # contrasts = contrasts_matrix_list
#                             Hess = T)
#         
#       }
#       
#     }
#     
#     return(model)
#   }) }
# 
# regression_tables_logistic_3_4 <- lapply(unique(outcome_age_combos_grouped_3_4$group), function(x) {
#   table <- outcome_age_combos_grouped_3_4 %>%
#     filter(group == x) %>%
#     select(-group, -label) %>%
#     model_function_logistic() %>%
#     modelsummary()
# }) 
# 
# # you don't need the lapply here
# regression_tables_logistic_7 <- lapply(unique(outcome_age_combos_grouped_7$group), function(x) {
#   table <- outcome_age_combos_grouped_7 %>%
#     filter(group == x) %>%
#     select(-group, -label) %>%
#     model_function_logistic_7() %>%
#     modelsummary()
# })
# 
# regression_tables_logistic_7$youth_outcomes
