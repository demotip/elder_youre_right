# These are the main results of the paper as logistic/ordered logistic
# This is the most current script as of 05-11-2024

# LOAD NECESSARY PACKAGES ----

library(groundhog)

contrasts_fe <- c("glue", "lfe", "tidyverse", "lmtest", 
               "sandwich", "MASS", "ordinal", "lme4")

library(alpaca)

groundhog.library(contrasts_fe, "2021-11-01")

# Load Adida et al. Afrobarometer data with age difference appended
# age differences appended in scripts/adida_append_age_differences.R
afpr <- readRDS("./data_clean/afpr_ages.rds")

# Get list of outcomes and dataframe with outcome descriptions
source("scripts/variable_labels.R")

# Convenience function, opposite of %in%
'%!in%' <- function(x,y)!('%in%'(x,y))

# Reverse code present living conditions so direction is consistent with other variables
afpr$z_ec_conditions_self <- as.vector(scale(6 - afpr$ec_conditions_self))

# According to Adida et al.:
# "The estimates reported in Figures 1 to 4 correspond with Model 4, 
# the model that includes the full set of controls."
# This being the case, we will only use the 4th model here.

# DEFINE CONTRASTS ----

# To define contrasts, our coarsened age variables need to be factors
# with pre-defined levels. 
if(!is.factor(afpr$coarsened_age_10) |
   !is.factor(afpr$coarsened_age_30) |
   !is.factor(afpr$coarsened_age_35) |
   !is.factor(afpr$coarsened_age_40) ) {
  stop("Make sure that coarsened age variables are factors.")
}

# SETTING UP CONTRASTS ----

# contrast() function takes the inverse of the matrix of
# desired contrast weights. So although there are only
# two contrasts we care about, we need a square matrix,
# so we specify three contrasts, plus a row 1/4s for the constant
not_relevant <- c(0 , 0, 1,  0)
younger_int <- c( -1,   1,  0,    0 )
older_int <- c( 0,   0,   -1,   1 )
# Get inverse of contrast matrix and remove constant
contrasts_matrix <- solve(rbind(constant=1/4, not_relevant, younger_int, older_int))[ , -1]

# DEFINE MODEL FORMULA ----
# Removed  "| region + tribe + enumeth" from end of formula

form_base_logistic  <- paste0("as.factor({outcome_variable})",
                            " ~ {age_variable} + ",
                            "noncoeth +", 
                            "age + gender + edu + ",
                            "urban + minority +",
                            "round + inhomelang |region + tribe + enumeth")

form_base_factor  <- paste0("as.factor({outcome_variable})",
                            " ~ {age_variable} + ",
                            "noncoeth +", 
                            "age + gender + edu + ",
                            "urban + minority +",
                            "round + inhomelang + (1|region) + (1|tribe) + (1|enumeth)")

# Generate all outcome-coarsened age combinations
outcome_age_combos_3_4 <- 
  expand.grid(outcome = c(pro_outcomes, pol_outcomes, stat_outcomes, eth_outcomes), 
              stringsAsFactors = FALSE,
              age_variable = c("coarsened_age_10", 
                               "coarsened_age_30",
                               "coarsened_age_35",
                               "coarsened_age_40")
  ) %>%
  mutate(round = "3 and 4")  

# Youth outcomes apply only to Round 7
outcome_age_combos_7 <- 
  expand.grid(outcome = youth_outcomes, 
              stringsAsFactors = FALSE,
              age_variable = c("coarsened_age_10", 
                               "coarsened_age_30",
                               "coarsened_age_35", 
                               "coarsened_age_40")
  ) %>%
  mutate(round = "7") # redefine as numeric



lengths(unique(afpr[,"hostile"]), use.names = FALSE)

# Combine for full set of outcome-coarsened_age-round combinations
outcome_age_combos <- bind_rows(outcome_age_combos_3_4, outcome_age_combos_7)

age_diff_models_fe <- 
  pmap_dfr(outcome_age_combos, function(outcome, age_variable, round) {
    
    if(round == 7) {
      include_round <- 7} else {
        include_round <- 3:4}
    
    outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
    
    # Run model
    if(age_variable == "coarsened_age_10") {
      
      if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
        
        print(outcome_variable)
        print("1")
        
        model <- feglm(as.formula(glue(form_base_logistic)),
                     data = afpr[afpr$round %in% include_round, ],
                     family = binomial())
      } else {
        # Note: no contrasts for 10-year age difference model
        # because we only have one baseline of interest: same age.
        
        print(outcome_variable)
        print("2")
        
        model <- ordinal::clmm(as.formula(glue(form_base_factor)),
                            method = "logistic",
                            data = afpr[afpr$round %in% include_round, ],
                            Hess = T
        ) }
    } else {
      
      contrasts_matrix_list <- list(x = contrasts_matrix)
      names(contrasts_matrix_list) <- age_variable
      
      if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
        
        print(outcome_variable)
        print("3")
        
        model <- glmer(as.formula(glue(form_base_logistic)),
                     data = afpr[afpr$round %in% include_round, ],
                     family = binomial(), 
                     contrasts = contrasts_matrix_list
                     )
      } else {
        
        print(outcome_variable)
        print("4")
        
        model <- ordinal::clmm(as.formula(glue(form_base_factor)),
                            method = "logistic",
                            data = afpr[afpr$round %in% include_round,],
                            contrasts = contrasts_matrix_list,
                            Hess = T
        ) }
    }
    
    n_obs <- nrow(model$response)
    
    model <- model %>%
      # Extract estimates with robust standard errors
      summary(., robust = TRUE) %>%
      "$"(coefficients) %>%
      data.frame() %>%
      rownames_to_column() %>%
      "names<-"(c("term", "estimate", "std.error", "statistic", "p.value")) %>%
      # Get just coefficients of interest
      filter(grepl("older_int|younger_int|Interviewer|noncoeth", term)) %>%
      # Add identifiers for outcome and age difference variable
      mutate(n_obs = n_obs,
             outcome_variable = outcome_variable,
             age_variable = age_variable) %>%
      # Calculate upper and lower confidence bands
      mutate(upper = estimate + qnorm(.975)*std.error,
             lower = estimate - qnorm(.975)*std.error)
    
    return(model)
  }) %>%
  # Create variable labels column
  mutate(label = plyr::mapvalues(outcome_variable, 
                                 paste0("z_", variable_labels$var),
                                 as.character(variable_labels$label))) %>%
  # Make sure factor levels are correct for order of variables in plots
  mutate(label = factor(label, levels = variable_labels$label)) %>%
  # Create variable grouping column (i.e. stat_outcomes, pol_outcomes, etc.)
  mutate(group = plyr::mapvalues(outcome_variable, 
                                 paste0("z_", variable_labels$var),
                                 variable_labels$group))

# RUN ROUND 7 MODELS FOR JUST MAURITIUS, AND BOTH ----

outcome_age_combos_mauritius <-
  expand.grid(outcome = youth_outcomes,
              age_variable = c("coarsened_age_10",
                               "coarsened_age_30",
                               "coarsened_age_35",
                               "coarsened_age_40"),
              stringsAsFactors = FALSE)

age_diff_models_mauritius <-
  pmap_dfr(outcome_age_combos_mauritius, function(outcome, age_variable) {
    
    outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
    
    # Run model
    if(age_variable == "coarsened_age_10") {
      # Note: no contrasts for 10-year age difference model
      # because we only have one baseline of interest: same age.
      
      if(length(unique(afpr[{outcome_variable},])==2)) {
        
        model <- glm(as.formula(glue(form_base_factor)),
                     data = afpr[afpr$round == 7 & afpr$country %in% "Mauritius", ],
                     family = "binomial")
      } else {
        # Note: no contrasts for 10-year age difference model
        # because we only have one baseline of interest: same age.
        
        model <- ordinal::clmm(as.formula(glue(form_base_factor)),
                            method = "logistic",
                            data = afpr[afpr$round == 7 & afpr$country %in% "Mauritius", ],
                            Hess = T
        ) }
      
    } else {
      
      contrasts_matrix_list <- list(x = contrasts_matrix)
      names(contrasts_matrix_list) <- age_variable
      
      if(length(unique(afpr[{outcome_variable},])==2)) {
        
        model <- glm(as.formula(glue(form_base_factor)),
                     data = afpr[afpr$round == 7 & afpr$country %in% "Mauritius", ],
                     family = "binomial",
                     contrasts = contrasts_matrix_list
        )
      } else {
        
        model <- ordinal::clmm(as.formula(glue(form_base_factor)),
                            method = "logistic",
                            data = afpr[afpr$round == 7 & afpr$country %in% "Mauritius", ],
                            Hess = T,
                            contrasts = contrasts_matrix_list
        ) }
    }
    
    n_obs <- nrow(model$response)
    
    model <- model %>%
      # Extract estimates with robust standard errors
      summary(., robust = TRUE) %>%
      "$"(coefficients) %>%
      # Clean up into tidy dataframe
      data.frame() %>%
      rownames_to_column() %>%
      "names<-"(c("term", "estimate", "std.error", "statistic", "p.value")) %>%
      # Get just coefficients of interest
      filter(grepl("older_int|younger_int|Interviewer|noncoeth", term)) %>%
      # Add identifiers for outcome and age difference variable
      mutate(n_obs = n_obs,
             outcome_variable = outcome_variable,
             age_variable = age_variable) %>%
      # Calculate upper and lower confidence bands
      mutate(upper = estimate + qnorm(.975)*std.error,
             lower = estimate - qnorm(.975)*std.error)
    
    return(model)
  }) %>%
  # Create variable labels column
  mutate(label = plyr::mapvalues(outcome_variable,
                                 paste0("z_", variable_labels$var),
                                 as.character(variable_labels$label))) %>%
  # Make sure factor levels are correct for order of variables in plots
  mutate(label = factor(label, levels = variable_labels$label)) %>%
  # Create variable grouping column (i.e. stat_outcomes, pol_outcomes, etc.)
  mutate(group = plyr::mapvalues(outcome_variable,
                                 paste0("z_", variable_labels$var),
                                 variable_labels$group))


age_diff_models_mauritius <- age_diff_models_mauritius %>%
  mutate(country = "Mauritius")

# RUN ALL MODELS WITH UNSTANDARIZED OUTCOME ----
# i.e. outcomes on original scale

age_diff_models_original_scale <- outcome_age_combos %>%
  filter(age_variable == "coarsened_age_35") %>%
  pmap_dfr(function(outcome, age_variable, round) {
    
    if(round == "7") include_round <- 7 else include_round <- 3:4
    outcome_variable <- outcome
    
    # Run model
    contrasts_matrix_list <- list(x = contrasts_matrix)
    names(contrasts_matrix_list) <- age_variable
    
    if(length(unique(afpr[{outcome_variable},])==2)) {
      
      model <- glm(as.formula(glue(form_base_factor)),
                   data = afpr[afpr$round %in% include_round, ],
                   family = "binomial",
                   contrasts = contrasts_matrix_list)
    } else {
      # Note: no contrasts for 10-year age difference model
      # because we only have one baseline of interest: same age.
      
      model <- ordinal::clmm(as.formula(glue(form_base_factor)),
                          method = "logistic",
                          data = afpr[afpr$round %in% include_round, ],
                          Hess = T,
                          contrasts = contrasts_matrix_list
      ) }
    
    n_obs <- nrow(model$response)
    
    model <- model %>%
      # Extract estimates with robust standard errors
      summary(., robust = TRUE) %>%
      "$"(coefficients) %>%
      # Clean up into tidy dataframe
      data.frame() %>%
      rownames_to_column() %>%
      "names<-"(c("term", "estimate", "std.error", "statistic", "p.value")) %>%
      # Get just coefficients of interest
      filter(grepl("older_int|younger_int|Interviewer|noncoeth", term)) %>%
      # Add identifiers for outcome and age difference variable
      mutate(n_obs = n_obs,
             outcome_variable = outcome_variable,
             age_variable = paste0(age_variable, "_originalscale")) %>%
      # Calculate upper and lower confidence bands
      mutate(upper = estimate + qnorm(.975)*std.error,
             lower = estimate - qnorm(.975)*std.error)
    
    return(model)
  }) %>%
  # Create variable labels column
  mutate(label = plyr::mapvalues(outcome_variable, 
                                 variable_labels$var,
                                 as.character(variable_labels$label))) %>%
  # Make sure factor levels are correct for order of variables in plots
  mutate(label = factor(label, levels = variable_labels$label)) %>%
  # Create variable grouping column (i.e. stat_outcomes, pol_outcomes, etc.)
  mutate(group = plyr::mapvalues(outcome_variable, 
                                 variable_labels$var,
                                 variable_labels$group))

# RUN ROUND 7 MODELS FOR JUST MAURITIUS, AND BOTH ----

outcome_age_combos_mauritius <-
  expand.grid(outcome = youth_outcomes,
              age_variable = c("coarsened_age_10",
                               "coarsened_age_30",
                               "coarsened_age_35",
                               "coarsened_age_40"),
              stringsAsFactors = FALSE)

age_diff_models_mauritius <-
  pmap_dfr(outcome_age_combos_mauritius, function(outcome, age_variable) {
    
    outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
    
    # Run model
    if(age_variable == "coarsened_age_10") {
      # Note: no contrasts for 10-year age difference model
      # because we only have one baseline of interest: same age.
      
      if(length(unique(afpr[{outcome_variable},])==2)) {
        
        model <- glm(as.formula(glue(form_base_factor)),
                     data = afpr[afpr$round == 7 & 
                                   afpr$country %in% "Mauritius", ],
                     family = "binomial")
      } else {
        # Note: no contrasts for 10-year age difference model
        # because we only have one baseline of interest: same age.
        
        model <- ordinal::clmm(as.formula(glue(form_base_factor)),
                            method = "logistic",
                            data = afpr[afpr$round == 7 & 
                                          afpr$country %in% "Mauritius", ],
                            Hess = T) }
      
    } else {
      
      contrasts_matrix_list <- list(x = contrasts_matrix)
      names(contrasts_matrix_list) <- age_variable
      
      if(length(unique(afpr[{outcome_variable},])==2)) {
        
        model <- glm(as.formula(glue(form_base_factor)),
                     data = afpr[afpr$round == 7 & 
                                   afpr$country %in% "Mauritius", ],
                     family = "binomial",
                     contrasts = contrasts_matrix_list)
      } else {
        # Note: no contrasts for 10-year age difference model
        # because we only have one baseline of interest: same age.
        
        model <- ordinal::clmm(as.formula(glue(form_base_factor)),
                            method = "logistic",
                            data = afpr[afpr$round == 7 & 
                                          afpr$country %in% "Mauritius", ],
                            Hess = T,
                            contrasts = contrasts_matrix_list) }
    }
    
    n_obs <- nrow(model$response)
    
    model <- model %>%
      # Extract estimates with robust standard errors
      summary(., robust = TRUE) %>%
      "$"(coefficients) %>%
      # Clean up into tidy dataframe
      data.frame() %>%
      rownames_to_column() %>%
      "names<-"(c("term", "estimate", "std.error", "statistic", "p.value")) %>%
      # Get just coefficients of interest
      filter(grepl("older_int|younger_int|Interviewer|noncoeth", term)) %>%
      # Add identifiers for outcome and age difference variable
      mutate(n_obs = n_obs,
             outcome_variable = outcome_variable,
             age_variable = age_variable) %>%
      # Calculate upper and lower confidence bands
      mutate(upper = estimate + qnorm(.975)*std.error,
             lower = estimate - qnorm(.975)*std.error)
    
    return(model)
  }) %>%
  # Create variable labels column
  mutate(label = plyr::mapvalues(outcome_variable,
                                 paste0("z_", variable_labels$var),
                                 as.character(variable_labels$label))) %>%
  # Make sure factor levels are correct for order of variables in plots
  mutate(label = factor(label, levels = variable_labels$label)) %>%
  # Create variable grouping column (i.e. stat_outcomes, pol_outcomes, etc.)
  mutate(group = plyr::mapvalues(outcome_variable,
                                 paste0("z_", variable_labels$var),
                                 variable_labels$group))


age_diff_models_mauritius <- age_diff_models_mauritius %>%
  mutate(country = "Mauritius")

# RUN ALL MODELS WITH UNSTANDARIZED OUTCOME ----
# i.e. outcomes on original scale

age_diff_models_original_scale <- outcome_age_combos %>%
  filter(age_variable == "coarsened_age_35") %>%
  pmap_dfr(function(outcome, age_variable, round) {
    
    if(round == "7") include_round <- 7 else include_round <- 3:4
    outcome_variable <- outcome
    
    # Run model
    contrasts_matrix_list <- list(x = contrasts_matrix)
    names(contrasts_matrix_list) <- age_variable
    
    if(length(unique(afpr[{outcome_variable},])==2)) {
      
      model <- glm(as.formula(glue(form_base_factor)),
                   data = afpr[afpr$round %in% include_round, ],
                   family = "binomial",
                   contrasts = contrasts_matrix_list)
    } else {
      # Note: no contrasts for 10-year age difference model
      # because we only have one baseline of interest: same age.
      
      model <- ordinal::clmm(as.formula(glue(form_base_factor)),
                          method = "logistic",
                          data = afpr[afpr$round %in% include_round, ],
                          Hess = T,
                          contrasts = contrasts_matrix_list) }
    
    n_obs <- nrow(model$response)
    
    model <- model %>%
      # Extract estimates with robust standard errors
      summary(., robust = TRUE) %>%
      "$"(coefficients) %>%
      # Clean up into tidy dataframe
      data.frame() %>%
      rownames_to_column() %>%
      "names<-"(c("term", "estimate", "std.error", "statistic", "p.value")) %>%
      # Get just coefficients of interest
      filter(grepl("older_int|younger_int|Interviewer|noncoeth", term)) %>%
      # Add identifiers for outcome and age difference variable
      mutate(n_obs = n_obs,
             outcome_variable = outcome_variable,
             age_variable = paste0(age_variable, "_originalscale")) %>%
      # Calculate upper and lower confidence bands
      mutate(upper = estimate + qnorm(.975)*std.error,
             lower = estimate - qnorm(.975)*std.error)
    
    return(model)
  }) %>%
  # Create variable labels column
  mutate(label = plyr::mapvalues(outcome_variable, 
                                 variable_labels$var,
                                 as.character(variable_labels$label))) %>%
  # Make sure factor levels are correct for order of variables in plots
  mutate(label = factor(label, levels = variable_labels$label)) %>%
  # Create variable grouping column (i.e. stat_outcomes, pol_outcomes, etc.)
  mutate(group = plyr::mapvalues(outcome_variable, 
                                 variable_labels$var,
                                 variable_labels$group))

rbind(age_diff_models, age_diff_models_original_scale) %>%
  mutate(country = "All") %>%
  bind_rows(age_diff_models_mauritius) %>%
  saveRDS(., "data_clean/age_diff_models_logistic.rds")