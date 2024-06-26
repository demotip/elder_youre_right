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

model_function <- function(data_frame) {
  pmap(data_frame, function(outcome, age_variable, round) {
    
    if(round == "7") include_round <- 7 else include_round <- 3:4
    
    outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
    
    # Run model
    if(age_variable == "coarsened_age_10") {
      # Note: no contrasts for 10-year age difference model
      # because we only have one baseline of interest: same age.
      model <- felm(as.formula(glue(form_base)), data = afpr[afpr$round %in% include_round, ])
    } else {
      
      contrasts_matrix_list <- list(x = contrasts_matrix)
      names(contrasts_matrix_list) <- age_variable
      
      model <- felm(as.formula(glue(form_base)), data = afpr[afpr$round %in% include_round, ], 
                    contrasts = contrasts_matrix_list)
    }
    # model <- model %>% summary(., robust = TRUE) # IS THIS THE RIGHT SPECIFICATION FOR ROBUST??
    
    return(model)
  }) }

regression_tables <- lapply(unique(outcome_age_combos_grouped$group), function(x) {
  table <- outcome_age_combos_grouped %>%
    filter(group == x) %>%
    select(-group, -label) %>%
    model_function() %>%
    modelsummary()
}) %>%
  "names<-"(unique(outcome_age_combos_grouped$group))

#### REGRESSION TABLES FOR COUNTRY FIXED EFFECTS ----

model_function_countryfe <- function(data_frame) {
  pmap(data_frame, function(outcome, age_variable, round) {
    
    if(round == "7") include_round <- 7 else include_round <- 3:4
    
    outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
    
    # Run model
    if(age_variable == "coarsened_age_10") {
      # Note: no contrasts for 10-year age difference model
      # because we only have one baseline of interest: same age.
      model <- felm(as.formula(glue(form_base_country)), data = afpr[afpr$round %in% include_round, ])
    } else {
      
      contrasts_matrix_list <- list(x = contrasts_matrix)
      names(contrasts_matrix_list) <- age_variable
      
      model <- felm(as.formula(glue(form_base_country)), data = afpr[afpr$round %in% include_round, ], 
                    contrasts = contrasts_matrix_list)
    }
    
    return(model)
  }) }

regression_tables_countryfe <- lapply(unique(outcome_age_combos_grouped$group), function(x) {
  table <- outcome_age_combos_grouped %>%
    filter(group == x) %>%
    select(-group, -label) %>%
    model_function_countryfe() %>%
    # `names<-`(.,) #the names are tricky here - ask AE? 
    modelsummary()
}) %>%
  "names<-"(unique(outcome_age_combos_grouped$group))

#### REGRESSION TABLES FOR LOGISTIC/ORDERED LOGISTIC FIXED EFFECTS ----

form_base_factor  <- paste0("as.factor({outcome_variable}) ~ {age_variable} + ",
                            "noncoeth +", 
                            "age + gender + edu + ",
                            "urban + minority +",
                            "round + inhomelang + country") #same as in cross_validated.R - defined again here for convenience

form_base_factor_round7  <- paste0("as.factor({outcome_variable}) ~ {age_variable} + ",
                            "noncoeth +", 
                            "age + gender + edu + ",
                            "urban + minority +",
                            "round + inhomelang") #round 7 outcomes will run without country fe

# This should also be unstandardized but clarify that with AE to make sure

# splitting outcome_age_combos by round because the youth_outcomes can't run with country fixed effects

outcome_age_combos_grouped_3_4 <- outcome_age_combos_grouped %>%
  filter(round == "3 and 4")

outcome_age_combos_grouped_7 <- outcome_age_combos_grouped %>%
  filter(round == "7")

# This is the function for rounds 3 and 4
model_function_logistic <- function(data_frame) { 
  pmap(data_frame, function(outcome, age_variable, round) {
  
  if(round == "7") include_round <- 7 else include_round <- 3:4
  
  outcome_variable <- outcome # not standardizing variables
  
  print(outcome_variable)
  
  # Run model
  if(age_variable == "coarsened_age_10") {
    # Note: no contrasts for 10-year age difference model
    # because we only have one baseline of interest: same age.
    
    if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
      
      print(2)
      
      model <- glm(as.formula(glue(form_base_factor)),
                   data = afpr[afpr$round %in% include_round, ],
                   family = "binomial")
      
    } else {
      # Note: no contrasts for 10-year age difference model
      # because we only have one baseline of interest: same age.
      
      print(3)
      
      model <- MASS::polr(as.formula(glue(form_base_factor)),
                          method = "logistic",
                          data = afpr[afpr$round %in% include_round, ],
                          Hess = T)
      }
    
  } else {
    
    contrasts_matrix_list <- list(x = contrasts_matrix)
    names(contrasts_matrix_list) <- age_variable
    
    if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
      print(5)
      
      model <- glm(as.formula(glue(form_base_factor)),
                   data = afpr[afpr$round %in% include_round, ],
                   # contrasts = ????
                   family = "binomial")
      
    } else {
      
      print(6)
      
      model <- MASS::polr(as.formula(glue(form_base_factor)),
                          method = "logistic",
                          data = afpr[afpr$round %in% include_round, ],
                          # contrasts = ????
                          Hess = T)
      
      }
    
  }
  
  return(model)
}) }

# This is the function for round 7
model_function_logistic_7 <- function(data_frame) { 
  pmap(data_frame, function(outcome, age_variable, round) {
    
    if(round == "7") include_round <- 7 else include_round <- 3:4
    
    outcome_variable <- outcome # not standardizing variables
    
    print(outcome_variable)
    
    # Run model
    if(age_variable == "coarsened_age_10") {
      # Note: no contrasts for 10-year age difference model
      # because we only have one baseline of interest: same age.
      
      if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
        
        print(2)
        
        model <- glm(as.formula(glue(form_base_factor_round7)),
                     data = afpr[afpr$round %in% include_round, ],
                     family = "binomial")
        
      } else {
        # Note: no contrasts for 10-year age difference model
        # because we only have one baseline of interest: same age.
        
        print(3)
        
        model <- MASS::polr(as.formula(glue(form_base_factor_round7)),
                            method = "logistic",
                            data = afpr[afpr$round %in% include_round, ],
                            Hess = T)
      }
      
    } else {
      
      contrasts_matrix_list <- list(x = contrasts_matrix)
      names(contrasts_matrix_list) <- age_variable
      
      if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
        print(5)
        
        model <- glm(as.formula(glue(form_base_factor_round7)),
                     data = afpr[afpr$round %in% include_round, ],
                     # contrasts = ????
                     family = "binomial")
        
      } else {
        
        print(6)
        
        model <- MASS::polr(as.formula(glue(form_base_factor_round7)),
                            method = "logistic",
                            data = afpr[afpr$round %in% include_round, ],
                            # contrasts = ????
                            Hess = T)
        
      }
      
    }
    
    return(model)
  }) }

regression_tables_logistic_3_4 <- lapply(unique(outcome_age_combos_grouped_3_4$group), function(x) {
  table <- outcome_age_combos_grouped_3_4 %>%
    filter(group == x) %>%
    select(-group, -label) %>%
    model_function_logistic() %>%
    # `names<-`(.,) #the names are tricky here - ask AE? 
    modelsummary()
}) %>%
  "names<-"(unique(outcome_age_combos_grouped_3_4$group))

# you honestly don't need the lapply here bc there's only one group but go off
regression_tables_logistic_7 <- lapply(unique(outcome_age_combos_grouped_7$group), function(x) {
  table <- outcome_age_combos_grouped_7 %>%
    filter(group == x) %>%
    select(-group, -label) %>%
    model_function_logistic_7() %>%
    # `names<-`(.,) #the names are tricky here - ask AE? 
    modelsummary()
}) %>%
  "names<-"(unique(outcome_age_combos_grouped_7$group))
