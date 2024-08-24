source("scripts/contrasts.R")
source("scripts/variable_labels.R")

# Loading necessary packages ----

library(groundhog)

cv <- c("glue", "lfe", "tidyverse", "lmtest", 
        "sandwich", "modelsummary", "boot", "caret", "rms", "htmlTable")

groundhog.library(cv, "2021-11-01")

# Setting up fixed effects as factors ----

afpr$region <- as.factor(afpr$region)
afpr$tribe <- as.factor(afpr$tribe)
afpr$enumeth <- as.factor(afpr$enumeth)
afpr$country <- as.factor(afpr$country)

# All the forms ----

form_base_factor  <- paste0("as.factor({outcome_variable}) ~ {age_variable} + ",
                            "noncoeth +", 
                            "age + gender + edu + ",
                            "urban + minority +",
                            "round + inhomelang + country") # removed  "+ region + tribe + enumeth"

form_base_country  <- paste0("{outcome_variable} ~ {age_variable} + ",
                             "noncoeth +", 
                             "age + gender + edu + ",
                             "urban + minority +",
                             "round + inhomelang + country") # removed + country

# The most recent function with rms, split by round ----

cv_lin_log_3_4 <- 
  pmap_dfr(outcome_age_combos_3_4, function(outcome, age_variable, round) {
    
    if(round == "7") include_round <- 7 else include_round <- 3:4
    
    outcome_variable <- outcome #not standardizing variables
    
    print(outcome_variable)
    
    # Run model
    if(age_variable == "coarsened_age_10") {
      
      model_lin <- rms::ols(as.formula(glue(form_base_country)), data = 
                              droplevels(afpr[afpr$round %in% include_round & 
                                                complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                         "noncoeth", "age", "gender", "edu", "urban", 
                                                                         "minority", "round", "inhomelang", "country")]), ]),
                            x = TRUE,  # need x and y to be set to TRUE
                            y = TRUE
      )
      
      if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
        
        model_log <- rms::lrm(as.formula(glue(form_base_factor)),
                              data = droplevels(afpr[afpr$round %in% include_round & 
                                                       complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                                "noncoeth", "age", "gender", "edu", "urban", 
                                                                                "minority", "round", "inhomelang", "country")]), ]),
                              x = TRUE,
                              y = TRUE)
      } else {
        
        model_log <- rms::orm(as.formula(glue(form_base_factor)),
                              data = afpr[afpr$round %in% include_round & 
                                            complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                     "noncoeth", "age", "gender", "edu", "urban", 
                                                                     "minority", "round", "inhomelang", "country")]), ],
                              x = TRUE,
                              y = TRUE ) }
      
    } else {
      
      model_lin <- rms::ols(as.formula(glue(form_base_country)), data =
                              droplevels(afpr[afpr$round %in% include_round & 
                                                complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                         "noncoeth", "age", "gender", "edu", "urban", 
                                                                         "minority", "round", "inhomelang", "country")]), ]),
                            x = TRUE,
                            y = TRUE
      ) 
      
      if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
        
        model_log <- rms::lrm(as.formula(glue(form_base_factor)),
                              droplevels(afpr[afpr$round %in% include_round & 
                                                complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                         "noncoeth", "age", "gender", "edu", "urban", 
                                                                         "minority", "round", "inhomelang", "country")]), ]),
                              x = TRUE,
                              y = TRUE
        )
      } else {
        
        model_log <- rms::orm(as.formula(glue(form_base_factor)),
                              data = afpr[afpr$round %in% include_round & 
                                            complete.cases(afpr[ , c({outcome_variable},{age_variable}, 
                                                                     "noncoeth", "age", "gender", "edu", "urban", 
                                                                     "minority", "round", "inhomelang", "country")]), ],
                              x = TRUE,
                              y = TRUE
        ) }
      
    }
    
    cv.error.1 <- rms::validate(model_lin, 
                                method = "crossvalidation", 
                                tol = 1e-14, #tol parameter for judging matrix singularity in solvet (default is 1e-12)
                                B = 10)
    cv.error.2 <- rms::validate(model_log, 
                                method = "crossvalidation", 
                                B = 10)
    
    cv.error.1 <- as.numeric(cv.error.1[3,3])
    
    if(dim(cv.error.2)[1] == 5) {
      cv.error.2 <- as.numeric(cv.error.2[4,3])
      cv.error.3 <- NA
    } else {
      cv.error.2 <- as.numeric(cv.error.2[10,3])
    }
    
    winner <- ifelse(cv.error.1 > cv.error.2, 1.0, ifelse(cv.error.1 < cv.error.2, 2.0, NA))
    
    cv.error <- data.frame("name" = outcome_variable,"age_split" = age_variable, "linear" = cv.error.1, "log/ordered" = cv.error.2, "winner" = winner)
    
    return(cv.error)
  }) %>%
  # Create variable labels column
  mutate(label = plyr::mapvalues(name, 
                                 paste0(variable_labels$var),
                                 as.character(variable_labels$label))) %>%
  select(-name)
  

cv_lin_log_final <- cv_lin_log_3_4 %>%
  mutate(term =
           case_when(grepl("age_40", age_split) ~
                       "Age 40 split",
                     grepl("age_35", age_split) ~
                       "Age 35 split",
                     grepl("age_30", age_split) ~
                       "Age 30 split",
                     grepl("age_10", age_split) ~
                       "Age 10 split"
                       )) %>%
  mutate(winner = as.character(winner)) %>%
  mutate(larger =
           case_when(grepl("2", winner) ~
                       "Logistic/ordered",
                     grepl("1", winner) ~
                       "Linear"
           )) %>%
  select(-age_split, -winner)

View(cv_lin_log_final)

write.csv(cv_lin_log_final, "tables/cross_validation/cross_validation_results.csv", row.names=FALSE, quote=FALSE) 
