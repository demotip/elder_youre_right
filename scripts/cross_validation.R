source("scripts/contrasts.R")
source("scripts/variable_labels.R")

# LOAD NECESSARY PACKAGES ----

library(groundhog)

cv <- c("glue", "lfe", "tidyverse", "lmtest", 
        "sandwich", "modelsummary", "boot", "caret", "rms", "htmlTable")

groundhog.library(cv, "2021-11-01")

# To define contrasts, our coarsened age variables need to be factors
# with pre-defined levels. 
if(!is.factor(afpr$coarsened_age_10) |
   !is.factor(afpr$coarsened_age_30) |
   !is.factor(afpr$coarsened_age_35) |
   !is.factor(afpr$coarsened_age_40) ) {
  stop("Make sure that coarsened age variables are factors.")
}

# Setting up fixed effects as factors ----

afpr$region <- as.factor(afpr$region)
afpr$tribe <- as.factor(afpr$tribe)
afpr$enumeth <- as.factor(afpr$enumeth)
afpr$country <- as.factor(afpr$country)

# All the forms ----
form_base  <- paste0("{outcome_variable} ~ {age_variable} + ",
                     "noncoeth +", 
                     "age + gender + edu + ",
                     "urban + minority +",
                     "round + inhomelang + region + tribe + enumeth")

form_base_factor  <- paste0("as.factor({outcome_variable}) ~ {age_variable} + ",
                            "noncoeth +", 
                            "age + gender + edu + ",
                            "urban + minority +",
                            "round + inhomelang + country") # removed  "+ region + tribe + enumeth": just being silly with it

form_base_country  <- paste0("{outcome_variable} ~ {age_variable} + ",
                             "noncoeth +", 
                             "age + gender + edu + ",
                             "urban + minority +",
                             "round + inhomelang + country") # removed + country

form_base_factor_7  <- paste0("as.factor({outcome_variable}) ~ {age_variable} + ",
                            "noncoeth +", 
                            "age + gender + edu + ",
                            "urban + minority +",
                            "round + inhomelang")

form_base_7  <- paste0("{outcome_variable} ~ {age_variable} + ",
                              "noncoeth +", 
                              "age + gender + edu + ",
                              "urban + minority +",
                              "round + inhomelang")

# Playing with the rms contrasts function ----

# dd <- datadist(afpr)
# options(datadist = "dd")
# print(dd$limits)
# 
# contrasts <- list(
#   "Both younger (age 30 cutoff)" = c(0, 0, 1, 0),
#   "Both older (age 30 cutoff)" = c(0, 0, 1, 0),
#   "Interviewer younger (age 30 cutoff)" = c(-1, 1, 0, 0),
#   "Interviewer older (age 30 cutoff)" = c(0, 0, -1,  1 )
#   )
# 
# contrast(model_lin, contrasts)

# Defining contrasts outside of the function bc that's the only way it works with polr()
# contrasts_matrix_rms <- matrix(c(
#   -1, -0.5, -0.5,
#   -1,  0.5, -0.5,
#   1,  0.0,  0.0,
#   1,  0.0,  1.0
# ), ncol = 3, byrow = TRUE)
# colnames(contrasts_matrix_rms) <- c("not_relevant", "younger_int", "older_int")
# rownames(contrasts_matrix_rms) <- c("younger_int", "older_int", "not_relevant", "younger_int")
# 
# contrasts(afpr$coarsened_age_30) <- contrasts_matrix_rms
# contrasts(afpr$coarsened_age_35) <- contrasts_matrix_rms
# contrasts(afpr$coarsened_age_40) <- contrasts_matrix_rms

# The most recent function with rms, split by round ----

cv_lin_log_3_4 <- 
  pmap_dfr(outcome_age_combos_3_4, function(outcome, age_variable, round) {
    
    if(round == "7") include_round <- 7 else include_round <- 3:4
    
    outcome_variable <- outcome # not standardizing variables
    
    print(outcome_variable)
    
    # Run model
    if(age_variable == "coarsened_age_10") {
      # Note: no contrasts for 10-year age difference model
      # because we only have one baseline of interest: same age.
      
      print(1)
      
      model_lin <- rms::ols(as.formula(glue(form_base_country)), data = 
                              droplevels(afpr[afpr$round %in% include_round & 
                                                complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                         "noncoeth", "age", "gender", "edu", "urban", 
                                                                         "minority", "round", "inhomelang", "country")]), ]),
                            x = TRUE,  # need x and y to be TRUE for validate.ols to work
                            y = TRUE
      )
      
      if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
        
        print(2)
        
        model_log <- rms::lrm(as.formula(glue(form_base_factor)),
                              data = droplevels(afpr[afpr$round %in% include_round & 
                                                       complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                                "noncoeth", "age", "gender", "edu", "urban", 
                                                                                "minority", "round", "inhomelang", "country")]), ]),
                              x = TRUE,
                              y = TRUE)
      } else {
        # Note: no contrasts for 10-year age difference model
        # because we only have one baseline of interest: same age.
        
        print(3)
        
        model_log <- rms::orm(as.formula(glue(form_base_factor)),
                              data = afpr[afpr$round %in% include_round & 
                                            complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                     "noncoeth", "age", "gender", "edu", "urban", 
                                                                     "minority", "round", "inhomelang", "country")]), ],
                              x = TRUE,
                              y = TRUE ) }
      
    } else {
      
      contrasts_matrix_list <- list(x = contrasts_matrix)
      names(contrasts_matrix_list) <- age_variable
      
      print(4)
      
      model_lin <- rms::ols(as.formula(glue(form_base_country)), data =
                              droplevels(afpr[afpr$round %in% include_round & 
                                                complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                         "noncoeth", "age", "gender", "edu", "urban", 
                                                                         "minority", "round", "inhomelang", "country")]), ]),
                            x = TRUE,
                            y = TRUE
                            # contrasts = contrasts_matrix_list
      ) 
      
      if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
        print(5)
        
        model_log <- rms::lrm(as.formula(glue(form_base_factor)),
                              droplevels(afpr[afpr$round %in% include_round & 
                                                complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                         "noncoeth", "age", "gender", "edu", "urban", 
                                                                         "minority", "round", "inhomelang", "country")]), ]),
                              x = TRUE,
                              y = TRUE
                              # contrasts = contrasts_matrix_list
        )
      } else {
        
        print(6)
        
        model_log <- rms::orm(as.formula(glue(form_base_factor)),
                              data = afpr[afpr$round %in% include_round & 
                                            complete.cases(afpr[ , c({outcome_variable},{age_variable}, 
                                                                     "noncoeth", "age", "gender", "edu", "urban", 
                                                                     "minority", "round", "inhomelang", "country")]), ],
                              x = TRUE,
                              y = TRUE
                              # contrasts = contrasts_matrix
        ) }
      
    }
    
    
    print(7)
    
    cv.error.1 <- rms::validate(model_lin, 
                                method = "crossvalidation", 
                                tol = 1e-14, #tol parameter for judging matrix singularity in solvet (default is 1e-12)
                                B = 10)
    cv.error.2 <- rms::validate(model_log, 
                                method = "crossvalidation", 
                                B = 10)
    
    cv.error.1 <- as.numeric(cv.error.1[3,3])
    # cv.error.1 <- exp(cv.error.1)/I(1 + exp(cv.error.1))
    
    if(dim(cv.error.2)[1] == 5) {
      cv.error.2 <- as.numeric(cv.error.2[4,3])
      cv.error.3 <- NA
    } else {
      cv.error.2 <- as.numeric(cv.error.2[10,3])
    }
    # cv.error.2.a <- exp(cv.error.2.a)/I(1 + exp(cv.error.2.a))
    
    winner <- ifelse(cv.error.1 > cv.error.2, 1.0, ifelse(cv.error.1 < cv.error.2, 2.0, NA))
    
    cv.error <- data.frame("name" = outcome_variable,"age_split" = age_variable, "linear" = cv.error.1, "log/ordered" = cv.error.2, "winner" = winner)
    
    return(cv.error)
  }) %>%
  # Create variable labels column
  mutate(label = plyr::mapvalues(name, 
                                 paste0(variable_labels$var),
                                 as.character(variable_labels$label))) %>%
  select(-name, -winner)
  

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

cv_lin_log_final

write.csv(cv_lin_log_final, "tables/cross_validation/cross_validation_results.csv", row.names=FALSE, quote=FALSE) 

# cv_lin_log_7 <- 
#   pmap_dfr(outcome_age_combos_7, function(outcome, age_variable, round) {
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
#       print(1)
#       
#       model_lin <- rms::ols(as.formula(glue(form_base_7)), data = 
#                               droplevels(afpr[afpr$round %in% include_round & 
#                                                 complete.cases(afpr[ , c({outcome_variable},{age_variable},
#                                                                          "noncoeth", "age", "gender", "edu", "urban", 
#                                                                          "minority", "round", "inhomelang", "country")]), ]),
#                             x = TRUE,  # need x and y to be TRUE for validate.ols to work
#                             y = TRUE)
#       
#       if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
#         
#         print(2)
#         
#         model_log <- rms::lrm(as.formula(glue(form_base_factor_7)),
#                               data = droplevels(afpr[afpr$round %in% include_round & 
#                                                        complete.cases(afpr[ , c({outcome_variable},{age_variable},
#                                                                                 "noncoeth", "age", "gender", "edu", "urban", 
#                                                                                 "minority", "round", "inhomelang", "country")]), ]),
#                               x = TRUE,
#                               y = TRUE)
#       } else {
#         # Note: no contrasts for 10-year age difference model
#         # because we only have one baseline of interest: same age.
#         
#         print(3)
#         
#         model_log <- rms::orm(as.formula(glue(form_base_factor_7)),
#                               data = afpr[afpr$round %in% include_round & 
#                                             complete.cases(afpr[ , c({outcome_variable},{age_variable},
#                                                                      "noncoeth", "age", "gender", "edu", "urban", 
#                                                                      "minority", "round", "inhomelang", "country")]), ],
#                               x = TRUE,
#                               y = TRUE ) }
#       
#     } else {
#       
#       contrasts_matrix_list <- list(x = contrasts_matrix)
#       names(contrasts_matrix_list) <- age_variable
#       
#       print(4)
#       
#       model_lin <- rms::ols(as.formula(glue(form_base_7)), data =
#                               droplevels(afpr[afpr$round %in% include_round & 
#                                                 complete.cases(afpr[ , c({outcome_variable},{age_variable},
#                                                                          "noncoeth", "age", "gender", "edu", "urban", 
#                                                                          "minority", "round", "inhomelang", "country")]), ]),
#                             x = TRUE,
#                             y = TRUE) 
#       
#       if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
#         print(5)
#         
#         model_log <- rms::lrm(as.formula(glue(form_base_factor_7)),
#                               droplevels(afpr[afpr$round %in% include_round & 
#                                                 complete.cases(afpr[ , c({outcome_variable},{age_variable},
#                                                                          "noncoeth", "age", "gender", "edu", "urban", 
#                                                                          "minority", "round", "inhomelang", "country")]), ]),
#                               x = TRUE,
#                               y = TRUE
#                               # contrasts = contrasts_matrix_list
#         )
#       } else {
#         
#         print(6)
#         
#         model_log <- rms::orm(as.formula(glue(form_base_factor_7)),
#                               data = afpr[afpr$round %in% include_round & 
#                                             complete.cases(afpr[ , c({outcome_variable},{age_variable}, 
#                                                                      "noncoeth", "age", "gender", "edu", "urban", 
#                                                                      "minority", "round", "inhomelang", "country")]), ],
#                               x = TRUE,
#                               y = TRUE
#                               # contrasts = contrasts_matrix
#         ) }
#       
#     }
#     
#     print(7)
#     
#     cv.error.1 <- rms::validate(model_lin, method = "crossvalidation", B = 10)
#     cv.error.2 <- rms::validate(model_log, method = "crossvalidation", B = 10)
#     
#     cv.error.1 <- as.numeric(cv.error.1[3,3])
#     # cv.error.1 <- exp(cv.error.1)/I(1 + exp(cv.error.1)) # Converting log-odds to probabilities: e^(g)/(1 + e^(g)) 
#     
#     # no need for an ifelse statement here bc there's no binary outcomes
#     
#     cv.error.2 <- as.numeric(cv.error.2[4,3])
#     # cv.error.2 <- exp(cv.error.2)/I(1 + exp(cv.error.2))
#     
#     # cv.error <- c(cv.error.1, cv.error.2)
#     
#     winner <- ifelse(cv.error.1 > cv.error.2, 1.0, ifelse(cv.error.1 < cv.error.2, 2.0, NA))
#     
#     cv.error <- data.frame("linear" = cv.error.1, "log.ordered" = cv.error.2, "winner" = winner)
#     
#     return(cv.error)
#   })

# cv_lin_log_x <- rbind(cv_lin_log_3_4, cv_lin_log_7)
# cv_lin_log_x <- cv_lin_log_x %>%
#   na_if(., 'NaN')

# x <- colMeans(cv_lin_log_x, na.rm = TRUE) 
# 
# cv_lin_log <- rbind(cv_lin_log_x, x) #appending it to the end of the df seems like a bad idea

cv_lin_log <- cv_lin_log_3_4

saveRDS(cv_lin_log_3_4, "tables/cross_validation_group_3_4")

# the variables that don't work for linear: bribe1, etrust, netrust

unique(afpr$bribe1)
unique(afpr$etrust)
unique(afpr$netrust)

form_base_country

model_lin_netrust <- rms::ols(netrust ~ coarsened_age_35 + noncoeth + age + 
                               gender + edu + urban + minority + round + 
                               inhomelang + country, 
                             data = droplevels(afpr[afpr$round %in% 3:4 & 
                                    complete.cases(afpr[ , c("netrust","coarsened_age_35",
                                                             "noncoeth", "age", "gender", "edu", "urban", 
                                                             "minority", "round", "inhomelang", "country")]), ]),
                      x = TRUE,
                      y = TRUE) 

cv.error.1.netrust <- rms::validate(model_lin_netrust, method = "crossvalidation", B = 10,
                                   # maxiter = 10,
                                   tol = 1e-14)

cv.error.1.netrust

# ----

cv_lin_log_1e_12 <- 
  pmap_dfr(outcome_age_combos_3_4, function(outcome, age_variable, round) {
    
    if(round == "7") include_round <- 7 else include_round <- 3:4
    
    outcome_variable <- outcome # not standardizing variables
    
    print(outcome_variable)
    
    # Run model
    if(age_variable == "coarsened_age_10") {
      # Note: no contrasts for 10-year age difference model
      # because we only have one baseline of interest: same age.
      
      print(1)
      
      model_lin <- rms::ols(as.formula(glue(form_base_country)), data = 
                              droplevels(afpr[afpr$round %in% include_round & 
                                                complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                         "noncoeth", "age", "gender", "edu", "urban", 
                                                                         "minority", "round", "inhomelang", "country")]), ]),
                            x = TRUE,  # need x and y to be TRUE for validate.ols to work
                            y = TRUE
      )
      
      if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
        
        print(2)
        
        model_log <- rms::lrm(as.formula(glue(form_base_factor)),
                              data = droplevels(afpr[afpr$round %in% include_round & 
                                                       complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                                "noncoeth", "age", "gender", "edu", "urban", 
                                                                                "minority", "round", "inhomelang", "country")]), ]),
                              x = TRUE,
                              y = TRUE)
      } else {
        # Note: no contrasts for 10-year age difference model
        # because we only have one baseline of interest: same age.
        
        print(3)
        
        model_log <- rms::orm(as.formula(glue(form_base_factor)),
                              data = afpr[afpr$round %in% include_round & 
                                            complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                     "noncoeth", "age", "gender", "edu", "urban", 
                                                                     "minority", "round", "inhomelang", "country")]), ],
                              x = TRUE,
                              y = TRUE ) }
      
    } else {
      
      contrasts_matrix_list <- list(x = contrasts_matrix)
      names(contrasts_matrix_list) <- age_variable
      
      print(4)
      
      model_lin <- rms::ols(as.formula(glue(form_base_country)), data =
                              droplevels(afpr[afpr$round %in% include_round & 
                                                complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                         "noncoeth", "age", "gender", "edu", "urban", 
                                                                         "minority", "round", "inhomelang", "country")]), ]),
                            x = TRUE,
                            y = TRUE
                            # contrasts = contrasts_matrix_list
      ) 
      
      if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
        print(5)
        
        model_log <- rms::lrm(as.formula(glue(form_base_factor)),
                              droplevels(afpr[afpr$round %in% include_round & 
                                                complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                         "noncoeth", "age", "gender", "edu", "urban", 
                                                                         "minority", "round", "inhomelang", "country")]), ]),
                              x = TRUE,
                              y = TRUE
                              # contrasts = contrasts_matrix_list
        )
      } else {
        
        print(6)
        
        model_log <- rms::orm(as.formula(glue(form_base_factor)),
                              data = afpr[afpr$round %in% include_round & 
                                            complete.cases(afpr[ , c({outcome_variable},{age_variable}, 
                                                                     "noncoeth", "age", "gender", "edu", "urban", 
                                                                     "minority", "round", "inhomelang", "country")]), ],
                              x = TRUE,
                              y = TRUE
                              # contrasts = contrasts_matrix
        ) }
      
    }
    
    
    print(7)
    
    cv.error.1 <- rms::validate(model_lin, 
                                method = "crossvalidation", 
                                tol = 1e-12, #tol parameter for judging matrix singularity in solvet (default is 1e-12)
                                B = 10)
    cv.error.2 <- rms::validate(model_log, 
                                method = "crossvalidation", 
                                B = 10)
    
    cv.error.1 <- as.numeric(cv.error.1[3,3])
    # cv.error.1 <- exp(cv.error.1)/I(1 + exp(cv.error.1))
    
    if(dim(cv.error.2)[1] == 5) {
      cv.error.2 <- as.numeric(cv.error.2[4,3])
      cv.error.3 <- NA
    } else {
      cv.error.2 <- as.numeric(cv.error.2[10,3])
    }
    # cv.error.2.a <- exp(cv.error.2.a)/I(1 + exp(cv.error.2.a))
    
    winner <- ifelse(cv.error.1 > cv.error.2, 1.0, ifelse(cv.error.1 < cv.error.2, 2.0, NA))
    
    cv.error <- data.frame("name" = outcome_variable,"age_split" = age_variable, "linear" = cv.error.1, "log/ordered" = cv.error.2)
    
    return(cv.error)
  })
