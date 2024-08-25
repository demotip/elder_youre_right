# MAKING FULL REGRESSION TABLES FOR BOTH LINEAR AND LOGISTIC/ORDERED REGRESSIONS

source("scripts/descriptive_statistics.R")
source("scripts/variable_labels.R")

library(groundhog)

reg_tables <- c("glue", "lfe", "tidyverse", "lmtest", "sandwich", "modelsummary", "janitor", "kableExtra")
groundhog.library(reg_tables, "2021-11-01")

country_levels <- levels(as.factor(afpr$country))
country_levels_formatted <- paste0("country",country_levels)

# The glance function is a way to customize the output of model.summary function later

glance_custom.felm <- function(x, ...) {
  if (names(x[["fe"]][1]) == "country") {
    out <- data.frame("Country and survey round fixed effects" = "Yes")
  } else {
    out <- data.frame("Adida et al. (2016) fixed effects" = "Yes")
  }
  
  return(out)
}

### Defining model names, map of variable names ----

cm <- c("coarsened_age_30older_int" = "30 split, interviewer older",
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

s <- c("30_split", "35_split", "40_split")
splits <- rep(s, each = 8)

names_stat <- rep(stat_outcomes, 3)
names_stat_split <- paste(names_stat, splits, sep = "-")

names_pol <- rep(pol_outcomes, 3)
splits_pro <- rep(s, each = 9)
names_pol_split <- paste(names_pol, splits_pro, sep = "-")

names_pro <- rep(pro_outcomes, 3)
splits_pro <- rep(s, each = 4)
names_pro_split <- paste(names_pro, splits_pro, sep = "-")

names_eth <- rep(eth_outcomes, 3)
splits_eth <- rep(s, each = 7)
names_eth_split <- paste(names_eth, splits_eth, sep = "-")

names_youth <- rep(youth_outcomes, 3)
splits_youth <- rep(s, each = 7)
names_youth_split <- paste(names_youth, splits_youth, sep = "-")

### REGRESSION TABLES FOR ADIDA FIXED EFFECTS ----
# Contrasts.R forms a data frame of estimates as the output of the regressions
# To make a full regression table we need these same regressions in a list output
# It's then necessary to split the regression lists by group due to memory usage restrictions

# First, defining the combinations of outcome var and age split, 
# then grouping the combinations by outcome group

form_base_adida  <- paste0("{outcome_variable} ~ {age_variable} + ",
                     "noncoeth +", 
                     "age + gender + edu + ",
                     "urban + minority +",
                     "round + inhomelang | region + tribe + enumeth")

outcome_age_combos_grouped <- outcome_age_combos %>%
  mutate(label = plyr::mapvalues(outcome, 
                                 paste0(variable_labels$var),
                                 as.character(variable_labels$label))) %>%
  # Make sure factor levels are correct for order of variables in plots
  mutate(label = factor(label, levels = variable_labels$label)) %>%
  # Create variable grouping column (i.e. stat_outcomes, pol_outcomes, etc.)
  mutate(group = plyr::mapvalues(outcome, 
                                 paste0(variable_labels$var),
                                 variable_labels$group)) %>% #need the group specification here for the next function
  filter(age_variable!="coarsened_age_10") #removing 10-year age splits from the data

outcome_age_combos_mauritius <-
  expand.grid(outcome = youth_outcomes,
              age_variable = c("coarsened_age_30",
                               "coarsened_age_35",
                               "coarsened_age_40"),
              stringsAsFactors = FALSE)

# Defining the model function for youth outcomes and everybody else

model_function <- function(data_frame) { pmap(data_frame, function(outcome, age_variable, round) {
    
    if(round == "7") include_round <- 7 else include_round <- 3:4
    
    outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
      
    contrasts_matrix_list <- list(x = contrasts_matrix)
    names(contrasts_matrix_list) <- age_variable
    model <- felm(as.formula(glue(form_base_adida)), data = afpr[afpr$round %in% include_round, ], 
                    contrasts = contrasts_matrix_list)
    
    return(model)
}) } 

model_function_mauritius <- function(data_frame) { pmap(data_frame, function(outcome, age_variable) {
    
    outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
      
    contrasts_matrix_list <- list(x = contrasts_matrix)
    names(contrasts_matrix_list) <- age_variable
    
    if(outcome_variable == "z_youth_needs") {
      
      if(age_variable == "coarsened_age_10") {
        # Note: no contrasts for 10-year age difference model
        # because we only have one baseline of interest: same age.
        model <- felm(as.formula(glue(form_base_adida)), data = afpr[afpr$round == 7, ])
        
      } else {
        
        model <- felm(as.formula(glue(form_base_adida)), data = afpr[afpr$round == 7, ],
                    contrasts = contrasts_matrix_list) } 
      
    } else {  if(age_variable == "coarsened_age_10") {
      
        model <- felm(as.formula(glue(form_base_adida)), data = afpr[afpr$round == 7 & afpr$country %in% "Mauritius", ]) 
      
      } else {
      
        model <- felm(as.formula(glue(form_base_adida)), data = afpr[afpr$round == 7 & afpr$country %in% "Mauritius", ],
                    contrasts = contrasts_matrix_list
                  )} 
    
    }
    
    return(model)
  }) }

# Running the regression output functions for Adida fe ----

names_all <- list("stat_outcomes" = list("stat_outcomes", names_stat_split),
                  "pol_outcomes" = list("pol_outcomes", names_pol_split),
                  "pro_outcomes" = list("pro_outcomes", names_pro_split),
                  "eth_outcomes" = list("eth_outcomes", names_eth_split))

regression_output <- lapply(names_all, function(x){outcome_age_combos_grouped %>%
                                          filter(group == x[1]) %>%
                                          dplyr::select(-group, -label) %>%
                                          model_function() %>%
                                          "names<-"(x) %>%
                            saveRDS(., file = glue("data_clean/regression_output_",as.character(x[1]))) })

regression_output$youth_outcomes <- outcome_age_combos_mauritius %>%
  # dplyr::select(-group, -label) %>%
  model_function_mauritius() %>%
  "names<-"(names_youth_split) %>%
  saveRDS(., file = "data_clean/regression_output_youth_outcomes")

### Plotting adida fe ----
### First, reading in the RDS files (can skip this part)

regression_output_adidafe <- list()

regression_output_adidafe$stat <- readRDS(file = "data_clean/regression_output_stat_outcomes")
regression_output_adidafe$pol <- readRDS(file = "data_clean/regression_output_pol_outcomes")
regression_output_adidafe$pro <- readRDS(file = "data_clean/regression_output_pro_outcomes")
regression_output_adidafe$eth <- readRDS(file = "data_clean/regression_output_eth_outcomes")
regression_output_adidafe$youth <- readRDS(file = "data_clean/regression_output_youth_outcomes")

regression_output_adidafe$stat <- regression_output_adidafe$stat[order(names(regression_output_adidafe$stat))]
regression_output_adidafe$pol <- regression_output_adidafe$pol[order(names(regression_output_adidafe$pol))]
regression_output_adidafe$pro <- regression_output_adidafe$pro[order(names(regression_output_adidafe$pro))]
regression_output_adidafe$eth <- regression_output_adidafe$eth[order(names(regression_output_adidafe$eth))]
regression_output_adidafe$youth <- regression_output_adidafe$youth[order(names(regression_output_adidafe$youth))]


modelsummary_function_adida <- function(data_frame) {
  modelsummary(data_frame, coef_omit = "enumeth|region|tribe|round",
               coef_map = cm,
               estimate = c("{estimate} ({std.error}){stars}"),
               fmt = 2,
               statistic = NULL,
               output = "latex",  #undocumented but need to specify here to output as latex
               caption = paste0(""),
               booktabs = TRUE,
               linesep = "", 
               escape = TRUE, 
               notes = list("Regression models for the estimates that include fixed effects present in Adida et al. (2016). Models all use robust standard errors. P-values: *** p<0.001, ** p<0.01, * p<0.05"),
               output = paste0("tables/reg_table_",data_frame,".docx")
                ) 
}

# For display - splitting the tables so they have a maximum of 15 outcome vars each

regression_output_adidafe$stat_1 <- regression_output_adidafe$stat[1:15]
regression_output_adidafe$stat_2 <- regression_output_adidafe$stat[16:24]
regression_output_adidafe$pol_1 <- regression_output_adidafe$pol[1:15]
regression_output_adidafe$pol_2 <- regression_output_adidafe$pol[16:27]
regression_output_adidafe$eth_1 <- regression_output_adidafe$eth[1:15]
regression_output_adidafe$eth_2 <- regression_output_adidafe$eth[16:21]
regression_output_adidafe$youth_1 <- regression_output_adidafe$youth[1:15]
regression_output_adidafe$youth_2 <- regression_output_adidafe$youth[16:21]

# ERROR MESSAGE:
# Error in `[.data.frame`(est, , unique(c("part", names(est)))) : 
#   undefined columns selected
# Not sure why this is popping up right now but this error is produced
# both by the single table functions and by the lapply() functionalization
# I suspect this is some kind of bug?

table_test <- modelsummary(regression_output_adidafe$stat_1, coef_omit = "enumeth|region|tribe|round",
             coef_map = cm,
             estimate = c("{estimate} ({std.error}){stars}"),
             fmt = 2,
             statistic = NULL,
             # output = "latex",  #undocumented but need to specify here to output as latex
             # caption = paste0(""),
             booktabs = TRUE,
             linesep = "", 
             escape = TRUE, 
             notes = list("Regression models for the estimates that include fixed effects present in Adida et al. (2016). Models all use robust standard errors. P-values: *** p<0.001, ** p<0.01, * p<0.05"),
             # output = paste0("tables/regression_tables/reg_table_stat_adida_1.tex")
) |>
  kable_styling(
    font_size = 7,
    full_width = FALSE,
    latex_options = c("HOLD_position")
  ) 

reg_tables_adidafe <- lapply(regression_output_adidafe[6:13], function(x) {
  plot <- modelsummary(x, coef_omit = "enumeth|region|tribe|round",
               coef_map = cm,
               estimate = c("{estimate} ({std.error}){stars}"),
               fmt = 2,
               statistic = NULL,
               output = "latex",  # undocumented but need to specify here to output as latex
               # caption = paste0(""),
               booktabs = TRUE,
               linesep = "", 
               escape = TRUE, 
               notes = list("Regression models for the estimates that include fixed effects present in Adida et al. (2016). Models all use robust standard errors. P-values: *** p<0.001, ** p<0.01, * p<0.05"),
               # output = paste0("tables/regression_tables/reg_table_",names(x),"_adida_1.tex")
  ) |>
    kable_styling(
      font_size = 7,
      full_width = FALSE,
      latex_options = c("HOLD_position")
    ) 
  
  return(plot)
  
})


# Running the country fe models ----
# Same exact process here: need a list output for the regressions with country fixed effects

form_base_country  <- paste0("{outcome_variable} ~ {age_variable} + ",
                             "noncoeth +", 
                             "age + gender + edu + ",
                             "urban + minority +",
                             "round + inhomelang | country")

# Defining model functions to iterate across variable groups

model_function_countryfe <- function(data_frame) { pmap(data_frame, function(outcome, age_variable, round) {
  
  if(round == "7") include_round <- 7 else include_round <- 3:4
  
  if(round == "7") afpr <- subset(afpr, country != "Uganda") 
  
  outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
  
  contrasts_matrix_list <- list(x = contrasts_matrix)
  names(contrasts_matrix_list) <- age_variable
  
  model <- felm(as.formula(glue(form_base_country)), data = afpr[afpr$round %in% include_round, ], 
                contrasts = contrasts_matrix_list)
  
  return(model)
}) } 

model_function_mauritius_countryfe <-
  function(data_frame) { pmap(data_frame, function(outcome, age_variable) {
    
    outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
    
    contrasts_matrix_list <- list(x = contrasts_matrix)
    names(contrasts_matrix_list) <- age_variable
    
    if(outcome_variable == "z_youth_needs") {
      
      if(age_variable == "coarsened_age_10") {
        # Note: no contrasts for 10-year age difference model
        # because we only have one baseline of interest: same age.
        model <- felm(as.formula(glue(form_base_country)), data = afpr[afpr$round == 7, ])
        
      } else {
        
        model <- felm(as.formula(glue(form_base_country)), data = afpr[afpr$round == 7, ],
                      contrasts = contrasts_matrix_list) } 
      
    } else {  if(age_variable == "coarsened_age_10") {
      
      model <- felm(as.formula(glue(form_base_country)), data = afpr[afpr$round == 7 & afpr$country %in% "Mauritius", ]) 
      
    } else {
      
      model <- felm(as.formula(glue(form_base_country)), data = afpr[afpr$round == 7 & afpr$country %in% "Mauritius", ],
                    contrasts = contrasts_matrix_list
      )} 
      
    }
    
    return(model)
  }) }

regression_output_countryfe <- lapply(names_all, 
     function(x){outcome_age_combos_grouped %>%
         filter(group == x[1]) %>%
         dplyr::select(-group, -label) %>%
         model_function_countryfe() %>%
         "names<-"(x) %>%
         saveRDS(., file = glue("data_clean/regression_countryfe_",as.character(x[1]))) })

regression_output_countryfe$youth_outcomes <- outcome_age_combos_mauritius %>%
  # filter(group == "youth_outcomes") %>%
  # dplyr::select(-group, -label) %>%
  model_function_mauritius_countryfe() %>%
  "names<-"(names_youth_split) %>%
  saveRDS(., file = "data_clean/regression_countryfe_youth_outcomes")

# Plotting the country fe models ----

regression_output_countryfe <- list()

regression_output_countryfe$stat <- readRDS(file = "data_clean/regression_countryfe_stat_outcomes")
regression_output_countryfe$pol <- readRDS(file = "data_clean/regression_countryfe_pol_outcomes")
regression_output_countryfe$pro <- readRDS(file = "data_clean/regression_countryfe_pro_outcomes")
regression_output_countryfe$eth <- readRDS(file = "data_clean/regression_countryfe_eth_outcomes")
regression_output_countryfe$youth <- readRDS(file = "data_clean/regression_countryfe_youth_outcomes")

regression_output_countryfe$stat <- regression_output_countryfe$stat[order(names(regression_output_countryfe$stat))]
regression_output_countryfe$pol <- regression_output_countryfe$pol[order(names(regression_output_countryfe$pol))]
regression_output_countryfe$pro <- regression_output_countryfe$pro[order(names(regression_output_countryfe$pro))]
regression_output_countryfe$eth <- regression_output_countryfe$eth[order(names(regression_output_countryfe$eth))]
regression_output_countryfe$youth <- regression_output_countryfe$youth[order(names(regression_output_countryfe$youth))]

# Again, splitting up the regression output 

regression_output_countryfe$stat_1 <- regression_output_countryfe$stat[1:15]
regression_output_countryfe$stat_2 <- regression_output_countryfe$stat[16:24]
regression_output_countryfe$pol_1 <- regression_output_countryfe$pol[1:15]
regression_output_countryfe$pol_2 <- regression_output_countryfe$pol[16:27]
regression_output_countryfe$eth_1 <- regression_output_countryfe$eth[1:15]
regression_output_countryfe$eth_2 <- regression_output_countryfe$eth[16:21]
regression_output_countryfe$youth_1 <- regression_output_countryfe$youth[1:15]
regression_output_countryfe$youth_2 <- regression_output_countryfe$youth[16:21]

reg_tables_countryfe <- lapply(regression_output_countryfe[6:13], function(x) {
  
  plot <- modelsummary(x, coef_omit = "country|round|(Intercept)",
               coef_map = cm,
               estimate = c("{estimate} ({std.error}){stars}"),
               fmt = 2,
               statistic = NULL,
               output = "latex",
               # caption = paste0(""),
               booktabs = TRUE,
               linesep = "", 
               escape = TRUE, 
               notes = list("Regression models for the estimates that include only country and survey round fixed effects. Models all use robust standard errors. P-values: *** p<0.001, ** p<0.01, * p<0.05"),
               # output = paste0("tables/regression_tables/reg_table_",names(x),"_country.tex")
  ) |>
    
    kable_styling(
      font_size = 7,
      full_width = FALSE,
      latex_options = c("HOLD_position")
    )
  
  return(plot)
  
})

# REGRESSION TABLES FOR LOGISTIC/ORDERED LOGISTIC FIXED EFFECTS ----

form_base_factor  <- paste0("as.factor({outcome_variable}) ~ {age_variable} + ",
                            "noncoeth +",
                            "age + gender + edu + ",
                            "urban + minority +",
                            "round + inhomelang + as.factor(country)") #same as in cross_validated.R - defined again here for convenience

form_base_factor_nocount  <- paste0("as.factor({outcome_variable}) ~ {age_variable} + ",
                            "noncoeth +",
                            "age + gender + edu + ",
                            "urban + minority +",
                            "round + inhomelang")

# Defining contrasts outside of the function bc that's the only way it works with polr()
contrasts_matrix_polr <- matrix(c(
  -1, -0.5, -0.5,
  -1,  0.5, -0.5,
  1,  0.0,  0.0,
  1,  0.0,  1.0
), ncol = 3, byrow = TRUE)
colnames(contrasts_matrix_polr) <- c("not_relevant", "younger_int", "older_int")
rownames(contrasts_matrix_polr) <- c("younger_int", "older_int", "not_relevant", "younger_int")

contrasts(afpr$coarsened_age_30) <- contrasts_matrix_polr
contrasts(afpr$coarsened_age_35) <- contrasts_matrix_polr
contrasts(afpr$coarsened_age_40) <- contrasts_matrix_polr

model_function_logistic <- function(data_frame) {
  pmap(data_frame, function(outcome, age_variable, round) {

  if(round == "7") include_round <- 7 else include_round <- 3:4

  outcome_variable <- outcome # not standardizing variables

    # contrasts_matrix_list <- list(x = contrasts_matrix)
    # names(contrasts_matrix_list) <- age_variable

    if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {

      model <- glm(as.formula(glue(form_base_factor)),
                   data = afpr[afpr$round %in% include_round, ],
                   # contrasts = contrasts_matrix_list,
                   family = "binomial")

    } else {

      model <- MASS::polr(as.formula(glue(form_base_factor)),
                          method = "logistic",
                          data = afpr[afpr$round %in% include_round, ],
                          # contrasts = list("{age_variable}" = contrasts_matrix_polr),
                          Hess = T)

    }

  return(model)
  }
  ) }

model_function_mauritius_logistic <-
  function(data_frame) { pmap(data_frame, function(outcome, age_variable) {

    outcome_variable <- outcome # not standardized variables

    # contrasts_matrix_list <- list(x = contrasts_matrix)
    # names(contrasts_matrix_list) <- age_variable

    if(outcome_variable == "youth_needs") {
      
      model <- MASS::polr(as.formula(glue(form_base_factor)),
                          method = "logistic",
                          data = afpr[afpr$round == 7, ],
                          Hess = T)

    } else {
      
      if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
        
        model <- glm(as.formula(glue(form_base_factor_nocount)),
                     data = afpr[afpr$round == 7 & afpr$country %in% "Mauritius", ],
                     family = "binomial")
        
      } else {
        
        model <- MASS::polr(as.formula(glue(form_base_factor_nocount)),
                            method = "logistic",
                            data = afpr[afpr$round == 7 & afpr$country %in% "Mauritius", ],
                            Hess = T)
        
      }

    }

    return(model)
  }) }

regression_output_logistic <- lapply(names_all, 
function(x){
  
  outcome_age_combos_grouped %>%
    filter(group == x[1]) %>%
    dplyr::select(-group, -label) %>%
    model_function_logistic() %>%
    "names<-"(x) %>%
    saveRDS(., file = glue("data_clean/regression_logistic_",as.character(x[1])))
  
})

regression_output_youth_logistic <- outcome_age_combos_mauritius %>%
  # filter(group == "youth_outcomes") %>%
  # dplyr::select(-group, -label) %>%
  model_function_mauritius_logistic() %>%
  "names<-"(names_youth_split) %>%
  saveRDS(., file = "data_clean/regression_logistic_youth_outcomes")

regression_output_logistic <- list()

regression_output_logistic$stat <- readRDS(file = "data_clean/regression_logistic_stat_outcomes")
regression_output_logistic$pol <- readRDS(file = "data_clean/regression_logistic_pol_outcomes")
regression_output_logistic$pro <- readRDS(file = "data_clean/regression_logistic_pro_outcomes")
regression_output_logistic$eth <- readRDS(file = "data_clean/regression_logistic_eth_outcomes")
regression_output_logistic$youth <- readRDS(file = "data_clean/regression_logistic_youth_outcomes")

regression_output_logistic$stat <- regression_output_logistic$stat[order(names(regression_output_logistic$stat))]
regression_output_logistic$pol <- regression_output_logistic$pol[order(names(regression_output_logistic$pol))]
regression_output_logistic$pro <- regression_output_logistic$pro[order(names(regression_output_logistic$pro))]
regression_output_logistic$eth <- regression_output_logistic$eth[order(names(regression_output_logistic$eth))]
regression_output_logistic$youth <- regression_output_logistic$youth[order(names(regression_output_logistic$youth))]

# Plotting the logistic models ----

cm_log <- list("(Intercept)" = "Constant",
            "0|1" = "0|1",
            "1|2" = "1|2",
            "2|3" = "2|3",
            "3|4" = "3|4",
            "4|5" = "4|5",
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
        "inhomelang" = "In-home language")

regression_output_logistic$stat_1 <- regression_output_logistic$stat[1:15]
regression_output_logistic$stat_2 <- regression_output_logistic$stat[16:24]
regression_output_logistic$pol_1 <- regression_output_logistic$pol[1:15]
regression_output_logistic$pol_2 <- regression_output_logistic$pol[16:27]
regression_output_logistic$eth_1 <- regression_output_logistic$eth[1:15]
regression_output_logistic$eth_2 <- regression_output_logistic$eth[16:21]
regression_output_logistic$youth_1 <- regression_output_logistic$youth[1:15]
regression_output_logistic$youth_2 <- regression_output_logistic$youth[16:21]

# Saving these tables one at a time is easiest for now because the different
# specifications for custom.gof.row don't follow a particular pattern

texreg::texreg(regression_output_logistic$stat_1,
               # single.row = TRUE,
               omit.coef = "(enumeth)|(region)|(tribe)",
               caption = paste0("Logistic and ordinal regressions"),
               custom.gof.row = list("Country and round fixed effects" = rep("Yes", 15)), 
               custom.coef.map = cm_log,
               threeparttable = TRUE,
               booktabs = TRUE,
               include.loglik = FALSE,
               file = paste0("tables/regression_tables/reg_table_stat_log_1.tex")
               ) 

texreg::texreg(regression_output_logistic$stat_2,
               # single.row = TRUE,
               omit.coef = "(enumeth)|(region)|(tribe)",
               caption = paste0("Logistic and ordinal regressions"),
               custom.gof.row = list("Country and round fixed effects" = rep("Yes", 9)), 
               custom.coef.map = cm_log,
               threeparttable = TRUE,
               booktabs = TRUE,
               include.loglik = FALSE,
               file = paste0("tables/regression_tables/reg_table_stat_log_2.tex")) 

texreg::texreg(regression_output_logistic$pol_1,
               # single.row = TRUE,
               omit.coef = "(enumeth)|(region)|(tribe)",
               caption = paste0("Logistic and ordinal regressions"),
               custom.gof.row = list("Country and round fixed effects" = rep("Yes", 15)), 
               custom.coef.map = cm_log,
               threeparttable = TRUE,
               booktabs = TRUE,
               include.loglik = FALSE,
               file = paste0("tables/regression_tables/reg_table_pol_log_1.tex")) 

texreg::texreg(regression_output_logistic$pol_2,
               # single.row = TRUE,
               omit.coef = "(enumeth)|(region)|(tribe)",
               caption = paste0("Logistic and ordinal regressions"),
               custom.gof.row = list("Country and round fixed effects" = rep("Yes", 12)), 
               custom.coef.map = cm_log,
               threeparttable = TRUE,
               booktabs = TRUE,
               include.loglik = FALSE,
               file = paste0("tables/regression_tables/reg_table_pol_log_2.tex")) 

texreg::texreg(regression_output_logistic$eth_1,
               # single.row = TRUE,
               omit.coef = "(enumeth)|(region)|(tribe)",
               caption = paste0("Logistic and ordinal regressions"),
               custom.gof.row = list("Country and round fixed effects" = rep("Yes", 15)), 
               custom.coef.map = cm_log,
               threeparttable = TRUE,
               booktabs = TRUE,
               include.loglik = FALSE,
               file = paste0("tables/regression_tables/reg_table_eth_log_1.tex")) 

texreg::texreg(regression_output_logistic$eth_2,
               # single.row = TRUE,
               omit.coef = "(enumeth)|(region)|(tribe)",
               caption = paste0("Logistic and ordinal regressions"),
               custom.gof.row = list("Country and round fixed effects" = rep("Yes", 6)), 
               custom.coef.map = cm_log,
               threeparttable = TRUE,
               booktabs = TRUE,
               include.loglik = FALSE,
               file = paste0("tables/regression_tables/reg_table_eth_log_2.tex")) 

texreg::texreg(regression_output_logistic$youth_1,
               # single.row = TRUE,
               omit.coef = "(enumeth)|(region)|(tribe)",
               caption = paste0("Logistic and ordinal regressions"),
               custom.gof.row = list("Country and round fixed effects" = rep("Yes", 15)), 
               custom.coef.map = cm_log,
               threeparttable = TRUE,
               booktabs = TRUE,
               include.loglik = FALSE,
               file = paste0("tables/regression_tables/reg_table_youth_log_1.tex")) 

texreg::texreg(regression_output_logistic$youth_2,
               # single.row = TRUE,
               omit.coef = "(enumeth)|(region)|(tribe)",
               caption = paste0("Logistic and ordinal regressions"),
               custom.gof.row = list("Country and round fixed effects" = rep("Yes", 6)), 
               custom.coef.map = cm_log,
               threeparttable = TRUE,
               booktabs = TRUE,
               include.loglik = FALSE,
               file = paste0("tables/regression_tables/reg_table_youth_log_2.tex")) 

texreg::texreg(regression_output_logistic$pro,
               # single.row = TRUE,
               omit.coef = "(enumeth)|(region)|(tribe)",
               caption = paste0("Logistic and ordinal regressions"),
               custom.gof.row = list("Country and round fixed effects" = rep("Yes", 12)), 
               custom.coef.map = cm_log,
               threeparttable = TRUE,
               booktabs = TRUE,
               include.loglik = FALSE,
               file = paste0("tables/regression_tables/reg_table_pro_log.tex")) 

### Getting the coefficients from each model ----
# 
# coef_t_df <- function(model) {
#   as.data.frame(t(coef(model)))
# }
# 
# coefficients_list_stat <- lapply(regression_output_countryfe$stat, coef_t_df)
# coefficients_list_pol <- lapply(regression_output_countryfe$pol, coef_t_df)
# coefficients_list_pro <- lapply(regression_output_countryfe$pro, coef_t_df)
# coefficients_list_eth <- lapply(regression_output_countryfe$eth, coef_t_df)
# coefficients_list_youth <- lapply(regression_output_countryfe$youth, coef_t_df)
# 
# coefficients_df_stat <- bind_rows(coefficients_list_stat) %>%
#   `rownames<-`(., names(regression_output_countryfe$stat))
# coefficients_df_pol <- bind_rows(coefficients_list_pol) %>%
#   `rownames<-`(., names(regression_output_countryfe$pol))
# coefficients_df_pro <- bind_rows(coefficients_list_pro) %>%
#   `rownames<-`(., names(regression_output_countryfe$pro))
# coefficients_df_eth <-bind_rows(coefficients_list_eth) %>%
#   `rownames<-`(., names(regression_output_countryfe$eth))
# coefficients_df_youth <- bind_rows(coefficients_list_youth) %>%
#   `rownames<-`(., names(regression_output_countryfe$youth))
# 
# coefficientsdf <- rbind(coefficients_df_stat, coefficients_df_pol, coefficients_df_pro, coefficients_df_eth, coefficients_df_youth)
# 
# coefficientsdf <- coefficientsdf %>%
#   dplyr::select(coarsened_age_30younger_int, coarsened_age_35younger_int, coarsened_age_40younger_int, 
#                 coarsened_age_30older_int, coarsened_age_35older_int, coarsened_age_40older_int,
#         coarsened_age_30not_relevant, coarsened_age_35not_relevant, coarsened_age_40not_relevant, everything()) %>%
#   write.csv(file = "tables/all_country_coefficients.csv")


