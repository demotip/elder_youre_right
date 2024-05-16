# the first rule of the marginal effects zoo package is to have fun and be yourself :)
library(groundhog)
library(marginaleffects)
marginal_effects <- c("tidyverse", "modelsummary", "rms", "glue")
groundhog.library(marginal_effects, "2021-11-01")

# Get list of outcomes and dataframe with outcome descriptions
source("scripts/variable_labels.R")

afpr <- readRDS("./data_clean/afpr_ages.rds")

age_diff_models <- readRDS("data_clean/age_diff_models.rds")

# first just make a list of all the models if that's not prohibitively large
# then run comparisons() on the list or feed the list to comparisons with a function

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

form_base_factor  <- paste0("as.factor({outcome_variable})",
                            " ~ {age_variable} + ",
                            "noncoeth +", 
                            "age + gender + edu + ",
                            "urban + minority +",
                            "round + inhomelang")

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

# Combine for full set of outcome-coarsened_age-round combinations
outcome_age_combos <- bind_rows(outcome_age_combos_3_4, outcome_age_combos_7)

age_diff_models_list <- 
  pmap_dfr(outcome_age_combos, function(outcome, age_variable, round) {
    
    if(round == 7) {
      include_round <- 7} else {
        include_round <- 3:4}
    
    outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
    
    # Run model
    if(age_variable == "coarsened_age_10") {
      
      if(length(unique(afpr[{outcome_variable},])==2)) {
        
        model <- glm(as.formula(glue(form_base_factor)),
                     data = afpr[afpr$round %in% include_round, ],
                     family = "binomial")
      } else {
        # Note: no contrasts for 10-year age difference model
        # because we only have one baseline of interest: same age.
        
        model <- MASS::polr(as.formula(glue(form_base_factor)),
                            method = "logistic",
                            data = afpr[afpr$round %in% include_round, ],
                            Hess = T,
                            contrasts = contrasts_matrix_list
        ) }
    } else {
      
      contrasts_matrix_list <- list(x = contrasts_matrix)
      names(contrasts_matrix_list) <- age_variable
      
      if(length(unique(afpr[{outcome_variable},])==2)) {
        
        model <- glm(as.formula(glue(form_base_factor)),
                     data = afpr[afpr$round %in% include_round, ],
                     family = "binomial",
                     contrasts = contrasts_matrix_list
        )
      } else {
        
        model <- MASS::polr(as.formula(glue(form_base_factor)),
                            method = "logistic",
                            data = afpr[afpr$round %in% include_round,],
                            Hess = T,
                            contrasts = contrasts_matrix_list
        ) }
    }
    
    model <- summary(model, robust = TRUE) 
    
    return(model)
  }) 

# binary outcomes ----

# marginaleffects::predictions(age_diff_models)

