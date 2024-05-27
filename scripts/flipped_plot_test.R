library(groundhog)

plot_30_yr <- c("cowplot", "tidyverse", "showtext", "sysfonts",
                "glue", "lfe", "tidyverse", "lmtest", "sandwich", "modelsummary")
groundhog.library(plot_30_yr, "2021-11-01")

font_add_google("Roboto", "Roboto")
showtext_auto()

afpr <- readRDS("./data_clean/afpr_ages.rds")

source("scripts/variable_labels_flipped.R")

# Convenience function, opposite of %in%
'%!in%' <- function(x,y)!('%in%'(x,y))

afpr_stat <- afpr %>%
  select(ec_conditions_self, notenoughfood, noincome, 
         nocleanwater, crime, aids,
         noncoeth, coarsened_age_10, coarsened_age_30,
         coarsened_age_35, coarsened_age_40, age, 
         gender, edu, urban, minority, round, inhomelang, 
         region, tribe, enumeth) 

afpr_stat$z_ec_conditions_self <- as.vector(afpr_stat$ec_conditions_self)
afpr_stat$z_notenoughfood <- as.vector(afpr_stat$notenoughfood)
afpr_stat$z_noincome <- as.vector(afpr_stat$noincome)
afpr_stat$z_nocleanwater <- as.vector(afpr_stat$nocleanwater)
afpr_stat$z_crime <- as.vector(afpr_stat$crime)
afpr_stat$z_aids <- as.vector(afpr_stat$aids)

# DEFINE MODEL FORMULA ----
# Outcomes and coarsened age variables will be glued in in the pmap_dfr() function below
form_base  <- paste0("{outcome_variable} ~ {age_variable} + ",
                     "noncoeth +", 
                     "age + gender + edu + ",
                     "urban + minority +",
                     "round + inhomelang | region + tribe + enumeth")

# Generate all outcome-coarsened age combinations
outcome_age_combos_stat <- 
  expand.grid(outcome = c(stat_outcomes), 
              age_variable = c("coarsened_age_10", 
                               "coarsened_age_30", 
                               "coarsened_age_35", 
                               "coarsened_age_40"),
              stringsAsFactors = FALSE) %>%
  mutate(round = "3 and 4")

age_diff_models_stat <- 
  pmap_dfr(outcome_age_combos_stat, function(outcome, age_variable, round) {
    
    include_round <- 3:4
    
    outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
    
    # Run model
    if(age_variable == "coarsened_age_10") {
      # Note: no contrasts for 10-year age difference model
      # because we only have one baseline of interest: same age.
      model <- felm(as.formula(glue(form_base)), data = afpr_stat[afpr_stat$round %in% include_round, ])
    } else {
      
      contrasts_matrix_list <- list(x = contrasts_matrix)
      names(contrasts_matrix_list) <- age_variable
      
      model <- felm(as.formula(glue(form_base)), data = afpr_stat[afpr_stat$round %in% include_round, ], 
                    contrasts = contrasts_matrix_list)
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
                                 paste0("z_", variable_labels_flipped$var),
                                 as.character(variable_labels_flipped$label))) %>%
  # Make sure factor levels are correct for order of variables in plots
  mutate(label = factor(label, levels = variable_labels_flipped$label)) %>%
  # Create variable grouping column (i.e. stat_outcomes, pol_outcomes, etc.)
  mutate(group = plyr::mapvalues(outcome_variable, 
                                 paste0("z_", variable_labels_flipped$var),
                                 variable_labels_flipped$group))

# PLOT FUNCTION ----
# This function plots coefficients from models
# created in contrasts.Rmd along with error bars
plotfun <- function(data) {
  data %>%
    ggplot(aes(label,
               estimate,
               colour = term,
               linetype = term,
               shape = term)) +
    theme_linedraw() +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = lower, ymax = upper),
                  position = position_dodge(width = 0.5),
                  width = 0.3) +
    scale_colour_manual(values = c("black", "gray50", "gray80")) +
    scale_shape_manual(values = c(17,15,19)) +
    scale_linetype_manual(values = c(rep("solid", 4))) +
    coord_flip() +
    scale_y_continuous(breaks = seq(-0.2, 0.2, 0.05),
                       limits = c(-0.2, 0.2)) +
    scale_y_reverse() +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 7),
          legend.key.width=unit(3,"line"),
          strip.background = element_blank(),
          strip.text = element_blank(),
          text = element_text(family = "Roboto")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    guides(colour = guide_legend(reverse = TRUE,
                                 ncol = 1),
           linetype = guide_legend(reverse = TRUE,
                                   ncol = 1),
           shape = guide_legend(reverse = TRUE,
                                ncol = 1)) +
    ylab("\nEstimated effect (in SDs) of non-coethnic interviewer \nand age difference, with 95% confidence intervals")
}

figure_2 <- stat_age_diff_models %>%
  filter(country == "All") %>%
  filter(age_variable == "coarsened_age_35") %>%
  plotfun()

figure_2 <- figure_2 +
  geom_vline(xintercept = 1.5, size = 0.3)

figure_2
