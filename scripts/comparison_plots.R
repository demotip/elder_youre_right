source("scripts/contrasts.R") # THIS CODE REQUIRES THE SAME SET-UP FOR CONTRASTS.R: THE CONTRASTS MATRIX, OUTCOME_VARIABLES, ETC
library(groundhog)

marg_effects <- c("cowplot", "tidyverse", "marginaleffects")

groundhog.library(marg_effects, "2021-11-01")

pacman::p_load(tidyverse, marginaleffects, ggplot2, magrittr, plyr, glue) 

# COMPARISON PLOTS

is_binary <- function(x) {
  length(unique(x))==3 #length == 3 because NA is counted in length
}

# Apply the function to each column of the dataset
afpr_binary <- sapply(afpr, is_binary)

# Get the names of binary variables
binary_var_names <- names(afpr_binary[afpr_binary])

form_base  <- paste0("{outcome_variable} ~ {age_variable} + ",
                     "noncoeth +", 
                     "age + gender + edu + ",
                     "urban + minority +",
                     "round + inhomelang + as.factor(country)")

form_base_factor  <- paste0("as.factor({outcome_variable}) ~ {age_variable} + ",
                            "noncoeth +", 
                            "age + gender + edu + ",
                            "urban + minority +",
                            "round + inhomelang + as.factor(country)")

# So, filter age_diff_combos by this list and then alter the pmap function to do both logistic and linear

outcome_age_combos_binary <- outcome_age_combos %>%
  filter(outcome %in% binary_var_names) %>%
  dplyr::select(!starts_with("z_")) 

comparisons_binary <- 
  pmap_dfr(outcome_age_combos_binary, function(outcome, age_variable, round) {
    
    if(round == "7") include_round <- 7 else include_round <- 3:4
    
    outcome_variable <- outcome
    
    print(outcome_variable)
    
    # Run model
    if(age_variable == "coarsened_age_10") {
      
      # Note: no contrasts for 10-year age difference model
      # because we only have one baseline of interest: same age.
      model <- lm(as.formula(glue(form_base)), data = afpr[afpr$round %in% include_round, ])
      
      model_logistic <- glm(as.formula(glue(form_base_factor)),
                                     data = afpr[afpr$round %in% include_round, ],
                                     family = "binomial")
        
    } else {
      
      # contrasts_matrix_list <- list(x = contrasts_matrix)
      # names(contrasts_matrix_list) <- age_variable
      
      model <- lm(as.formula(glue(form_base)), data = afpr[afpr$round %in% include_round, ]
                    # contrasts = contrasts_matrix_list
                  )
      
      model_logistic <- glm(as.formula(glue(form_base_factor)),
                            data = afpr[afpr$round %in% include_round, ],
                            family = "binomial"
                            # contrasts = contrasts_matrix_list
                            )
    }
    
    list_1 <- list("pairwise")
    names(list_1) = {age_variable}
    
    comparisons <- avg_comparisons(
      model,
      variables = list_1 #extract the name of the variable from the model object
      ) %>%
      mutate(outcome = {outcome_variable},
             logistic = "Linear-based estimate")
    
    comparisons_2 <- avg_comparisons(
      model_logistic,
      variables = list_1) %>%
      mutate(outcome = {outcome_variable}, 
             logistic = "Logistic-based estimate")  
    
    return(rbind(comparisons, comparisons_2))
  }) %>%
  mutate(label = plyr::mapvalues(outcome, 
                                 paste0(variable_labels$var),
                                 as.character(variable_labels$label))) 

contrasts_comparisons <- c("mean(Interviewer younger (age 35 cutoff)) - mean(Both older (age 35 cutoff))", 
  "mean(Both younger (age 35 cutoff)) - mean(Interviewer younger (age 35 cutoff))",
  "mean(Interviewer older (age 35 cutoff)) - mean(Both older (age 35 cutoff))")

# Saving this data frame as an RDS object

saveRDS(comparisons_binary, "data_clean/comparisons_binary")
comparisons_binary <- readRDS("data_clean/comparisons_binary")

# model_names <- c("Linear", "Logistic")
# names(model_names) <- c("Yes", "No")

comparison_plot <- comparisons_binary %>%
  filter(term == "coarsened_age_35") %>%
  filter(contrast %in% contrasts_comparisons) %>%
  ggplot(aes(label,
             estimate,
             colour = contrast,
             linetype = factor(logistic),
             shape = contrast)) +
  theme_linedraw() +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                position = position_dodge(width = 0.5),
                width = 0.3) +
  scale_colour_manual(values = c("black", "gray50", "gray80")) +
  scale_shape_manual(values = c(17,15,19, 21, 23, 25)) +
  scale_linetype_manual(values = c("solid", "dashed")) +
  # facet_wrap(~logistic, labeller = labeller(logistic = model_names)) +
  coord_flip() +
  scale_y_continuous(breaks = seq(-0.2, 0.2, 0.05),
                     limits = c(-0.2, 0.2)) +
  theme(legend.title = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(), # ADDED THIS LINE
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.key.width=unit(3,"line"),
        # strip.background = element_blank(),
        # strip.text = element_blank(),
        text = element_text(family = "Roboto"),
        panel.grid.major = element_line(color = 'gray80'),
        panel.grid.minor = element_line(color = 'gray80')) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  guides(colour = guide_legend(reverse = TRUE,
                               ncol = 1),
         linetype = guide_legend(reverse = TRUE,
                                 ncol = 1),
         shape = guide_legend(reverse = TRUE,
                              ncol = 1))

ggsave(filename = "figs/comparison_plot.pdf", plot = comparison_plot,
       units = "cm", width = 24, height = 24)