# COMPARISON PLOTS

# How to filter for all binary outcomes? 

is_binary <- function(x) {
  length(unique(x))==3 #length == 3 because NA is counted in length
}

# Apply the function to each column of the dataset
afpr_binary <- sapply(afpr, is_binary)

# Get the names of binary variables
binary_var_names <- names(afpr_binary[afpr_binary])

print(binary_var_names)

binary_var_names_plus <- c("coarsened_age_10", 
                           "coarsened_age_30", 
                           "coarsened_age_35", 
                           "coarsened_age_40",
                           "noncoeth",
                           "age", "edu", 
                           "region", "tribe", 
                           "enumeth", "country", binary_var_names)

# CHANGE THESE LATER - CONSISTENT NAMING SCHEMES FOR FORM_BASE PLEASE

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
# There's like 14 outcome vars so it shouldn't take long

afpr_filtered <- afpr %>%
  select(all_of(binary_var_names_plus)) %>%
  select(!starts_with("z_")) 

outcome_age_combos_binary <- outcome_age_combos %>%
  filter(outcome %in% binary_var_names)

age_diff_models_binary <- 
  pmap(outcome_age_combos_binary, function(outcome, age_variable, round) {
    
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
      
      contrasts_matrix_list <- list(x = contrasts_matrix)
      names(contrasts_matrix_list) <- age_variable
      
      model <- lm(as.formula(glue(form_base)), data = afpr[afpr$round %in% include_round, ], 
                    contrasts = contrasts_matrix_list)
      
      model_logistic <- glm(as.formula(glue(form_base_factor)),
                            data = afpr[afpr$round %in% include_round, ],
                            family = "binomial",
                            contrasts = contrasts_matrix_list)
    }
    
    model <- list(model, model_logistic)
    
    return(model)
  })

# Saving this dataframe as an RDS object

saveRDS(age_diff_models_binary, "data_clean/age_diff_models_binary")

# Next, plotted predictions

# Creating a new data frame for plotting predictions

# Need to make a new dataframe on which to conduct predictions - look back at 618 hw????

# 35 age split ----

coarsened_age_35 <- levels(afpr$coarsened_age_35)
allcombos <- expand.grid(coarsened_age_35 = coarsened_age_35)
allcombos <- allcombos %>%
  mutate(noncoeth = 0, #valid to set this to 0? Ask AE?
         age = mean(afpr$age, na.rm = TRUE), #what to do with age here? Is this fine?
         edu = mean(afpr$edu, na.rm = TRUE),
         country = "Kenya", #I don't fucking know
         gender = 0, #hmmm le deuxième sexe
         urban = mean(afpr$urban, na.rm = TRUE),
         minority = mean(afpr$minority, na.rm = TRUE),
         inhomelang = mean(afpr$inhomelang, na.rm = TRUE),
         round = 0)

generate_predictions_35 <- function(models, data) {
  # Initialize an empty list to store predictions
  predictions_list <- list()
  
  pred <- data.frame(matrix(nrow = 4))
  # colnames(pred) <- c('predict_est', 'predict_lwr', "predict_upr")

  # Iterate over each outcome variable
  for (i in 13:18) {
    
    print("linear")
    
    pred[, c("predict_est", "predict_lwr", "predict_upr")] <-
      predict(age_diff_models_binary[[i]][[1]], allcombos, interval = "confidence")
    
    print("logistic")
    
    pred[,c("glm_link","glm_link_se","glm_link_residual_scale")] <- predict(age_diff_models_binary[[i]][[2]],
                                                                                 newdata = allcombos,
                                                                                 type = "link",
                                                                                 se.fit = TRUE)
    
    pred$predict_est_log <- plogis(pred$glm_link)
    
    # same things with upper and lower limits
    pred$predict_lwr_log <- plogis(pred$glm_link - (1.96 * pred$glm_link_se))
    pred$predict_upr_log <- plogis(pred$glm_link + (1.96 * pred$glm_link_se))
    
    pred$outcome_variable <- as.character(age_diff_models_binary[[i]][[1]][["terms"]][[2]])
    
    print("data_frame")

    # Add the data frame to the list
    predictions_list[[i]] <- pred
    
    
  }
  
  # Combine all data frames into one
  predictions_df <- bind_rows(predictions_list, .id = "outcome_variable_no")
  
  return(predictions_df)
}

age_diff_models_binary_35 <- age_diff_models_binary[13:18]

predictions_df_35 <- generate_predictions_35(age_diff_models_binary_35, allcombos)

predictions_df_35 <- predictions_df_35 %>%
  mutate(coarsened_age_35 = rep(levels(afpr$coarsened_age_35), 6)) %>%
  mutate(label = plyr::mapvalues(outcome_variable, 
                                 as.character(variable_labels$var),
                                 as.character(variable_labels$label)))

# Plotting 35 ----

predictions_df_35 %>%
  ggplot(aes(x = coarsened_age_35, y = predict_est, col = "LM")) +
  theme_linedraw() +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = predict_lwr, ymax = predict_upr), alpha = 0.3, width = 0.3) +
  geom_point(aes(x = coarsened_age_35, y = predict_est_log, col = "GLM"), position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = predict_lwr_log, ymax = predict_upr_log, col = "GLM"), alpha = 0.3, width = 0.3) +
  # scale_y_continuous(breaks = seq(0, 1, 0.25),
  #                    limits = c(0,1)) +
  scale_colour_manual(values = c("black", "gray50")) +
  facet_grid(facets = vars(label)) +
  coord_flip() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.key.width=unit(3,"line"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(family = "Roboto"),
        panel.grid.major = element_line(color = 'gray80'),
        panel.grid.minor = element_line(color = 'gray80')) +
  theme_bw()

# 30 year split ----

coarsened_age_30 <- levels(afpr$coarsened_age_30)
allcombos_30 <- expand.grid(coarsened_age_30 = coarsened_age_30)
allcombos_30 <- allcombos_30 %>%
  mutate(noncoeth = 0, #valid to set this to 0? Ask AE?
         age = mean(afpr$age, na.rm = TRUE), #what to do with age here? Is this fine?
         edu = mean(afpr$edu, na.rm = TRUE),
         country = "Kenya", #I don't fucking know
         gender = 0, #hmmm le deuxième sexe
         urban = mean(afpr$urban, na.rm = TRUE),
         minority = mean(afpr$minority, na.rm = TRUE),
         inhomelang = mean(afpr$inhomelang, na.rm = TRUE),
         round = 0)

x[, c("predict_est", "predict_lwr", "predict_upr")] <-
  predict(age_diff_models_binary[[6]][[1]], allcombos_30, interval = "confidence")

generate_predictions_30 <- function(models, data) {
  # Initialize an empty list to store predictions
  predictions_list <- list()
  
  pred <- data.frame()
  # colnames(pred) <- c('predict_est', 'predict_lwr', "predict_upr")
  
  # Iterate over each outcome variable
  for (i in 6:12) {
    
    print("linear")
    
    pred[, c("predict_est", "predict_lwr", "predict_upr")] <-
      predict(age_diff_models_binary[[i]][[1]], allcombos_30, interval = "confidence")
    
    print("logistic")
    
    pred[,c("glm_link","glm_link_se","glm_link_residual_scale")] <- predict(age_diff_models_binary[[i]][[2]],
                                                                            newdata = allcombos_30,
                                                                            type = "link",
                                                                            se.fit = TRUE)
    
    pred$predict_est_log <- plogis(pred$glm_link)
    
    # same things with upper and lower limits
    pred$predict_lwr_log <- plogis(pred$glm_link - (1.96 * pred$glm_link_se))
    pred$predict_upr_log <- plogis(pred$glm_link + (1.96 * pred$glm_link_se))
    
    pred$outcome_variable <- as.character(age_diff_models_binary[[i]][[1]][["terms"]][[2]])
    
    print("data_frame")
    
    # Add the data frame to the list
    predictions_list[[i]] <- pred
  }
  
  # Combine all data frames into one
  predictions_df <- bind_rows(predictions_list, .id = "outcome_variable_no")
  
  return(predictions_df)
}

age_diff_models_binary_30 <- age_diff_models_binary[6:12]

predictions_df_30 <- generate_predictions_30(age_diff_models_binary_30, allcombos_30)

# Plotting 30 ----

predictions_df_30 <- predictions_df_30 %>%
  mutate(coarsened_age_30 = rep(levels(afpr$coarsened_age_30), 6))
mutate(label = plyr::mapvalues(outcome_variable, 
                               as.character(variable_labels$var),
                               as.character(variable_labels$label)))

predictions_df_40 %>%
  ggplot(aes(x = coarsened_age_40, y = predict_est, col = "LM")) +
  theme_linedraw() +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = predict_lwr, ymax = predict_upr), alpha = 0.3, width = 0.3) +
  geom_point(aes(x = coarsened_age_40, y = predict_est_log, col = "GLM"), position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = predict_lwr_log, ymax = predict_upr_log, col = "GLM"), alpha = 0.3, width = 0.3) +
  # scale_y_continuous(breaks = seq(0, 1, 0.25),
  #                    limits = c(0,1)) +
  scale_colour_manual(values = c("black", "gray50")) +
  facet_grid(facets = vars(label)) +
  coord_flip() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.key.width=unit(3,"line"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(family = "Roboto"),
        panel.grid.major = element_line(color = 'gray80'),
        panel.grid.minor = element_line(color = 'gray80')) +
  theme_bw()


# 40 year split ----

coarsened_age_40 <- levels(afpr$coarsened_age_40)
allcombos_40 <- expand.grid(coarsened_age_40 = coarsened_age_40)
allcombos_40 <- allcombos_40 %>%
  mutate(noncoeth = 0, #valid to set this to 0? Ask AE?
         age = mean(afpr$age, na.rm = TRUE), #what to do with age here? Is this fine?
         edu = mean(afpr$edu, na.rm = TRUE),
         country = "Kenya", #I don't fucking know
         gender = 0, #hmmm le deuxième sexe
         urban = mean(afpr$urban, na.rm = TRUE),
         minority = mean(afpr$minority, na.rm = TRUE),
         inhomelang = mean(afpr$inhomelang, na.rm = TRUE),
         round = 0)

generate_predictions_40 <- function(models, data) {
  # Initialize an empty list to store predictions
  predictions_list <- list()
  
  pred <- data.frame(matrix(nrow = 4))
  # colnames(pred) <- c('predict_est', 'predict_lwr', "predict_upr")
  
  # Iterate over each outcome variable
  for (i in 19:24) {
    
    print("linear")
    
    pred[, c("predict_est", "predict_lwr", "predict_upr")] <-
      predict(age_diff_models_binary[[i]][[1]], allcombos_40, interval = "confidence")
    
    print("logistic")
    
    pred[,c("glm_link","glm_link_se","glm_link_residual_scale")] <- predict(age_diff_models_binary[[i]][[2]],
                                                                            newdata = allcombos_40,
                                                                            type = "link",
                                                                            se.fit = TRUE)
    
    pred$predict_est_log <- plogis(pred$glm_link)
    
    # same things with upper and lower limits
    pred$predict_lwr_log <- plogis(pred$glm_link - (1.96 * pred$glm_link_se))
    pred$predict_upr_log <- plogis(pred$glm_link + (1.96 * pred$glm_link_se))
    
    pred$outcome_variable <- as.character(age_diff_models_binary[[i]][[1]][["terms"]][[2]])
    
    print("data_frame")
    
    # Add the data frame to the list
    predictions_list[[i]] <- pred
  }
  
  # Combine all data frames into one
  predictions_df <- bind_rows(predictions_list, .id = "outcome_variable_no")
  
  return(predictions_df)
}

age_diff_models_binary_40 <- age_diff_models_binary[19:24]

predictions_df_40 <- generate_predictions_40(age_diff_models_binary_40, allcombos_40)

#Adding coarsened_age_40 manually (better way to do this?)

predictions_df_40 <- predictions_df_40 %>%
  mutate(coarsened_age_40 = rep(levels(afpr$coarsened_age_40), 6)) %>%
  mutate(label = plyr::mapvalues(outcome_variable, 
                               as.character(variable_labels$var),
                               as.character(variable_labels$label)))

# Plotting 40 ----

predictions_df_40 %>%
  ggplot(aes(x = coarsened_age_40, y = predict_est, col = "LM")) +
  theme_linedraw() +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = predict_lwr, ymax = predict_upr), alpha = 0.3, width = 0.3) +
  geom_point(aes(x = coarsened_age_40, y = predict_est_log, col = "GLM"), position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = predict_lwr_log, ymax = predict_upr_log, col = "GLM"), alpha = 0.3, width = 0.3) +
  # scale_y_continuous(breaks = seq(0, 1, 0.25),
  #                    limits = c(0,1)) +
  scale_colour_manual(values = c("black", "gray50")) +
  facet_grid(facets = vars(label)) +
  coord_flip() +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size = 7),
        legend.key.width=unit(3,"line"),
        strip.background = element_blank(),
        strip.text = element_blank(),
        text = element_text(family = "Roboto"),
        panel.grid.major = element_line(color = 'gray80'),
        panel.grid.minor = element_line(color = 'gray80')) +
  theme_bw()
  