
library(groundhog)

contrasts <- c("glue", "lfe", "tidyverse", "lmtest", "sandwich", "modelsummary", "janitor", "kableExtra")
groundhog.library(contrasts, "2021-11-01")

glance_custom.lm <- function(x, ...) {
  if (x[["terms"]][[3]][[3]][[2]] == "country") {
    out <- data.frame("Country fixed effects" = "Yes")
  } else {
    out <- data.frame("Country fixed effects" = "No")
  }
  
  return(out)
}

glance_custom.felm <- function(x, ...) {
  country_levels <- levels(as.factor(afpr$country))
  country_levels_formatted <- paste0("country",country_levels)
  
  if (country_levels_formatted %in% names(coef(x))) {
    out <- data.frame("Country fixed effects" = "Yes")
  } else {
    out <- data.frame("Country fixed effects" = "No")
  }
  
  return(out)
}

glance_custom.polr <- function(x, ...) {
  
  if (x[["terms"]][[3]][[3]] == "as.factor(country)") {
    out <- data.frame("Country fixed effects" = "Yes")
  } else {
    out <- data.frame("Country fixed effects" = "No")
  }
  
  return(out)
}

glance_custom.glm <- glance_custom.polr

### Defining model names, map of variable names ----
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

s <- c("30_split", "35_split", "40_split")
splits <- rep(s, each = 8)

names_stat <- rep(stat_outcomes, 3)
names_stat_split <- paste(names_stat, splits, sep = "-")

names_pol <- rep(pol_outcomes, 3)
names_pol_split <- paste(names_pol, splits, sep = "-")

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

form_base  <- paste0("{outcome_variable} ~ {age_variable} + ",
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

model_function <- function(data_frame) { pmap(data_frame, function(outcome, age_variable, round) {
    
    if(round == "7") include_round <- 7 else include_round <- 3:4
    
    outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
    
    print(outcome_variable)
      
      contrasts_matrix_list <- list(x = contrasts_matrix)
      names(contrasts_matrix_list) <- age_variable
      model <- felm(as.formula(glue(form_base)), data = afpr[afpr$round %in% include_round, ], 
                    contrasts = contrasts_matrix_list)
    
    return(model)
  }) } 

# Running the regression output functions for Adida fe ----

regression_output_stat <- outcome_age_combos_grouped %>%
    filter(group == "stat_outcomes") %>%
    dplyr::select(-group, -label) %>%
    model_function() %>%
   "names<-"(names_stat_split) %>%
  saveRDS(., file = "data_clean/regression_output_stat")

regression_output_pol <- outcome_age_combos_grouped %>%
  filter(group == "pol_outcomes") %>%
  dplyr::select(-group, -label) %>%
  model_function() %>%
  "names<-"(names_pol_split) %>%
  saveRDS(., file = "data_clean/regression_output_pol")

regression_output_pro <- outcome_age_combos_grouped %>%
  filter(group == "pro_outcomes") %>%
  dplyr::select(-group, -label) %>%
  model_function()  %>%
  "names<-"(names_pro_split) %>%
  saveRDS(., file = "data_clean/regression_output_pro")

regression_output_eth <- outcome_age_combos_grouped %>%
  filter(group == "eth_outcomes") %>%
  dplyr::select(-group, -label) %>%
  model_function()  %>% # Error: cannot allocate vector of size 49.6 Mb
  "names<-"(names_eth_split) %>%
  saveRDS(., file = "data_clean/regression_output_eth")

# regression_output_youth <- outcome_age_combos_grouped %>%
#   filter(group == "youth_outcomes") %>%
#   select(-group, -label) %>%
#   model_function() %>%
#   # "names<-"(names_youth_split) %>%
#   saveRDS(., file = "data_clean/regression_output_youth")

### Plotting adida fe ----

regression_output_adidafe <- list()

regression_output_adidafe$stat <- readRDS(file = "data_clean/regression_output_stat")
regression_output_adidafe$pol <- readRDS(file = "data_clean/regression_output_pol")
regression_output_adidafe$pro <- readRDS(file = "data_clean/regression_output_pro")
regression_output_adidafe$eth <- readRDS(file = "data_clean/regression_output_eth")

modelsummary_function_adida <- function(data_frame) {
  modelsummary(data_frame, coef_omit = "enumeth|region|tribe",
               coef_map = cm,
               estimate = c("{estimate} ({std.error}){stars}"),
               fmt = 2,
               statistic = NULL,
               output = "latex",  #undocumented but need to specify here to output as latex
               caption = paste0("adida fixed effects"),
               booktabs = TRUE,
               linesep = "", 
               escape = TRUE) |>
    kable_styling(
      font_size = 7,
      full_width = FALSE,
      latex_options = c("HOLD_position")
    ) |>
    row_spec(0, bold = TRUE, angle = -90, align = "c") |>
    column_spec(2:17, width ="3em") |>
    add_footnote("Add footnote here if needed.",
                 threeparttable = TRUE, 
                 escape = FALSE,
                 notation = "none") 
}

regression_tables_adidafe <- lapply(regression_output_adidafe, modelsummary_function_adida)

regression_tables_adidafe

regression_tables_adidafe |>
  save_kable(
    file.path(paste0("tables/regression_tables_adidafe.tex")),
    keep_tex = FALSE,
    self_contained = FALSE)

# Running the country fe models ----

form_base_country  <- paste0("{outcome_variable} ~ {age_variable} + ",
                     "noncoeth +", 
                     "age + gender + edu + ",
                     "urban + minority +",
                     "round + inhomelang + as.factor(country)")

model_function_countryfe <- function(data_frame) { pmap(data_frame, function(outcome, age_variable, round) {
  
  if(round == "7") include_round <- 7 else include_round <- 3:4
  
  outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
  
  print(outcome_variable)
  
  contrasts_matrix_list <- list(x = contrasts_matrix)
  names(contrasts_matrix_list) <- age_variable
  model <- lm(as.formula(glue(form_base_country)), data = afpr[afpr$round %in% include_round, ], 
                contrasts = contrasts_matrix_list)
  
  return(model)
}) } 

regression_output_stat_countryfe <- outcome_age_combos_grouped %>%
  filter(group == "stat_outcomes") %>%
  dplyr::select(-group, -label) %>%
  model_function_countryfe() %>%
  "names<-"(names_stat_split) %>%
  saveRDS(., file = "data_clean/regression_output_stat_countryfe") 

regression_output_pol_countryfe <- outcome_age_combos_grouped %>%
  filter(group == "pol_outcomes") %>%
  dplyr::select(-group, -label) %>%
  model_function_countryfe() %>%
  "names<-"(names_pol_split) %>%
  saveRDS(., file = "data_clean/regression_output_pol_countryfe")

regression_output_pro_countryfe <- outcome_age_combos_grouped %>%
  filter(group == "pro_outcomes") %>%
  dplyr::select(-group, -label) %>%
  model_function_countryfe() %>%
  "names<-"(names_pro_split) %>%
  saveRDS(., file = "data_clean/regression_output_pro_countryfe")

regression_output_eth_countryfe <- outcome_age_combos_grouped %>%
  filter(group == "eth_outcomes") %>%
  dplyr::select(-group, -label) %>%
  model_function_countryfe() %>%
  "names<-"(names_eth_split) %>%
  saveRDS(., file = "data_clean/regression_output_eth_countryfe")

# Plotting the country fe models ----

regression_output_countryfe <- list()

regression_output_countryfe$stat <- readRDS(file = "data_clean/regression_output_stat_countryfe")
regression_output_countryfe$pol <- readRDS(file = "data_clean/regression_output_pol_countryfe")
regression_output_countryfe$pro <- readRDS(file = "data_clean/regression_output_pro_countryfe")
regression_output_countryfe$eth <- readRDS(file = "data_clean/regression_output_eth_countryfe")

modelsummary_function <- function(data_frame) {
  modelsummary(data_frame, coef_omit = "enumeth|region|tribe|country",
               coef_map = cm,
               estimate = c("{estimate} ({std.error}){stars}"),
               fmt = 2,
               statistic = NULL,
               output = "latex",  #undocumented but need to specify here to output as latex
               caption = paste0("country fixed effects"),
               booktabs = TRUE,
               linesep = "", 
               escape = TRUE) |>
  kable_styling(
    font_size = 7,
    full_width = FALSE,
    latex_options = c("HOLD_position")
  ) |>
  row_spec(0, bold = TRUE, angle = -90, align = "c") |>
  column_spec(2:17, width ="3em") |>
  add_footnote("Add footnote here if needed.",
               threeparttable = TRUE, 
               escape = FALSE,
               notation = "none") 
  }

regression_tables_countryfe <- lapply(regression_output_countryfe, modelsummary_function)

regression_tables_countryfe |>
  save_kable(
        file.path(paste0("tables/regression_tables_countryfe.tex")),
        keep_tex = FALSE,
        self_contained = FALSE)

# #### REGRESSION TABLES FOR LOGISTIC/ORDERED LOGISTIC FIXED EFFECTS ----

form_base_factor  <- paste0("as.factor({outcome_variable}) ~ {age_variable} + ",
                            "noncoeth +",
                            "age + gender + edu + ",
                            "urban + minority +",
                            "round + inhomelang + country") #same as in cross_validated.R - defined again here for convenience

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

  print(outcome_variable)

    contrasts_matrix_list <- list(x = contrasts_matrix)
    names(contrasts_matrix_list) <- age_variable

    if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
      print(5)

      model <- glm(as.formula(glue(form_base_factor)),
                   data = afpr[afpr$round %in% include_round, ],
                   contrasts = contrasts_matrix_list,
                   family = "binomial")

    } else {

      print(6)

      model <- MASS::polr(as.formula(glue(form_base_factor)),
                          method = "logistic",
                          data = afpr[afpr$round %in% include_round, ],
                          # contrasts = list("{age_variable}" = contrasts_matrix_polr),
                          Hess = T)

      }

  return(model)
  }) }

regression_output_stat_logistic <- outcome_age_combos_grouped %>%
  filter(group == "stat_outcomes") %>%
  dplyr::select(-group, -label) %>%
  model_function_logistic() %>%
  "names<-"(names_stat_split) %>%
  saveRDS(., file = "data_clean/regression_output_stat_logistic") 

regression_output_pol_logistic <- outcome_age_combos_grouped %>%
  filter(group == "pol_outcomes") %>%
  dplyr::select(-group, -label) %>%
  model_function_logistic() %>%
  "names<-"(names_pol_split) %>%
  saveRDS(., file = "data_clean/regression_output_pol_logistic")

regression_output_pro_logistic <- outcome_age_combos_grouped %>%
  filter(group == "pro_outcomes") %>%
  dplyr::select(-group, -label) %>%
  model_function_logistic() %>%
  "names<-"(names_pro_split) %>%
  saveRDS(., file = "data_clean/regression_output_pro_logistic")

regression_output_eth_logistic <- outcome_age_combos_grouped %>%
  filter(group == "eth_outcomes") %>%
  dplyr::select(-group, -label) %>%
  model_function_logistic() %>%
  "names<-"(names_eth_split) %>%
  saveRDS(., file = "data_clean/regression_output_eth_logistic")

### Plotting logistic ----

regression_output_logistic <- list()

regression_output_logistic$stat <- readRDS(file = "data_clean/regression_output_stat_logistic")
regression_output_logistic$pol <- readRDS(file = "data_clean/regression_output_pol_logistic")
regression_output_logistic$pro <- readRDS(file = "data_clean/regression_output_pro_logistic")
regression_output_logistic$eth <- readRDS(file = "data_clean/regression_output_eth_logistic")

modelsummary_function_logistic <- function(data_frame) {
  modelsummary(data_frame, coef_omit = "enumeth|region|tribe",
               coef_map = cm,
               estimate = c("{estimate} ({std.error})"),
               fmt = 2,
               statistic = NULL,
               output = "latex",  #undocumented but need to specify here to output as latex
               caption = paste0("Logistic and ordinal regressions"),
               booktabs = TRUE,
               linesep = "", 
               escape = TRUE) |>
    kable_styling(
      font_size = 7,
      full_width = FALSE,
      latex_options = c("HOLD_position")
    ) |>
    row_spec(0, bold = TRUE, angle = -90, align = "c") |>
    column_spec(2:17, width ="3em") |>
    add_footnote("Add footnote here if needed.",
                 threeparttable = TRUE, 
                 escape = FALSE,
                 notation = "none") 
}

regression_tables_logistic <- lapply(regression_output_logistic, modelsummary_function_logistic)

regression_tables_logistic

regression_tables_logistic |>
  save_kable(
    file.path(paste0("tables/regression_tables_logistic.tex")),
    keep_tex = FALSE,
    self_contained = FALSE)

