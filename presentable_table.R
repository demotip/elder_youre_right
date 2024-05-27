library(groundhog)

plot_30_yr <- c("cowplot", "tidyverse", "showtext", "sysfonts")
groundhog.library(plot_30_yr, "2021-11-01")

font_add_google("Roboto", "Roboto")
showtext_auto()

source("scripts/variable_labels.R")

age_diff_models_table <- readRDS("data_clean/age_diff_models.rds")

age_diff_models_table_grouped <- age_diff_models_table %>%
  filter(country == "All") %>%
  filter(!(term %in% c("coarsened_age_30", "coarsened_age_35", "coarsened_age_40"))) %>%
  mutate(., significant = ifelse(p.value < 0.05, 1, 0)) %>%
  mutate(age =
           case_when(grepl("old", ignore.case = T, term) ~
                       "Interviewer older (relative to younger)",
                     grepl("young", ignore.case = T, term) ~
                       "Interviewer younger (relative to older)",
                     TRUE ~ term))

unique(age_diff_models$group[!(age_diff_models$group == "youth_outcomes")])

sig_tables <- lapply(unique(age_diff_models$group[!(age_diff_models$group == "youth_outcomes")]), function(x) {
  
  table <- age_diff_models_table %>%
    filter(group == x) %>%
    filter(term != "noncoeth") %>%
    filter(age_variable != "coarsened_age_10") %>%
    filter(age_variable != "coarsened_age_35_originalscale") %>%
    mutate(sig_pos = ifelse(p.value < 0.05, ifelse(upper < 0, "-", "+"), "null")) %>% 
    pivot_wider(names_from = term,
                values_from = sig_pos, outcome_variable) 
  
  table$no_pos <- rowSums(table == "+")
  table$no_neg <- rowSums(table == "-")
  table$no_sig <- table$no_pos + table$no_neg
  
  return(table) }) %>%
  "names<-"(unique(age_diff_models$group[!(age_diff_models$group == "youth_outcomes")]))

sig_tables

## Youth outcomes needs to be done separately bc it's weird

youth_age_diff_mauritius <- age_diff_models_table %>%
  filter(group == "youth_outcomes") %>%
  filter(term != "noncoeth") %>%
  filter(country != "All") %>%
  filter(age_variable != "coarsened_age_10") %>%
  filter(age_variable != "coarsened_age_35_originalscale") %>%
  mutate(sig_pos = ifelse(p.value < 0.05, ifelse(upper < 0, "-", "+"), "null")) 

youth_age_diff_mauritius_wider <- youth_age_diff_mauritius %>%
  pivot_wider(names_from = term,
              values_from = sig_pos, outcome_variable,
              values_fn = list) %>%
  rename(younger_30_m = coarsened_age_30younger_int,
         younger_35_m = coarsened_age_35younger_int,
         younger_40_m = coarsened_age_40younger_int,
         older_30_m = coarsened_age_30older_int,
         older_35_m = coarsened_age_35older_int,
         older_40_m = coarsened_age_40older_int) %>%
  select(-outcome_variable)

youth_age_diff_all <- age_diff_models_table %>%
  filter(group == "youth_outcomes") %>%
  filter(term != "noncoeth") %>%
  filter(country != "Mauritius") %>%
  filter(age_variable != "coarsened_age_10") %>%
  filter(age_variable != "coarsened_age_35_originalscale") %>%
  mutate(sig_pos = ifelse(p.value < 0.05, ifelse(upper < 0, "-", "+"), "null")) 

youth_age_diff_all_wider <- youth_age_diff_all %>%
  pivot_wider(names_from = term,
              values_from = sig_pos, outcome_variable,
              values_fn = list)

youth_age_diff <- cbind(youth_age_diff_all_wider, youth_age_diff_mauritius_wider)

youth_age_diff$no_pos <- rowSums(youth_age_diff == "+")
youth_age_diff$no_neg <- rowSums(youth_age_diff == "-")
youth_age_diff$no_sig <- youth_age_diff$no_neg + youth_age_diff$no_pos

sig_tables$youth_outcomes <- youth_age_diff

## Exporting these as docx (need to figure out how to functionalize)

datasummary_df(sig_tables$pro_outcomes, output = "sig_table_pro.docx")
datasummary_df(sig_tables$pol_outcomes, output = "sig_table_pol.docx")
datasummary_df(sig_tables$stat_outcomes, output = "sig_table_stat.docx")
datasummary_df(sig_tables$eth_outcomes, output = "sig_table_eth.docx")
# datasummary_df(sig_tables$youth_outcomes, output = "sig_table_youth.docx") #not working for some reason? debug later 
