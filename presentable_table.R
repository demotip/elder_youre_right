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

stat_age_diff <- age_diff_models_table %>%
  filter(group == "stat_outcomes") %>%
  filter(term != "noncoeth") %>%
  filter(age_variable != "coarsened_age_10") %>%
  filter(age_variable != "coarsened_age_35_originalscale") %>%
  mutate(sig_pos = ifelse(p.value < 0.05, ifelse(upper < 0, "-", "+"), "null")) %>% 
  pivot_wider(names_from = term,
              values_from = sig_pos, outcome_variable)

stat_age_diff$no_pos <- rowSums(stat_age_diff == "+")
stat_age_diff$no_neg <- rowSums(stat_age_diff == "-")
stat_age_diff$no_sig <- stat_age_diff$no_neg + stat_age_diff$no_pos
# 
# stat_age_diff <- stat_age_diff %>%
#   select(-no_pos, -no_neg)

eth_age_diff <- age_diff_models_table %>%
  filter(group == "eth_outcomes") %>%
  filter(term != "noncoeth") %>%
  filter(age_variable != "coarsened_age_10") %>%
  filter(age_variable != "coarsened_age_35_originalscale") %>%
  mutate(sig_pos = ifelse(p.value < 0.05, ifelse(upper < 0, "-", "+"), "null")) %>% 
  pivot_wider(names_from = term,
              values_from = sig_pos, outcome_variable)

eth_age_diff$no_pos <- rowSums(eth_age_diff == "+")
eth_age_diff$no_neg <- rowSums(eth_age_diff == "-")
eth_age_diff$no_sig <- eth_age_diff$no_neg + eth_age_diff$no_pos
# 
# eth_age_diff <- eth_age_diff %>%
#   select(-no_pos, -no_neg)

pol_age_diff <- age_diff_models_table %>%
  filter(group == "pol_outcomes") %>%
  filter(term != "noncoeth") %>%
  filter(age_variable != "coarsened_age_10") %>%
  filter(age_variable != "coarsened_age_35_originalscale") %>%
  mutate(sig_pos = ifelse(p.value < 0.05, ifelse(upper < 0, "-", "+"), "null")) %>% 
  pivot_wider(names_from = term,
              values_from = sig_pos, outcome_variable)

pol_age_diff$no_pos <- rowSums(pol_age_diff == "+")
pol_age_diff$no_neg <- rowSums(pol_age_diff == "-")
# pol_age_diff$no_sig <- pol_age_diff$no_neg + pol_age_diff$no_pos
# 
# pol_age_diff <- pol_age_diff %>%
#   select(-no_pos, -no_neg)

pro_age_diff <- age_diff_models_table %>%
  filter(group == "pro_outcomes") %>%
  filter(term != "noncoeth") %>%
  filter(age_variable != "coarsened_age_10") %>%
  filter(age_variable != "coarsened_age_35_originalscale") %>%
  mutate(sig_pos = ifelse(p.value < 0.05, ifelse(upper < 0, "-", "+"), "null")) %>% 
  pivot_wider(names_from = term,
              values_from = sig_pos, outcome_variable)

pro_age_diff$no_pos <- rowSums(pro_age_diff == "+")
pro_age_diff$no_neg <- rowSums(pro_age_diff == "-")
pro_age_diff$no_sig <- pro_age_diff$no_neg + pro_age_diff$no_pos
# 
# pro_age_diff <- pro_age_diff %>%
#   select(-no_pos, -no_neg)

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
#
# youth_age_diff <- youth_age_diff %>%
#   select(-no_pos, -no_neg)