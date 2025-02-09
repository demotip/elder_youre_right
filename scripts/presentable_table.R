# This is a script that produces tables of the significance and direction of
# the age variable in all regressions.

# library(groundhog)

# plot_30_yr <- c("cowplot", "tidyverse", "showtext", "sysfonts")
# groundhog.library(plot_30_yr, "2021-11-01")

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

sig_tables <- lapply(unique(age_diff_models_table$group[!(age_diff_models_table$group == "youth_outcomes")]), function(x) {
  
  table <- age_diff_models_table %>%
    filter(group == x) %>%
    filter(term != "noncoeth") %>%
    filter(age_variable != "coarsened_age_10") %>%
    filter(age_variable != "coarsened_age_35_originalscale") %>%
    mutate(sig_pos = ifelse(p.value < 0.05, ifelse(upper < 0, "-", "+"), "null")) %>% 
    pivot_wider(names_from = term,
                values_from = sig_pos, id_cols =label) 
  
  table$no_pos <- rowSums(table == "+")
  table$no_neg <- rowSums(table == "-")
  table$no_sig <- table$no_pos + table$no_neg
  
  return(table) }) %>%
  "names<-"(unique(age_diff_models_table$group[!(age_diff_models_table$group == "youth_outcomes")]))

## Youth outcomes need to be done separately 

youth_mauritius <- age_diff_models_table %>%
  filter(group == "youth_outcomes") %>%
  filter(term != "noncoeth") %>%
  filter(country != "All") %>%
  filter(age_variable != "coarsened_age_10") %>%
  filter(age_variable != "coarsened_age_35_originalscale") %>%
  mutate(sig_pos = ifelse(p.value < 0.05, ifelse(upper < 0, "-", "+"), "null")) 

youth_mauritius_wider <- youth_mauritius %>%
  pivot_wider(names_from = term,
              values_from = sig_pos, 
              id_cols = label,
              values_fn = list) %>%
  dplyr::rename(., younger_30_m = coarsened_age_30younger_int,
         younger_35_m = coarsened_age_35younger_int,
         younger_40_m = coarsened_age_40younger_int,
         older_30_m = coarsened_age_30older_int,
         older_35_m = coarsened_age_35older_int,
         older_40_m = coarsened_age_40older_int) %>%
  mutate(younger_30_m = as.character(younger_30_m),
         younger_35_m = as.character(younger_35_m),
         younger_40_m = as.character(younger_40_m),
         older_30_m = as.character(older_30_m),
         older_35_m = as.character(older_35_m),
         older_40_m = as.character(older_40_m)) %>%
  dplyr::select(-label)

youth_all <- age_diff_models_table %>%
  filter(group == "youth_outcomes") %>%
  filter(term != "noncoeth") %>%
  filter(country != "Mauritius") %>%
  filter(age_variable != "coarsened_age_10") %>%
  filter(age_variable != "coarsened_age_35_originalscale") %>%
  mutate(sig_pos = ifelse(p.value < 0.05, ifelse(upper < 0, "-", "+"), "null")) 

youth_all_wider <- youth_all %>%
  pivot_wider(names_from = term,
              values_from = sig_pos, 
              id_cols = label,
              values_fn = list) %>%
  mutate(coarsened_age_30younger_int = as.character(coarsened_age_30younger_int),
         coarsened_age_35younger_int = as.character(coarsened_age_35younger_int),
         coarsened_age_40younger_int = as.character(coarsened_age_40younger_int),
         coarsened_age_30older_int = as.character(coarsened_age_30older_int),
         coarsened_age_35older_int = as.character(coarsened_age_35older_int),
         coarsened_age_40older_int = as.character(coarsened_age_40older_int)) 

youth_all_wider$no_pos_all <- rowSums(youth_all_wider == "+")
youth_all_wider$no_neg_all <- rowSums(youth_all_wider == "-")
youth_all_wider$no_sig_all <- youth_all_wider$no_neg_all + youth_all_wider$no_pos_all

youth_mauritius_wider$no_pos_m <- rowSums(youth_mauritius_wider == "+")
youth_mauritius_wider$no_neg_m <- rowSums(youth_mauritius_wider == "-")
youth_mauritius_wider$no_sig_m <- youth_mauritius_wider$no_neg_m + youth_mauritius_wider$no_pos_m

youth_age_diff <- as_tibble(cbind(youth_all_wider, youth_mauritius_wider))

sig_tables$youth_outcomes <- youth_age_diff

## Exporting these as .docx

# datasummary_df(sig_tables$pro_outcomes, output = "tables/significance_tables/sig_table_pro.docx")
# datasummary_df(sig_tables$pol_outcomes, output = "tables/significance_tables/sig_table_pol.docx")
# datasummary_df(sig_tables$stat_outcomes, output = "tables/significance_tables/sig_table_stat.docx")
# datasummary_df(sig_tables$eth_outcomes, output = "tables/significance_tables/sig_table_eth.docx")
# datasummary_df(sig_tables$youth_outcomes, output = "tables/significance_tables/sig_table_youth.docx") 
