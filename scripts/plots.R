# PLOTS FROM PAPER ----

library(groundhog)

plot_30_yr <- c("cowplot", "tidyverse", "showtext", "sysfonts")
groundhog.library(plot_30_yr, "2021-11-01")

font_add_google("Roboto", "Roboto")
showtext_auto()

source("scripts/variable_labels.R")

age_diff_models_og <- readRDS("data_clean/age_diff_models.rds")

# Clean up coefficient estimates df ----

age_diff_models <- age_diff_models_og %>%
  filter(country == "All") %>%
  filter(!(term %in% c("coarsened_age_30", "coarsened_age_35", "coarsened_age_40"))) %>%
  # Clean up coefficient estimate names
  mutate(term =
           case_when(grepl("Interviewer younger", term) ~
                       "Interviewer under 10 years younger than respondent\n(relative to interviewer's age within 10 years of respondent)",
                     grepl("Interviewer older", term) ~
                       "Interviewer over 10 years older then respondents\n(relative to interviewer's age within 10 years of respondent)",
                     grepl("30.*older_int", term) ~
                       "Interviewer over 30 (relative to both 30 and under)",
                     grepl("30.*younger_int", term) ~
                       "Interviewer 30 and under (relative to both over 30)",
                     grepl("35.*older_int", term) ~
                       "Interviewer over 35 (relative to both 35 and under)",
                     grepl("35.*younger_int", term) ~
                       "Interviewer 35 and under (relative to both over 35)",
                     grepl("40.*older_int", term) ~
                       "Interviewer over 40 (relative to both 40 and under)",
                     grepl("40.*younger_int", term) ~
                       "Interviewer 40 and under (relative to both over 40)",
                     grepl("noncoeth", term) ~
                       "Non-coethnic interviewer",
                     TRUE ~ term))

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
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 7),
          legend.key.width=unit(3,"line"),
          strip.background = element_blank(),
          strip.text = element_blank(),
          text = element_text(family = "Roboto"),
          panel.grid.major = element_line(color = 'gray80'),
          panel.grid.minor = element_line(color = 'gray80')) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    guides(colour = guide_legend(reverse = TRUE,
                                 ncol = 1),
           linetype = guide_legend(reverse = TRUE,
                                   ncol = 1),
           shape = guide_legend(reverse = TRUE,
                                ncol = 1)) +
    ylab("\nEstimated effect (in SDs) of non-coethnic interviewer \nand age difference, with 95% confidence intervals")
}

# 10 year age difference plots ----

plot10yr <- lapply(unique(age_diff_models$group), function(x) {
  plot <- age_diff_models %>%
    filter(age_variable == "coarsened_age_10") %>%
    filter(group == x) %>%
    plotfun()
  
  if(x == "pol_outcomes") {
    plot <- plot +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1),
                         limits = c(-0.4, 0.4)) +
      geom_vline(xintercept = 2.5, size = 0.3) 
    # +
    #   geom_vline(xintercept = 4.5, size = 0.3)
  } else if (x == "stat_outcomes") {
    plot <- plot +
      geom_vline(xintercept = 1.5, size = 0.3)
  } else if (x == "pro_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.2, 0.2)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.05))
  } else if (x == "youth_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.4, 0.4)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1))
  }
  return(plot)
}) %>%
  "names<-"(unique(age_diff_models$group))

plot10yr$youth_outcomes <- plot10yr$youth_outcomes +
  # Facet the question that was asked in all countries
  # "Addressing *needs* of youth"
  facet_grid(rows = vars(grepl("needs", outcome_variable)),
             labeller = labeller(.rows = function(x) {
               ifelse(x, "All\ncountries", "Mauritius only")
             }),
             scales = "free", space = "free") +
  theme(strip.text.y = element_text(size = 8, margin = margin(l = 3),
                                    vjust = 0, hjust = 0.5))

### Align x-axes of plots
## This constrains the plot rectangles to all be
## the same size and makes it easier to compare
## effect sizes. Without this, varying variable
## label length makes all the plots different sizes

plot10yr_align <- align_plots(plotlist = plot10yr,
                              align = "hv",
                              axis = "tblr") %>%
  lapply(ggdraw)



map2(names(plot10yr_align), c(5, 8, 5, 3.5, 5), function(x, y) {
  save_plot(paste0("figs/", x, "10yr.pdf"),
            plot10yr_align[[x]],
            base_width = 7,
            base_height = y) })


# 30 year age difference plots ----

plot30yr <- lapply(unique(age_diff_models$group), function(x) {
  plot <- age_diff_models %>%
    filter(country == "All") %>%
    filter(age_variable == "coarsened_age_30") %>%
    filter(group == x) %>%
    plotfun()
  
  if(x == "pol_outcomes") {
    plot <- plot +
      geom_vline(xintercept = 2.5, size = 0.3) 
    # +
    #   geom_vline(xintercept = 4.5, size = 0.3)
  } else if (x == "stat_outcomes") {
    plot <- plot +
      geom_vline(xintercept = 1.5, size = 0.3)
  } else if (x == "pro_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.2, 0.2)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.05))
  } else if (x == "youth_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.7, 0.7)) +
      scale_y_continuous(breaks = seq(-0.8, 0.8, 0.1))
  }
  return(plot)
}) %>%
  "names<-"(unique(age_diff_models$group))


plot30yr$youth_outcomes <- plot30yr$youth_outcomes +
  # Facet the question that was asked in all countries
  # "Addressing *needs* of youth"
  facet_grid(rows = vars(grepl("needs", outcome_variable)),
             labeller = labeller(.rows = function(x) {
               ifelse(x, "All\ncountries", "Mauritius only")
             }),
             scales = "free", space = "free") +
  theme(strip.text.y = element_text(size = 8, margin = margin(l = 3),
                                    vjust = 0, hjust = 0.5))

plot30yr_align <- align_plots(plotlist = plot30yr,
                              align = "hv",
                              axis = "tblr") %>%
  lapply(ggdraw)

map2(names(plot30yr_align), c(5, 8, 5, 5, 5), function(x, y) {
  save_plot(paste0("figs/", x, "30yr.pdf"),
            plot30yr_align[[x]],
            base_width = 7,
            base_height = y)
})

# 35 year age difference plots ----

plot35yr <- lapply(unique(age_diff_models$group), function(x) {
  plot <- age_diff_models %>%
    filter(country == "All") %>%
    filter(age_variable == "coarsened_age_35") %>%
    filter(group == x) %>%
    plotfun()
  
  if(x == "pol_outcomes") {
    plot <- plot +
      geom_vline(xintercept = 2.5, size = 0.3)
      # geom_vline(xintercept = 4.5, size = 0.3)
  } else if (x == "stat_outcomes") {
    plot <- plot +
      geom_vline(xintercept = 1.5, size = 0.3)
  } else if (x == "pro_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.2, 0.2)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.05))
  } else if (x == "youth_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.7, 0.7)) +
      scale_y_continuous(breaks = seq(-0.8, 0.8, 0.1))
  }
  return(plot)
}) %>%
  "names<-"(unique(age_diff_models$group))


plot35yr$youth_outcomes <- plot35yr$youth_outcomes +
  # Facet the question that was asked in all countries
  # "Addressing *needs* of youth"
  facet_grid(rows = vars(grepl("needs", outcome_variable)),
             labeller = labeller(.rows = function(x) {
               ifelse(x, "All\ncountries", "Mauritius only")
             }),
             scales = "free", space = "free") +
  theme(strip.text.y = element_text(size = 8, margin = margin(l = 3),
                                    vjust = 0, hjust = 0.5))

plot35yr_align <- align_plots(plotlist = plot35yr,
                              align = "hv",
                              axis = "tblr") %>%
  lapply(ggdraw)

map2(names(plot35yr_align), c(5, 8, 5, 5, 5), function(x, y) {
  save_plot(paste0("figs/", x, "35yr.pdf"),
            plot35yr_align[[x]],
            base_width = 7,
            base_height = y)
})

# 40 year age difference plots ----
plot40yr <- lapply(unique(age_diff_models$group), function(x) {
  plot <- age_diff_models %>%
    filter(age_variable == "coarsened_age_40") %>%
    filter(group == x) %>%
    plotfun()
  
  if(x == "pol_outcomes") {
    plot <- plot +
      geom_vline(xintercept = 2.5, size = 0.3) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1)) 
    # +
    #   geom_vline(xintercept = 4.5, size = 0.3)
  } else if (x == "stat_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.4, 0.4)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1)) +
      geom_vline(xintercept = 1.5, size = 0.3)
  } else if (x == "pro_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.4, 0.4)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1))
  } else if (x == "youth_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.4, 0.4)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1))
  }
  return(plot)
}) %>%
  "names<-"(unique(age_diff_models$group))

plot40yr$youth_outcomes <- plot40yr$youth_outcomes +
  # Facet the question that was asked in all countries
  # "Addressing *needs* of youth"
  facet_grid(rows = vars(grepl("needs", outcome_variable)),
             labeller = labeller(.rows = function(x) {
               ifelse(x, "All\ncountries", "Mauritius only")
             }),
             scales = "free", space = "free") +
  theme(strip.text.y = element_text(size = 8, margin = margin(l = 3),
                                    vjust = 0, hjust = 0.5))

plot40yr_align <- align_plots(plotlist = plot40yr,
                              align = "hv",
                              axis = "tblr") %>%
  lapply(ggdraw)

plot40yr_align

map2(names(plot40yr_align), c(5, 8, 5, 3.5, 5), function(x, y) {
  save_plot(paste0("figs/", x, "40yr.pdf"),
            plot40yr_align[[x]],
            base_width = 7,
            base_height = y)
})

# 35 year age difference results (original scale) table ----

age_diff_models %>%
  filter(age_variable == "coarsened_age_35_originalscale") %>%
  dplyr::select(term, label, estimate, std.error, upper, lower) %>%
  mutate_if(is.numeric, list(~round(., 4))) %>%
  write.csv("tables/effects_original_scales.csv")

# For the paper: 35 year age difference plots with country fixed effects ----

age_diff_models_cfe <- readRDS("data_clean/age_diff_models_countryfe.rds")

age_diff_models_fe <- age_diff_models_cfe %>%
  filter(country == "All") %>%
  filter(!(term %in% c("coarsened_age_30", "coarsened_age_35", "coarsened_age_40"))) %>%
  mutate(age =
           case_when(grepl("old", ignore.case = T, term) ~
                       "old",
                     grepl("young", ignore.case = T, term) ~
                       "young",
                     TRUE ~ term)) %>%
  mutate(age_label =
           case_when(grepl("old", ignore.case = T, age) ~
                       "Effect of old interviewer on young respondents",
                     grepl("young", ignore.case = T, age) ~
                       "Effect of young interviewer on old respondents",
                     grepl("noncoeth", ignore.case = T, age) ~
                       "Non-coethnic interviewer",
                     TRUE ~ age)) %>%
  rename(p_value = p.value) %>%
  select(-term, -std.error, -statistic, -n_obs) %>%
  pivot_wider(names_from = c(age_variable), 
              values_from = c(estimate, p_value, upper, lower),
              names_sep = ".") %>%
  #   mutate(., sig_across = ifelse(age != "noncoeth", 
  #                                 ifelse(p_value.coarsened_age_30 < 0.05 &
  #                                          p_value.coarsened_age_35 < 0.05 &
  #                                          p_value.coarsened_age_40 < 0.05, 1, 0), 0)) %>%
  #   mutate(., sign_flip = ifelse(age != "noncoeth", 
  #                                ifelse(p_value.coarsened_age_35 < 0.05,
  #                                       ifelse(p_value.coarsened_age_40 < 0.05 &
  #                                                lower.coarsened_age_35*lower.coarsened_age_40 < 0 , 1,
  #                                              ifelse(p_value.coarsened_age_30 < 0.05 &
  #                                                       lower.coarsened_age_35*lower.coarsened_age_30 < 0 , 1, 0)), 0), 0)) 
  mutate(label = as.factor(label)) %>% 
  mutate(label = fct_relevel(fct_reorder(label, p_value.coarsened_age_35, .fun = min),
                             "Knows MP's name", "Exposure to vote buying"))

plotfun_wide_fe <- function(data) {
  data %>%
    ggplot(aes(label,
               estimate.coarsened_age_35,
               colour = age_label,
               shape = age_label
               # linetype = factor(sign_flip), # extra visual indicators for sign_flip and sig_across conditions
               # size = factor(sig_across)
    )) +
    theme_linedraw() +
    geom_point(position = position_dodge(width = 0.5)
               # size = 2
    ) +
    geom_errorbar(aes(ymin = lower.coarsened_age_35, 
                      ymax = upper.coarsened_age_35), 
                  width = 0.4,
                  position = position_dodge(width = 0.5)) +
    scale_colour_manual(values = c("gray50", "black", "gray80")) +
    scale_shape_manual(values = c(17,15,19)) +
    scale_size_manual(values = c(0.5,1.5)) + 
    coord_flip() +
    scale_y_continuous(breaks = seq(-0.2, 0.2, 0.05),
                       limits = c(-0.2, 0.2)) +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 7),
          legend.key.width=unit(3,"line"),
          strip.background = element_blank(),
          strip.text = element_blank(),
          text = element_text(family = "Roboto"),
          panel.grid.major = element_line(color = 'gray80'),
          panel.grid.minor = element_line(color = 'gray80')) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    guides(colour = guide_legend(reverse = TRUE,
                                 ncol = 1),
           linetype = "none",
           shape = guide_legend(reverse = TRUE,
                                ncol = 1),
           size = "none"
    ) +
    ylab("\nEstimated effect (in SDs) of non-coethnic interviewer \nand age difference, with 95% confidence intervals: \n Fixed effects by country only") }

plot35yr_countryfe <- lapply(unique(age_diff_models_fe$group), function(x) {
  plot <- age_diff_models_fe %>%
    filter(group == x) %>%
    plotfun_wide_fe()
  
  if(x == "pol_outcomes") {
    plot <- plot +
      geom_vline(xintercept = 2.5, size = 0.3)
    # geom_vline(xintercept = 4.5, size = 0.3)
  } else if (x == "stat_outcomes") {
    plot <- plot +
      geom_vline(xintercept = 1.5, size = 0.3)
  } else if (x == "pro_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.2, 0.2)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.05))
  } else if (x == "youth_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.7, 0.7)) +
      scale_y_continuous(breaks = seq(-0.8, 0.8, 0.1))
  }
  return(plot)
}) %>%
  "names<-"(unique(age_diff_models_fe$group))


plot35yr_countryfe$youth_outcomes <- plot35yr_countryfe$youth_outcomes +
  # Facet the question that was asked in all countries
  # "Addressing *needs* of youth"
  facet_grid(rows = vars(grepl("needs", outcome_variable)),
             labeller = labeller(.rows = function(x) {
               ifelse(x, "All\ncountries", "Mauritius only")
             }),
             scales = "free", space = "free") +
  theme(strip.text.y = element_text(size = 8, margin = margin(l = 3),
                                    vjust = 0, hjust = 0.5))

plot35yr_countryfe_align <- align_plots(plotlist = plot35yr_countryfe,
                                        align = "hv",
                                        axis = "tblr") %>%
  lapply(ggdraw)

map2(names(plot35yr_countryfe_align), c(5, 8, 5, 5, 5), function(x, y) {
  save_plot(paste0("figs/", x, "35yr_countryfe.pdf"),
            plot35yr_countryfe_align[[x]],
            base_width = 7,
            base_height = y)
})

# For the paper: 35 year age difference plots, standard fixed effects ----

age_diff_models_wide <- age_diff_models_int %>%
  mutate(age =
           case_when(grepl("old", ignore.case = T, term) ~
                       "old",
                     grepl("young", ignore.case = T, term) ~
                       "young",
                     TRUE ~ term)) %>%
  mutate(age_label =
           case_when(grepl("old", ignore.case = T, age) ~
                       "Effect of old interviewer on young respondents",
                     grepl("young", ignore.case = T, age) ~
                       "Effect of young interviewer on old respondents",
                     grepl("noncoeth", ignore.case = T, age) ~
                       "Non-coethnic interviewer",
                     TRUE ~ age)) %>%
  rename(p_value = p.value) %>%
  select(-term, -std.error, -statistic, -n_obs, -country) %>%
  filter(age_variable != "coarsened_age_10") %>%
  filter(age_variable != "coarsened_age_35_originalscale") %>%
  pivot_wider(names_from = c(age_variable),
              values_from = c(estimate, p_value, upper, lower),
              names_sep = ".") %>%
  mutate(label = as.factor(label)) %>%
  mutate(., label = factor(label, levels = levels(age_diff_models_fe$label)))

plotfun_wide <- function(data) {
  data %>%
    ggplot(aes(label,
               estimate.coarsened_age_35,
               colour = age_label,
               # linetype = factor(sign_flip),
               shape = age_label
               # size = factor(sig_across)
    )) +
    theme_linedraw() +
    geom_point(position = position_dodge(width = 0.5)
               # size = 2
    ) +
    geom_errorbar(aes(ymin = lower.coarsened_age_35,
                      ymax = upper.coarsened_age_35),
                  width = 0.4,
                  position = position_dodge(width = 0.5)) +
    scale_colour_manual(values = c("gray50", "black", "gray80")) +
    scale_shape_manual(values = c(17,15,19)) +
    scale_size_manual(values = c(0.5,1.5)) +
    coord_flip() +
    scale_y_continuous(breaks = seq(-0.2, 0.2, 0.05),
                       limits = c(-0.2, 0.2)) +
    theme(axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.position = "bottom",
          legend.text = element_text(size = 7),
          legend.key.width = unit(3,"line"),
          strip.background = element_blank(),
          strip.text = element_blank(),
          text = element_text(family = "Roboto"),
          panel.grid.major = element_line(color = 'gray80'),
          panel.grid.minor = element_line(color = 'gray80')) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    guides(colour = guide_legend(reverse = TRUE,
                                 ncol = 1),
           linetype = "none",
           shape = guide_legend(reverse = TRUE,
                                ncol = 1),
           size = "none"
    ) +
    ylab("\nEstimated effect (in SDs) of non-coethnic interviewer \nand age difference, with 95% confidence intervals")
}

plot35yr_paper <- lapply(unique(age_diff_models_wide$group), function(x) {
  plot <- age_diff_models_wide %>%
    filter(group == x) %>%
    plotfun_wide()
  
  if(x == "pol_outcomes") {
    plot <- plot +
      geom_vline(xintercept = 2.5, size = 0.3)
    # geom_vline(xintercept = 4.5, size = 0.3)
  } else if (x == "stat_outcomes") {
    plot <- plot +
      geom_vline(xintercept = 1.5, size = 0.3)
  } else if (x == "pro_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.2, 0.2)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.05))
  } else if (x == "youth_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.7, 0.7)) +
      scale_y_continuous(breaks = seq(-0.8, 0.8, 0.1))
  }
  else if (x == "eth_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.2, 0.2)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.05))
  }
  return(plot)
}) %>%
  "names<-"(unique(age_diff_models$group))

plot35yr_paper$youth_outcomes <- plot35yr_paper$youth_outcomes +
  # Facet the question that was asked in all countries
  # "Addressing *needs* of youth"
  facet_grid(rows = vars(grepl("needs", outcome_variable)),
             labeller = labeller(.rows = function(x) {
               ifelse(x, "All\ncountries", "Mauritius only")
             }, .cols = names),
             scales = "free" , space = "free" ) +
  theme(strip.text.y = element_text(size = 8, margin = margin(l = 3),
                                    vjust = 0, hjust = 0.5))

plot35yr_paper_align <- align_plots(plotlist = plot35yr_paper,
                                    align = "hv",
                                    axis = "tblr") %>%
  lapply(ggdraw)

map2(names(plot35yr_paper_align), c(5, 8, 5, 5, 5), function(x, y) {
  save_plot(paste0("figs/", x, "paper.pdf"),
            plot35yr_paper_align[[x]],
            base_width = 7,
            base_height = y)
})

# PLOT FUNCTION FOR APPENDIX PLOTS W/ ALL CUTOFFS ----

plotfun_integrated <- function(data) {
  data %>%
    ggplot(aes(label,
               estimate,
               colour = age,
               linetype = age,
               shape = age)) +
    theme_linedraw() +
    geom_point(position = position_dodge(width = 0.5)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), #linewidth = var, linetype = var
                  position = position_dodge(width = 0.5),
                  width = 0.3) +
    facet_wrap(~age_variable, labeller = labeller(age_variable = names)) +
    scale_colour_manual(values = c("gray50", "black","gray50", "black",
                                   "gray50", "black", "grey50")) +
    scale_shape_manual(values = c(17,15,19,21,23,25,27)) +
    scale_linetype_manual(values = c(rep("solid", 7))) +
    coord_flip() +
    # scale_y_continuous(breaks = seq(-0.2, 0.2, 0.05),
    #                    limits = c(-0.2, 0.2)) +
    theme(legend.position = "bottom",
          axis.title.y = element_blank(),
          legend.title = element_blank(),
          legend.text = element_text(size = 7),
          legend.key.width= unit(3,"line"), 
          text = element_text(family = "Roboto"),
          panel.grid.major = element_line(color = 'gray80'),
          panel.grid.minor = element_line(color = 'gray80')) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    guides(colour = guide_legend(reverse = TRUE,
                                 ncol = 2),
           linetype = guide_legend(reverse = TRUE,
                                   ncol = 1),
           shape = guide_legend(reverse = TRUE,
                                ncol = 1)) +
    ylab("\nEstimated effect (in SDs) of age difference, with 95% confidence intervals")
}

# Age plots w/ all cutoffs, adida fe ----

# First, making necessary changes to the age_diff_models df

age_diff_models_int <- age_diff_models_og %>%
  filter(country == "All") %>%
  filter(!(term %in% c("coarsened_age_30", "coarsened_age_35", "coarsened_age_40"))) %>%
  # Clean up coefficient estimate names
  mutate(age =
           case_when(grepl("old", ignore.case = T, term) ~
                       "Effect of old interviewer on young respondents",
                     grepl("young", ignore.case = T, term) ~
                       "Effect of young interviewer on old respondents",
                     TRUE ~ term)) %>%
  mutate(., label = factor(label, levels = levels(age_diff_models_wide$label)))
  
  names <- c(coarsened_age_30 = "30 cutoff",
             coarsened_age_35 = "35 cutoff",
             coarsened_age_40 = "40 cutoff")

plots_integrated <- lapply(unique(age_diff_models_int$group), function(x) {
  
  plot <- age_diff_models_int %>%
    filter(country == "All") %>%
    filter(group == x) %>%
    filter(age_variable != "coarsened_age_10") %>%
    filter(age_variable != "coarsened_age_35_originalscale" ) %>%
    filter(term != "noncoeth") %>%
    plotfun_integrated()
  
  if(x == "pol_outcomes") {
    plot <- plot +
      geom_vline(xintercept = 2.5, size = 0.3)
    # geom_vline(xintercept = 4.5, size = 0.3)
  }
  
  return(plot) }) %>%
  "names<-"(unique(age_diff_models_int$group))

plots_integrated$youth_outcomes <- plots_integrated$youth_outcomes +
  # Facet the question that was asked in all countries
  # "Addressing *needs* of youth"
  facet_grid(rows = vars(grepl("needs", outcome_variable)),
             labeller = labeller(.rows = function(x) {
               ifelse(x, "All\ncountries", "Mauritius only")
             }, .cols = names),
             cols = vars(age_variable),
             scales = "free" , space = "free" ) +
  theme(strip.text.y = element_text(size = 8, margin = margin(l = 3),
                                    vjust = 0, hjust = 0.5)) 

plots_integrated_align <- align_plots(plotlist = plots_integrated,
                                      align = "hv",
                                      axis = "tblr") %>%
  lapply(ggdraw)

map2(names(plots_integrated_align), c(5, 8, 5, 3.5, 5), function(x, y) {
  save_plot(paste0("figs/", x, "integrated.pdf"),
            plots_integrated_align[[x]],
            base_width = 10,
            base_height = y)
})

# age_diff_models %>%
#   filter(!(term %in% c("coarsened_age_30", "coarsened_age_35", "coarsened_age_40"))) %>%
#   dplyr::select(term, label, estimate, std.error, upper, lower, p.value) %>%
#   mutate_if(is.numeric, list(~round(., 4))) %>%
#   write.csv("tables/effects_all_cutoffs.csv")

# Age plots w/ all cutoffs, country fe ----

# First, making necessary changes to the age_diff_models df

age_diff_models_fe_long <- age_diff_models_cfe %>%
  filter(country == "All") %>%
  filter(!(term %in% c("coarsened_age_30", "coarsened_age_35", "coarsened_age_40"))) %>%
  # Clean up coefficient estimate names
  mutate(age =
           case_when(grepl("old", ignore.case = T, term) ~
                       "Effect of old interviewer on young respondents",
                     grepl("young", ignore.case = T, term) ~
                       "Effect of young interviewer on old respondents",
                     TRUE ~ term)) %>%
  mutate(., label = factor(label, levels = levels(age_diff_models_wide$label)))

names <- c(coarsened_age_30 = "30 cutoff",
           coarsened_age_35 = "35 cutoff",
           coarsened_age_40 = "40 cutoff")

plots_integrated_country <- lapply(unique(age_diff_models_fe_long$group), function(x) {
  
  plot <- age_diff_models_fe_long %>%
    filter(country == "All") %>%
    filter(group == x) %>%
    filter(age_variable != "coarsened_age_10") %>%
    filter(age_variable != "coarsened_age_35_originalscale" ) %>%
    filter(term != "noncoeth") %>%
    plotfun_integrated()
  
  if(x == "pol_outcomes") {
    plot <- plot +
      geom_vline(xintercept = 2.5, size = 0.3)
    # geom_vline(xintercept = 4.5, size = 0.3)
  }
  
  return(plot) }) %>%
  "names<-"(unique(age_diff_models_fe_long$group))

plots_integrated_country$youth_outcomes <- plots_integrated_country$youth_outcomes +
  # Facet the question that was asked in all countries
  # "Addressing *needs* of youth"
  facet_grid(rows = vars(grepl("needs", outcome_variable)),
             labeller = labeller(.rows = function(x) {
               ifelse(x, "All\ncountries", "Mauritius only")
             }, .cols = names),
             cols = vars(age_variable),
             scales = "free" , space = "free" ) +
  theme(strip.text.y = element_text(size = 8, margin = margin(l = 3),
                                    vjust = 0, hjust = 0.5)) 

plots_integrated_country_align <- align_plots(plotlist = plots_integrated_country,
                                      align = "hv",
                                      axis = "tblr") %>%
  lapply(ggdraw)

map2(names(plots_integrated_country_align), c(5, 8, 5, 3.5, 5), function(x, y) {
  save_plot(paste0("figs/", x, "integrated_country.pdf"),
            plots_integrated_country_align[[x]],
            base_width = 10,
            base_height = y)
})

# Making a version of youth_outcomes without coethnicity - both country and adida fe ----

youth_plots_nocoeth <- list()

youth_plots_nocoeth$country <- age_diff_models_fe %>%
  filter(age != "noncoeth") %>%
  filter(group == "youth_outcomes") %>%
  plotfun_wide_fe() +
  coord_flip(ylim = c(-0.7, 0.7)) +
  scale_y_continuous(breaks = seq(-0.8, 0.8, 0.1)) +
  # Facet the question that was asked in all countries
  # "Addressing *needs* of youth"
  facet_grid(rows = vars(grepl("needs", outcome_variable)),
             labeller = labeller(.rows = function(x) {
               ifelse(x, "All\ncountries", "Mauritius only")
             }),
             scales = "free", space = "free") +
  theme(strip.text.y = element_text(size = 8, margin = margin(l = 3),
                                    vjust = 0, hjust = 0.5))

youth_plots_nocoeth$adida <- age_diff_models_wide %>%
  filter(age != "noncoeth") %>%
  filter(group == "youth_outcomes") %>%
  plotfun_wide_fe() +
  coord_flip(ylim = c(-0.7, 0.7)) +
  scale_y_continuous(breaks = seq(-0.8, 0.8, 0.1)) +
  # Facet the question that was asked in all countries
  # "Addressing *needs* of youth"
  facet_grid(rows = vars(grepl("needs", outcome_variable)),
             labeller = labeller(.rows = function(x) {
               ifelse(x, "All\ncountries", "Mauritius only")
             }),
             scales = "free", space = "free") +
  theme(strip.text.y = element_text(size = 8, margin = margin(l = 3),
                                    vjust = 0, hjust = 0.5))

map2(names(youth_plots_nocoeth), c(5, 5), function(x, y) {
  save_plot(paste0("figs/", x, "_youth_plots_nocoeth.pdf"),
            youth_plots_nocoeth[[x]],
            base_width = 7,
            base_height = y)
})

# 10 year age difference plots, country fe ----

age_diff_models_cfe_clean <- age_diff_models_cfe %>%
  filter(country == "All") %>%
  # filter(!(term %in% c("coarsened_age_30", "coarsened_age_35", "coarsened_age_40"))) %>%
  # Clean up coefficient estimate names
  mutate(term =
           case_when(grepl("Interviewer younger", term) ~
                       "Interviewer under 10 years younger than respondent\n(relative to interviewer's age within 10 years of respondent)",
                     grepl("Interviewer older", term) ~
                       "Interviewer over 10 years older then respondents\n(relative to interviewer's age within 10 years of respondent)",
                     grepl("30.*older_int", term) ~
                       "Interviewer over 30 (relative to both 30 and under)",
                     grepl("30.*younger_int", term) ~
                       "Interviewer 30 and under (relative to both over 30)",
                     grepl("35.*older_int", term) ~
                       "Interviewer over 35 (relative to both 35 and under)",
                     grepl("35.*younger_int", term) ~
                       "Interviewer 35 and under (relative to both over 35)",
                     grepl("40.*older_int", term) ~
                       "Interviewer over 40 (relative to both 40 and under)",
                     grepl("40.*younger_int", term) ~
                       "Interviewer 40 and under (relative to both over 40)",
                     grepl("noncoeth", term) ~
                       "Non-coethnic interviewer",
                     TRUE ~ term))

plot10yr_cfe <- lapply(unique(age_diff_models_cfe_clean$group), function(x) {
  plot <- age_diff_models_cfe_clean %>%
    filter(age_variable == "coarsened_age_10") %>%
    filter(group == x) %>%
    plotfun()
  
  if(x == "pol_outcomes") {
    plot <- plot +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1),
                         limits = c(-0.4, 0.4)) +
      geom_vline(xintercept = 2.5, size = 0.3) 
    # +
    #   geom_vline(xintercept = 4.5, size = 0.3)
  } else if (x == "stat_outcomes") {
    plot <- plot +
      geom_vline(xintercept = 1.5, size = 0.3)
  } else if (x == "pro_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.2, 0.2)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.05))
  } else if (x == "youth_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.4, 0.4)) +
      scale_y_continuous(breaks = seq(-0.4, 0.4, 0.1))
  }
  return(plot)
}) %>%
  "names<-"(unique(age_diff_models$group))

plot10yr_cfe$youth_outcomes <- plot10yr_cfe$youth_outcomes +
  # Facet the question that was asked in all countries
  # "Addressing *needs* of youth"
  facet_grid(rows = vars(grepl("needs", outcome_variable)),
             labeller = labeller(.rows = function(x) {
               ifelse(x, "All\ncountries", "Mauritius only")
             }),
             scales = "free", space = "free") +
  theme(strip.text.y = element_text(size = 8, margin = margin(l = 3),
                                    vjust = 0, hjust = 0.5))

### Align x-axes of plots
## This constrains the plot rectangles to all be
## the same size and makes it easier to compare
## effect sizes. Without this, varying variable
## label length makes all the plots different sizes

plot10yr_cfe_align <- align_plots(plotlist = plot10yr_cfe,
                              align = "hv",
                              axis = "tblr") %>%
  lapply(ggdraw)



map2(names(plot10yr_cfe_align), c(5, 8, 5, 3.5, 5), function(x, y) {
  save_plot(paste0("figs/", x, "10yr_countryfe.pdf"),
            plot10yr_cfe_align[[x]],
            base_width = 7,
            base_height = y) })


# Alternative plots not used in the final paper or appendix ----
# (35-cutoff plots with visual effects for
# sign flips and effects that are significant across age cutoffs.)
# 
# age_diff_models_wide <- age_diff_models_int %>%
#   mutate(age =
#            case_when(grepl("old", ignore.case = T, term) ~
#                        "old",
#                      grepl("young", ignore.case = T, term) ~
#                        "young",
#                      TRUE ~ term)) %>%
#   mutate(age_label =
#            case_when(grepl("old", ignore.case = T, age) ~
#                        "Effect of old interviewer on young respondents",
#                      grepl("young", ignore.case = T, age) ~
#                        "Effect of young interviewer on old respondents",
#                      grepl("noncoeth", ignore.case = T, age) ~
#                        "Non-coethnic interviewer",
#                      TRUE ~ age)) %>%
#   rename(p_value = p.value) %>%
#   select(-term, -std.error, -statistic, -n_obs, -country) %>%
#   filter(age_variable != "coarsened_age_10") %>%
#   filter(age_variable != "coarsened_age_35_originalscale") %>%
#   pivot_wider(names_from = c(age_variable),
#               values_from = c(estimate, p_value, upper, lower),
#               names_sep = ".") %>%
#   mutate(., sig_across = ifelse(age != "noncoeth",
#                                 ifelse(p_value.coarsened_age_30 < 0.05 &
#                                    p_value.coarsened_age_35 < 0.05 &
#                                    p_value.coarsened_age_40 < 0.05, 1, 0), 0)) %>%
#   mutate(., sign_flip = ifelse(age != "noncoeth",
#                                ifelse(p_value.coarsened_age_35 < 0.05,
#                                ifelse(p_value.coarsened_age_40 < 0.05 &
#                                         lower.coarsened_age_35*lower.coarsened_age_40 < 0 , 1,
#                                       ifelse(p_value.coarsened_age_30 < 0.05 &
#                                                lower.coarsened_age_35*lower.coarsened_age_30 < 0 , 1, 0)), 0), 0))
# 
# plotfun_wide <- function(data) {
#   data %>%
#     ggplot(aes(label,
#              estimate.coarsened_age_35,
#              colour = age_label,
#              linetype = factor(sign_flip),
#              shape = age_label,
#              size = factor(sig_across)
#              )) +
#   theme_linedraw() +
#   geom_point(position = position_dodge(width = 0.5),
#              size = 2) +
#   geom_errorbar(aes(ymin = lower.coarsened_age_35,
#                     ymax = upper.coarsened_age_35),
#                 width = 0.4,
#                 position = position_dodge(width = 0.5)) +
#   scale_colour_manual(values = c("gray50", "black", "gray80")) +
#   scale_shape_manual(values = c(17,15,19)) +
#   scale_size_manual(values = c(0.5,1.5)) +
#   coord_flip() +
#   scale_y_continuous(breaks = seq(-0.2, 0.2, 0.05),
#                                   limits = c(-0.2, 0.2)) +
#   theme(axis.title.y = element_blank(),
#         legend.title = element_blank(),
#         legend.position = "bottom",
#         legend.text = element_text(size = 7),
#         legend.key.width=unit(3,"line"),
#         strip.background = element_blank(),
#         strip.text = element_blank(),
#         text = element_text(family = "Roboto"),
#         panel.grid.major = element_line(color = 'gray80'),
#         panel.grid.minor = element_line(color = 'gray80')) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   guides(colour = guide_legend(reverse = TRUE,
#                                ncol = 1),
#          linetype = "none",
#          shape = guide_legend(reverse = TRUE,
#                               ncol = 1),
#          size = "none"
#          ) +
#   ylab("\nEstimated effect (in SDs) of non-coethnic interviewer \nand age difference, with 95% confidence intervals") }
# 
# plot35yr_paper <- lapply(unique(age_diff_models_wide$group), function(x) {
#   plot <- age_diff_models_wide %>%
#     filter(group == x) %>%
#     plotfun_wide()
# 
#   if(x == "pol_outcomes") {
#     plot <- plot +
#       geom_vline(xintercept = 2.5, size = 0.3)
#     # geom_vline(xintercept = 4.5, size = 0.3)
#   } else if (x == "stat_outcomes") {
#     plot <- plot +
#       geom_vline(xintercept = 1.5, size = 0.3)
#   } else if (x == "pro_outcomes") {
#     plot <- plot +
#       coord_flip(ylim = c(-0.2, 0.2)) +
#       scale_y_continuous(breaks = seq(-0.4, 0.4, 0.05))
#   } else if (x == "youth_outcomes") {
#     plot <- plot +
#       coord_flip(ylim = c(-0.7, 0.7)) +
#       scale_y_continuous(breaks = seq(-0.8, 0.8, 0.1))
#   }
#   else if (x == "eth_outcomes") {
#     plot <- plot +
#       coord_flip(ylim = c(-0.2, 0.2)) +
#       scale_y_continuous(breaks = seq(-0.4, 0.4, 0.05))
#   }
#   return(plot)
# }) %>%
#   "names<-"(unique(age_diff_models$group))
# 
# plot35yr_paper$eth_outcomes
# 
# plot35yr_paper$youth_outcomes <- plot35yr_paper$youth_outcomes +
#   # Facet the question that was asked in all countries
#   # "Addressing *needs* of youth"
#   facet_grid(rows = vars(grepl("needs", outcome_variable)),
#              labeller = labeller(.rows = function(x) {
#                ifelse(x, "All\ncountries", "Mauritius only")
#              }, .cols = names),
#              scales = "free" , space = "free" ) +
#   theme(strip.text.y = element_text(size = 8, margin = margin(l = 3),
#                                     vjust = 0, hjust = 0.5))
# 
# plot35yr_paper_align <- align_plots(plotlist = plot35yr_paper,
#                                       align = "hv",
#                                       axis = "tblr") %>%
#   lapply(ggdraw)
# 
# map2(names(plot35yr_paper_align), c(5, 8, 5, 5, 5), function(x, y) {
#   save_plot(paste0("figs/", x, "paper.pdf"),
#             plot35yr_paper_align[[x]],
#             base_width = 7,
#             base_height = y)
# })
