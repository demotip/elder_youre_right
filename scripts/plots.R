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

# Do the same for the integrated plots ----

age_diff_models_int <- age_diff_models_og %>%
  filter(country == "All") %>%
  filter(!(term %in% c("coarsened_age_30", "coarsened_age_35", "coarsened_age_40"))) %>%
  # Clean up coefficient estimate names
  mutate(age =
           case_when(grepl("old", ignore.case = T, term) ~
                       "Older interviewer for younger respondents",
                     grepl("young", ignore.case = T, term) ~
                       "Younger interviewer for older respondents",
                     TRUE ~ term))

names <- c(coarsened_age_30 = "30 cutoff",
           coarsened_age_35 = "35 cutoff",
           coarsened_age_40 = "40 cutoff")

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

# PLOT FUNCTION FOR INTEGRATED APPENDIX PLOTS ----

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

# Integrated age plots ----

plots_integrated <- lapply(unique(age_diff_models_int$group), function(x) {
  
  plot <- age_diff_models_int %>%
    filter(country == "All") %>%
    filter(group == x) %>%
    filter(age_variable != "coarsened_age_10") %>%
    filter(age_variable != "coarsened_age_35_originalscale" ) %>%
    filter(term != "noncoeth") %>%
    plotfun_integrated()
  
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

age_diff_models %>%
  filter(!(term %in% c("coarsened_age_30", "coarsened_age_35", "coarsened_age_40"))) %>%
  dplyr::select(term, label, estimate, std.error, upper, lower, p.value) %>%
  mutate_if(is.numeric, list(~round(., 4))) %>%
  write.csv("tables/effects_all_cutoffs.csv")


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
  # else if (x == "eth_outcomes") {
  # plot <- plot +
  #   # geom_point(data = subset(age_diff_models, outcome_variable == "z_patronage"),
  #   #            size = 4)
  #   geom_errorbar(data = subset(age_diff_models, outcome_variable == "z_patronage"),
  #                 aes(ymin = lower, ymax = upper),
  #                 width = 0.5) # Larger error bars for patronage
  #   # geom_point(data = subset(age_diff_models, outcome_variable == "z_patronage"),
  #   #            size = 4)
  # }
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