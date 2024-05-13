library(groundhog)

plot_30_yr <- c("cowplot", "tidyverse", "showtext", "sysfonts")
groundhog.library(plot_30_yr, "2021-11-01")

font_add_google("Roboto", "Roboto")
showtext_auto()

source("scripts/variable_labels.R")
age_diff_models <- readRDS("data_clean/age_diff_models.rds")

# Clean up coefficient estimates df ----

age_diff_models <- age_diff_models %>%
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

plotfun_integrated <- function(data) {
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
    facet_wrap(~age_variable) +
    scale_colour_manual(values = c("darkorchid4", "darkred", "darkblue",
                                   "darkorchid2", "red", "blue",
                                   "gray80")) +
    scale_shape_manual(values = c(17,15,19,21,23,25,27)) +
    scale_linetype_manual(values = c(rep("solid", 7))) +
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
          text = element_text(family = "Roboto")) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    guides(colour = guide_legend(reverse = TRUE,
                                 ncol = 2),
           linetype = guide_legend(reverse = TRUE,
                                   ncol = 1),
           shape = guide_legend(reverse = TRUE,
                                ncol = 1)) +
    ylab("\nEstimated effect (in SDs) of non-coethnic interviewer \nand age difference, with 95% confidence intervals")
}

plots_integrated <- lapply(unique(age_diff_models$group), function(x) {
  
  plot <- age_diff_models %>%
    filter(country == "All") %>%
    filter(group == x) %>%
    filter(age_variable != "coarsened_age_10") %>%
    filter(age_variable != "coarsened_age_35_originalscale" ) %>%
    plotfun_integrated()
  
  if(x == "pol_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.8, 0.8)) +
      scale_y_continuous(breaks = seq(-0.8, 0.8, 0.2)) + 
      geom_vline(xintercept = 4.5, size = 0.3)
  } else if (x == "stat_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.8, 0.8)) +
      scale_y_continuous(breaks = seq(-0.8, 0.8, 0.2)) + 
      geom_vline(xintercept = 1.5, size = 0.3)
  } else if (x == "pro_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.8, 0.8)) +
      scale_y_continuous(breaks = seq(-0.8, 0.8, 0.2))
  } else if (x == "youth_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.8, 0.8)) +
      scale_y_continuous(breaks = seq(-0.8, 0.8, 0.2))
  } else if (x == "eth_outcomes") {
    plot <- plot +
      coord_flip(ylim = c(-0.8, 0.8)) +
      scale_y_continuous(breaks = seq(-0.8, 0.8, 0.2))
  }
  return(plot) }) %>%
  "names<-"(unique(age_diff_models$group))

plots_integrated$youth_outcomes <- plots_integrated$youth_outcomes +
  # Facet the question that was asked in all countries
  # "Addressing *needs* of youth"
  facet_grid(rows = vars(grepl("needs", outcome_variable)),
             labeller = labeller(.rows = function(x) {
               ifelse(x, "All\ncountries", "Mauritius only")
             }),
             scales = "free", space = "free") +
  theme(strip.text.y = element_text(size = 8, margin = margin(l = 3),
                                    vjust = 0, hjust = 0.5))

plots_integrated_align <- align_plots(plotlist = plots_integrated,
                              align = "hv",
                              axis = "tblr") %>%
  lapply(ggdraw)

plots_integrated_align

map2(names(plots_integrated_align), c(5, 8, 5, 3.5, 5), function(x, y) {
  save_plot(paste0("figs/", x, "integrated.pdf"),
            plots_integrated_align[[x]],
            base_width = 7,
            base_height = y)
})