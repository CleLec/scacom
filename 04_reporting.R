# Project Info: ----------------------------------------------------------------
# Lechner et al. (2020). Stability and Change in Literacy and Numeracy
# Step 4: Reporting the results.
# This code was written by clemens.lechner@gesis.org
# R 4.0.3

# Basic settings ---------------------------------------------------------------
# rm(list = ls())

# List of subdirectories
dirs <- list(
  main = "D:/Dropbox/Forschung und Lehre/Stability_PIAAC_NEPS",
  data = "C:/users/lechnecs/Desktop/NEPS SC6 (11-0-0)/SPSS/en/",
  results = "./02_results"
)

# Load required packages
library(tidyverse)
library(glue)
library(sjlabelled)
library(mice)
library(miceadds)
library(mitools)
library(srvyr)
library(extrafont)

# Load raw data and results
map(c(
  "math_neps.Rda", "reading_neps.Rda",
  "math_piaac.Rda", "reading_piaac.Rda",
  "./02_results/cors.Rda",
  "./02_results/deltas.Rda"
), load, .GlobalEnv)



# Rename data -------------------------------------------------------------

beautify_names <- function(data) {
  data <- mutate(data,
    Domain = str_detect(target, "math") %>%
      factor(
        levels = c(FALSE, TRUE),
        labels =
          c("Literacy", "Numeracy")
      ),
    Group =
      str_c(grouping, group, sep = "=") %>%
        factor(.,
          levels =
            c(
              "total=1",
              "gender=1",
              "gender=2",
              "edugr=0",
              "edugr=1",
              "edugr=2",
              "agegr=0",
              "agegr=1",
              "agegr=2",
              "agegr=3"
            ),
          labels = c(
            "Total\nsample",
            "Male",
            "Female",
            "low\n(ISCED 0–3)",
            "intermediate\n(ISCED 4/5B)",
            "high\n(ISCED 5A/6)",
            "18–34 years",
            "35–44 years",
            "45–54 years",
            "55+ years"
          )
        ),
    heading = fct_recode(.data$Group,
      "All" =  "Total\npopulation",
      "Gender" = "Male",
      "Gender" = "Female",
      "Education" = "low\n(ISCED 0–3)",
      "Education" = "intermediate\n(ISCED 4/5B)",
      "Education" = "high\n(ISCED 5A/6)",
      "Age Group" = "18–34 years",
      "Age Group" = "35–44 years",
      "Age Group" = "45–54 years",
      "Age Group" = "55+ years"
    )
  )
  data
}

cors <- beautify_names(cors)
deltas <- beautify_names(deltas)


# Plot mean-level change (Cohen's dav) -------------------------------------

dodge1 <- position_dodge(width = 0.9)

deltas_plotter <- function(study) {
  data <- deltas %>%
    filter(str_detect(.data$target, {{ study }}))

  data %>%
    ggplot(aes(
      x = fct_relevel(Group, rev), y = dav, group = rev(Domain),
      color = Domain, shape = Domain
    )) +
    geom_pointrange(aes(ymin = dav_lower, ymax = dav_upper),
      position = dodge1, size = 0.6
    ) +
    geom_text(aes(label = sprintf("%.2f", dav)),
      position = dodge1,
      show.legend = F,
      vjust = -0.75,
      size = 4
    ) +
    geom_hline(yintercept = 0, lty = "dashed", color = "#666666") +
    coord_flip(ylim = c(-0.35, 0.35)) +
    facet_grid(
      rows = vars(heading), # strip.position = "bottom",
      switch = "y",
      # cols = NULL,
      scales = "free_y",
      space = "free_y",
      # margins = margin(1, 1, 1, 1, unit = "cm")
    ) +
    labs(
      # title = "Mean-level changes across 3 years (PIAAC)",
      y = expression(atop(
        "Mean-level change",
        paste("(Cohen's ", italic(d[av]), ")")
      ))
    ) +
    jtools::theme_apa() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.text = element_text(size = 15, face = "bold"),
      legend.position = "top",
      plot.margin = margin(1.25, 0.5, 0.5, 0.5,
        unit = "cm"
      ),
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text.y = element_text(size = 15),
      panel.spacing = unit(0.33, "lines"),
      # panel.grid.major.y = element_blank(),
      axis.text.x = element_text(
        angle = 90, hjust = 0.5,
        vjust = 0.5,
        size = 12
      ),
      axis.text.y = element_text(
        hjust = 0.5,
        vjust = 0.5,
        size = 12
      ),
      axis.title.y = element_blank(),
      axis.title.x = element_text(
        vjust = 0,
        size = 15,
        face = "bold"
      ),
    ) +
    scale_colour_manual( # values = c("#58748F", "#F06400")) +
      values = c("#005B96", "#CC0001")
    ) +
    scale_y_continuous(
      breaks = seq(from = -0.4, to = 0.4, by = 0.1),
      labels = function(x) sprintf("%.2f", x)
    )
}


deltas_plot <- cowplot::plot_grid(
  deltas_plotter("piaac"),
  deltas_plotter("neps"),
  labels = c("Three years (PIAAC-L)", "Six years (NEPS)"),
  align = "h",
  hjust = -0.1,
  label_size = 18

)



# Plot correlations  -------------------------------------

cors_plotter <- function(study) {
  data <- cors %>%
    filter(str_detect(.data$target, {{ study }}))

  head(data$target)

  data %>%
    ggplot(aes(
      x = fct_relevel(Group, rev), y = rho, group = rev(Domain),
      color = Domain, shape = Domain
    )) +
    geom_pointrange(aes(ymin = lower, ymax = upper),
      position = dodge1, size = 0.6
    ) +
    geom_text(aes(label = sprintf("%.2f", rho)),
      position = dodge1,
      show.legend = F,
      vjust = -0.75,
      size = 4
    ) +
    geom_hline(yintercept = 0.7, lty = "dashed", color = "#666666") +
    coord_flip(ylim = c(0.39, 1)) +
    facet_grid(
      rows = vars(heading), # strip.position = "bottom",
      switch = "y",
      cols = NULL,
      scales = "free_y",
      space = "free",
      # margins = margin(0.25, 25, 0.25, 0.25, unit = "cm")
    ) +
    labs(
      # title = "Mean-level changes across 3 years (PIAAC)",
      y = expression(atop(
        "Rank-order stability",
        paste("(Pearson's ", italic(r), ")")
      ))
    ) +
    jtools::theme_apa() +
    theme(
      text = element_text(family = "Source Sans Pro"),
      legend.text = element_text(size = 15, face = "bold"),
      legend.position = "top",
      plot.margin = margin(1.25, 0.5, 0.5, 0.5,
        unit = "cm"
      ),
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text.y = element_text(size = 15),
      panel.spacing = unit(0.33, "lines"),
      # panel.grid.major.y = element_blank(),
      axis.text.x = element_text(
        angle = 90, hjust = 0.5,
        vjust = 0.5,
        size = 12
      ),
      axis.text.y = element_text(
        hjust = 0.5,
        vjust = 0.5,
        size = 12
      ),
      axis.title.y = element_blank(),
      axis.title.x = element_text(
        vjust = 0,
        size = 15,
        face = "bold"
      ),
    ) +
    scale_colour_manual( # values = c("#58748F", "#F06400")) +
      values = c("#005B96", "#CC0001")
    ) +
    scale_y_continuous(
      breaks = seq(from = 0.3, to = 1, by = 0.1),
      labels = function(x) sprintf("%.2f", x)
    )
}

cors_plot <- cowplot::plot_grid(
  cors_plotter("piaac"),
  cors_plotter("neps"),
  labels = c("Three years (PIAAC-L)", "Six years (NEPS)"),
  align = "h",
  hjust = -0.1,
  label_size = 18
)

#cors_plot

#pdf("cors.pdf",
#    family = "Source Sans Pro",
#    width = 20,
#    height = 10
#)

#ggsave("cors.pdf", plot = deltas_plot)

#dev.off()

# Plot density distribution of change ---------------------------------------------
# Moved to appendix.Rmd


