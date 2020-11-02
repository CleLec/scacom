# Project Info: ----------------------------------------------------------------
# Lechner et al. (2020). Stability and Change in Literacy and Numeracy
# Step 4: Reporting the results.
# This code was written by clemens.lechner@gesis.org
# R 4.0.3

# Basic settings ---------------------------------------------------------------
rm(list = ls())

# List of subdirectories
dirs <- list(
  main = "D:/Dropbox/Forschung und Lehre/Stability_neps_NEPS",
  results = "./02_results"
)

# Load raw data and results
map(c(
  "math_neps.Rda", "reading_neps.Rda",
  "math_piaac.Rda", "reading_piaac.Rda",
  "./02_results/cors.Rda",
  "./02_results/deltas.Rda"
), load, .GlobalEnv)

# Load required packages
library(tidyverse)
library(glue)
library(sjlabelled)
library(mice)
library(miceadds)
library(mitools)
library(srvyr)

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
      "All" =  "Total\nsample",
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
    scale_colour_manual(values = c("#58748F", "#F06400")) +
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

deltas_plot


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
    scale_colour_manual(values = c("#58748F", "#F06400")) +
    scale_y_continuous(
      breaks = seq(from = 0.3, to = 1, by = 0.1),
      labels = function(x) sprintf("%.2f", x)
    )
}

pdf("test2.pdf", family = "Source Sans Pro",
    width = 20,
    height = 10)

cors_plot <- cowplot::plot_grid(
  cors_plotter("piaac"),
  cors_plotter("neps"),
  labels = c("Three years (PIAAC-L)", "Six years (NEPS)"),
  align = "h",
  hjust = -0.1,
  label_size = 18
)

cors_plot

ggsave("test2.pdf",  plot = cors_plot)

dev.off()

# Plot density distribution of change ---------------------------------------------

delta_density <- function(data) {

  # Compute sd at T1 (for computation of Cohen's dav and for coloring the plot)
 require(radiant.data)
  
  sds <- data %>%
    group_by(.imp) %>%
    summarise(
      sd_pooled = 0.5 * (weighted.sd(t1_pv, weight) + 
                           weighted.sd(t2_pv, weight)),
      sd_delta = weighted.sd(t2_pv - t1_pv, weight)
    ) %>%
    summarise(sd_pooled = mean(sd_pooled), sd_delta = mean(sd_delta))

  # Compute deltas (t2-t1) based on the EAP scores
  plotdata <- data %>%
    # group_by(seqid) %>%
    mutate(delta = t1_pv - t2_pv) %>%
    # filter(.imp == 1) %>%
    ungroup()

  # Compute densities for each delta value
  plotdata <- with(density(plotdata$delta), tibble(x, y)) %>%
    mutate(
      direction = ifelse(x <= -0.8 * sds[["sd_pooled"]], "neg",
        ifelse(x >= 0.8 * sds[["sd_pooled"]], "pos", "little")
      )
    )

  plotdata %>% ggplot(aes(x = x, y = y)) +
    geom_area(aes(fill = direction),
              alpha = 1, 
              show.legend = F) +
    geom_vline(
      xintercept = 0,
      size = 0.5,
      lty = "dashed",
      color = "#666666"
    ) +
    geom_line(size = 1.1, color = "#666666") +
    theme_apa() +
    xlab(expression(paste("Change score (", Delta["T2, T1"], ")"))) +
    ylab("Density") +
    theme(
      text = element_text(family = "Source Sans Pro"),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 15)
    ) +
    scale_fill_manual(values = c("#BDCDD8", "#58748F", "#58748F")) +
    expand_limits(x = c(-2 * sds$sd_pooled, 2 * sds$sd_pooled))
}


delta_density(math_neps)


#  Plot difference scores over age based on the EAP scores --------
neps_d_age <- reading_neps %>%
  filter(.imp == 1) %>%
  ggplot(aes(x = age, y = (eap_w9 - eap_w3) / sd(eap_w3))) +
  geom_smooth(
    span = 0.1, level = 0.95, fill = "#58748F",
    color = "#58748F"
  ) +
  # stat_smooth(level = 0.99, span = 0.01) +
  geom_hline(aes(yintercept = 0), color = "#666666") +
  expand_limits(y = c(-0.3, 0.3), x = c(25, 67)) +
  xlab(expression(paste("Alter zu ", T[1], " in Jahren"))) +
  ylab(expression(paste(
    "Veränderung von ", T[1], " zu ", T[2],
    " in ", italic("SD")
  ))) +
  theme(
    text = element_text(family = "Times"),
    panel.background = element_rect(fill = "white", color = "black"),
    axis.title.x = element_text(size = 30, margin = margin(t = 20)),
    axis.title.y = element_text(size = 30, margin = margin(r = 20)),
    axis.text = element_text(size = 20, ),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_line(colour = "grey90", size = 0.1)
  )


ggsave(neps_d_age,
  filename = "neps_d_age.png", # device = "wmf",
  path = "./02_results/"
)

piaac_d_age <- reading_piaac %>%
  group_by(seqid) %>%
  mutate(
    t1_eap = mean(t1_pv),
    t2_eap = mean(t2_pv)
  ) %>%
  ungroup() %>%
  filter(.imp == 1) %>%
  ggplot(aes(x = age, y = (t2_eap - t1_eap) / sd(t1_eap))) +
  stat_smooth(
    fill = "#58748F", span = 3,
    color = "#58748F", fullrange = F
  ) +
  # stat_smooth(level = 0.99, span = 0.01) +
  geom_hline(aes(yintercept = 0, color = "#666666")) +
  expand_limits(y = c(-0.3, 0.3), x = c(18, 65)) +
  xlab(expression(paste("Alter zu ", T[1], " in Jahren"))) +
  ylab(expression(paste(
    "Veränderung von ", T[1], " zu ", T[2],
    "in ", italic("SD")
  ))) +
  theme(
    text = element_text(family = "Times"),
    panel.background = element_rect(fill = "white", color = "black"),
    axis.title.x = element_text(size = 30, margin = margin(t = 20)),
    axis.title.y = element_text(size = 30, margin = margin(r = 20)),
    axis.text = element_text(size = 20, ),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_line(colour = "grey90", size = 0.1)
  )


ggsave(piaac_d_age,
  filename = "piaac_d_age.png", # device = "wmf",
  path = "./02_results/"
)


