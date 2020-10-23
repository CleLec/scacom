
# Plot mean-level change (Cohen's dz) -------------------------------------

deltas_plot <- function(study) {
  
  data <- deltas %>%
    filter(str_detect(.data$target, {{study}}))
    
  head(data$target)
    
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
          "45-54 years",
          "55+ years"
        )
      ),
  heading = fct_recode(.data$Group, 
                       "All" =  "Total\nsample",
                       "Gender" = "Male" ,
                       "Gender" = "Female",
                       "Education" = "low\n(ISCED 0–3)",
                       "Education" = "intermediate\n(ISCED 4/5B)",
                       "Education" = "high\n(ISCED 5A/6)",
                       "Age Group" = "18–34 years",
                       "Age Group" = "35–44 years",
                       "Age Group" = "45-54 years",
                       "Age Group" = "55+ years"
  )
  )
  

dodge1 <- position_dodge(width = 0.9)

data %>%
  ggplot(aes(
    x = fct_relevel(Group, rev), y = dz, group = rev(Domain),
    color = Domain, shape = Domain
  )) +
  geom_pointrange(aes(ymin = dz_lower, ymax = dz_upper),
    position = dodge1, size = 0.6
  ) +
  geom_text(aes(label = sprintf("%.2f", dz)),
    position = dodge1,
    show.legend = F,
    vjust = -0.75,
    size = 4
  ) +
  geom_hline(yintercept = 0, lty = "dashed") +
  coord_flip(ylim = c(-0.41, 0.41)) +
  facet_grid(
    rows = vars(heading), # strip.position = "bottom",
    switch = "y",
    cols = NULL,
    scales = "free_y",
    space = "free_y",
    margins = margin(0.5, 5, 0.5, 0.5, unit = "cm")
  ) +
  labs(
  # title = "Mean-level changes across 3 years (PIAAC)",
    y = expression(atop(
      "Mean-level change",
      paste("(Cohen's ", italic(d[z]), ")")
    ))
  ) +
 jtools::theme_apa() +
  theme(
    legend.text = element_text(size = 12),
    #legend.box.background = element_rect(colour = "black"),
    #legend.background = element_blank(),
    #legend.box.spacing = margin(0.2, 0.3, 
    #                            unit = "cm"),
    legend.position = "top",
    #legend.position = c(0.85, 0.05),
    plot.margin = margin(0.75, 0.75, 0.75, 0.75, 
                         unit = "cm"),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.y = element_text(size = 15),
    panel.spacing = unit(1, "lines"),
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
    axis.title.x = element_text(vjust = -1, 
                                size = 15, 
                                face = "bold"),
  ) +
  scale_colour_manual(values = c("dodgerblue4", "red3")) +
  scale_y_continuous(
    breaks = seq(from = -0.4, to = 0.4, by = 0.1),
    labels = function(x) sprintf("%.2f", x)
  )
}

deltas_plot("piaac")

outcome_plot <- cowplot::plot_grid(
  deltas_plot("neps"),
  deltas_plot("piaac"),
  labels = c("A. Six-year period (NEPS)", "B. Three-year period (PIAAC-L)"),
  align = "h",
  #vjust = 1,
  label_size = 18)

outcome_plot 


# Plot mean-level change (Cohen's dz) -------------------------------------

cors_plot <- function(study) {
  
  data <- cors %>%
    filter(str_detect(.data$target, {{study}}))
  
  head(data$target)
  
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
                            "45-54 years",
                            "55+ years"
                          )
                   ),
                 heading = fct_recode(.data$Group, 
                                      "All" =  "Total\nsample",
                                      "Gender" = "Male" ,
                                      "Gender" = "Female",
                                      "Education" = "low\n(ISCED 0–3)",
                                      "Education" = "intermediate\n(ISCED 4/5B)",
                                      "Education" = "high\n(ISCED 5A/6)",
                                      "Age Group" = "18–34 years",
                                      "Age Group" = "35–44 years",
                                      "Age Group" = "45-54 years",
                                      "Age Group" = "55+ years"
                 )
  )
  
  dodge1 <- position_dodge(width = 0.9)
  
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
    geom_hline(yintercept = 0, lty = "dashed") +
    coord_flip(ylim = c(0.35, 1)) +
    facet_grid(
      rows = vars(heading), # strip.position = "bottom",
      switch = "y",
      cols = NULL,
      scales = "free_y",
      space = "free_y",
      margins = margin(0.5, 5, 0.5, 0.5, unit = "cm")
    ) +
    labs(
      # title = "Mean-level changes across 3 years (PIAAC)",
      y = expression(atop(
        "Rank-order stability",
        paste("(Pearson's ", italic(r[T1, T2]), ")")
      ))
    ) +
    jtools::theme_apa() +
    theme(
      legend.text = element_text(size = 12),
      #legend.box.background = element_rect(colour = "black"),
      #legend.background = element_blank(),
      #legend.box.spacing = margin(0.2, 0.3, 
      #                            unit = "cm"),
      legend.position = "top",
      #legend.position = c(0.85, 0.05),
      plot.margin = margin(0.75, 0.75, 0.75, 0.75, 
                           unit = "cm"),
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text.y = element_text(size = 15),
      panel.spacing = unit(1, "lines"),
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
      axis.title.x = element_text(vjust = -1, 
                                  size = 15, 
                                  face = "bold"),
    ) +
    scale_colour_manual(values = c("dodgerblue4", "red3")) +
    scale_y_continuous(
      breaks = seq(from = 0.3, to = 1, by = 0.1),
      labels = function(x) sprintf("%.2f", x)
    )
}

cors_plot("piaac")

outcome_plot <- cowplot::plot_grid(
  cors_plot("neps"),
  cors_plot("piaac"),
  labels = c("A. Six-year period (NEPS)", "B. Three-year period (PIAAC-L)"),
  align = "h",
  #vjust = 1,
  label_size = 18)

outcome_plot 


