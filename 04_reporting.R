# Project Info: ----------------------------------------------------------------
# Lechner et al. (2020). Stability and Change in Literacy and Numeracy
# Step 4: Reporting the results.
# This code was written by clemens.lechner@gesis.org
# R 4.0.3

# Basic settings ---------------------------------------------------------------
rm(list = ls())

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
library(papaja)


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
            "Total\npopulation",
            "Male",
            "Female",
            "low\n(ISCED 0-3)",
            "intermediate\n(ISCED 4/5B)",
            "high\n(ISCED 5A/6)",
            "18 -- 34 years",
            "35 -- 44 years",
            "45 -- 54 years",
            "55+ years"
          )
        ),
    heading = fct_recode(.data$Group,
      "All" =  "Total\npopulation",
      "Gender" = "Male",
      "Gender" = "Female",
      "Education" = "low\n(ISCED 0-3)",
      "Education" = "intermediate\n(ISCED 4/5B)",
      "Education" = "high\n(ISCED 5A/6)",
      "Age Group" = "18 -- 34 years",
      "Age Group" = "35 -- 44 years",
      "Age Group" = "45 -- 54 years",
      "Age Group" = "55+ years"
    )
  )
  data
}

cors <- beautify_names(cors)
deltas <- beautify_names(deltas)

# Define generic plot theme

plot_theme <-
  theme(
    text = element_text(family = "Source Sans Pro"),
    legend.text = element_text(
      size = 8,
      face = "bold"
    ),
    legend.background = element_rect(linetype = "solid"),
    legend.position =  "top",
   # plot.margin = margin(1, 1, 5, 1),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.y = element_text(
      size = 8
    ),
     panel.spacing = unit(0.3, "lines"),
     axis.text.x = element_text(
      #angle = 90,
      hjust = 0.5,
      vjust = 0.5,
      size = 8
    ),
    axis.text.y = element_text(
      hjust = 0.5,
      vjust = 0.5,
      size = 8,
     # angle = 90,
    ),
    axis.title.y = element_blank(),
    axis.title.x = element_text(
      vjust = 0,
      size = 10,
      face = "bold"
    )
  )

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
      position = dodge1, size = 0.25
    ) +
    geom_text(aes(label = sprintf("%.2f", dav)),
      position = dodge1,
      show.legend = F,
      vjust = -0.75,
      size = 2
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
    plot_theme +
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
  nrow = 1,
  labels = c("Three years (PIAAC-L)", "Six years (NEPS)"),
  align = "h",
  hjust = -0.1,
  label_size = 12,
  label_fontfamily = "Source Sans Pro"
)


ggsave("deltas.pdf",
       device = "pdf", plot = deltas_plot,
       width = 22, height = 15, unit = "cm"
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
      position = dodge1, 
      size = 0.25
    ) +
    geom_text(aes(label = sprintf("%.2f", rho)),
      position = dodge1,
      show.legend = F,
      vjust = -0.75,
      size = 2
    ) +
    geom_hline(
      yintercept = 0.7,
      lty = "dashed",
      color = "#666666"
    ) +
    coord_flip(ylim = c(0.4, 1)) +
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
        "Rank-order consistency",
        paste("(Pearson's ", italic(r), ")")
      ))
    ) +
    jtools::theme_apa() +
    plot_theme +
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
  nrow = 1,
  labels = c("Three years (PIAAC-L)", "Six years (NEPS)"),
  align = "h",
  hjust = -0.1,
  label_fontfamily = "Source Sans Pro",
  label_size = 12
)

cors_plot

# pdf("cors.pdf",
#    family = "Source Sans Pro",
#    width = 20,
#    height = 10
# )

ggsave("cors.pdf",
  device = "pdf", plot = cors_plot,
  width = 22, height = 15, unit = "cm"
)

# dev.off()

# Plot density distribution of change ---------------------------------------------
# Moved to appendix.Rmd

# Function to get descriptive statistics on socio-demographic characteristics

describe_data <- function(study) {
  data <- get(study) %>%
    filter(.imp == 1) %>%
    mutate(
      edu0 = if_else(edugr == 0, 1, 0),
      edu1 = if_else(edugr == 1, 1, 0),
      edu2 = if_else(edugr == 2, 1, 0),
      age0 = if_else(agegr == 0, 1, 0),
      age1 = if_else(agegr == 1, 1, 0),
      age2 = if_else(agegr == 2, 1, 0),
      age3 = if_else(agegr == 3, 1, 0),
      female = if_else(gender == 2, 1, 0),
      native = if_else(language == 1, 1, 0),
      # t1_pv = round(t1_pv, 2),
      # t2_pv = round(t2_pv, 2),
      across(where(is.double), ~ as.numeric(.x))
    ) %>%
    select(
      age, age0, age1, age2, age3, female, edu0:edu2 # native, hhsize,
      # t1_pv, t2_pv
    ) %>%
    pivot_longer(cols = everything()) %>%
    mutate(
      target = study,
      Variables = recode(name,
        age = "Age in years",
        edu0 = "low (ISCED 0-3)",
        edu1 = "intermediate (ISCED 4/5B)",
        edu2 = "high (ISCED 5A/6)",
        age0 = "18-34 years",
        age1 = "35-44 years",
        age2 = "45-54 years",
        age3 = "55 + years",
        hhsize = "N. of person in houseold",
        female = "Female",
        native = "German native speaker" # ,
        # t1_pv = "Skill level at T1",
        # t2_pv = "Skill level at T2",
      )
    ) %>%
    group_by(Variables)

  head(data)

  descriptives <- data %>%
    summarize(
      "Range" = str_c(min(value, na.rm = T), " -- ", max(value, na.rm = T)),
      "\\emph{M}" = mean(value, na.rm = T),
      "\\emph{SD}" = sd(value, na.rm = T)
    )

  descriptives[-1] <- printnum(descriptives[-1])

  descriptives
}



# Function to get pooled unweighted descriptive statistics
# on skills and skill change
describe_skills <- function(study) {
  data <- get(study) %>%
    mutate(delta = t2_pv - t1_pv) %>%
    group_by(.imp) %>%
    summarize(across(c(t1_pv, t2_pv, delta),
      list(Min = min, Max = max, M = mean, SD = sd),
      .names = "{.col}_{.fn}"
    )) %>%
    summarise(across(everything(), mean)) %>%
    printnum()

  domain <- if_else(grepl("reading", study), "Literacy", "Numeracy")
  # Construct table manually
  descriptives <- rbind(
    cbind(
      str_c(domain, " at $T_1$"),
      str_c(data$t1_pv_Min, " -- ", data$t1_pv_Max),
      data$t1_pv_M,
      data$t1_pv_SD
    ),
    cbind(
      str_c(domain, " at $T_2$"),
      str_c(data$t2_pv_Min, " -- ", data$t2_pv_Max),
      data$t2_pv_M,
      data$t2_pv_SD
    ),
    cbind(
      str_c("Change in ", domain, " ($\\Delta{T_1, T_2}$)"),
      str_c(data$delta_Min, " -- ", data$delta_Max),
      data$delta_M,
      data$delta_SD
    )
  )

  descriptives[-1] <- printnum(descriptives[-1])

  descriptives
}

data_descriptives <- cbind(
  describe_data("reading_piaac"),
  describe_data("reading_neps")[-1]
)

data_skills <- rbind(
  cbind(
    describe_skills("reading_piaac"),
    describe_skills("reading_neps")
  )[, -5],
  cbind(
    describe_skills("math_piaac"),
    describe_skills("math_neps")
  )[, -5]
) %>%
  as.data.frame()

names(data_skills) <- names(data_descriptives)


# Additional information --------------------------------------------------

# Compute pooled correlations between change (delta) and initial level (t1)
cors_pooler2 <- function(target) {
  data_filtered <- get(target) %>%
    split(.$.imp) %>%
    imputationList()

  sink("NUL")

  results <- with(data_filtered, lm(scale(t2_pv - t1_pv) ~ scale(t1_pv),
    weights = weight
  )) %>%
    MIcombine() %>%
    summary()

  sink()

  resultlist <- tibble(
    rho = results[["results"]][2],
    se = results[["se"]][2],
    lower = results[["(lower"]][2],
    upper = results[["upper)"]][2]
  )

  resultlist
}

# Create a tibble for computing the PV-based correlations of t1 with delta
cors_t1_delta <- tibble(
  target = c("reading_neps", "math_neps", "reading_piaac", "math_piaac"),
)

# Map the function to the tibble to obtain the results
cors_t1_delta <- cors_t1_delta %>%
  rowwise() %>%
  mutate(ergebnis = list(cors_pooler2(target))) %>%
  unnest(ergebnis) %>%
  mutate(
    domain = str_extract(target, pattern = "reading|math"),
    study = str_extract(target, pattern = "neps|piaac"),
    CI = str_c("[", round(lower, 2), "; ", round(upper, 2), "]")
  )


# Cross-sectional age differences in skills -------------------------------

# Cross-sectional skill differences between youngest and oldest age group
sd_pooler <- function(data) {
  sd <- get(data) %>%
    group_by(.imp) %>%
    summarise(SD = 0.5 * (sd(t1_pv) + sd(t2_pv)) ) %>%
    summarise(SD = mean(SD)) %>%
    unlist()
  sd
}

means_pooler <- function(data) {
  get(data) %>%
    group_by(.imp, agegr) %>%
    summarise(skillmean = mean(t1_pv)) %>%
    ungroup() %>%
    group_by(agegr) %>%
    summarise(skillmean = mean(skillmean))
}

skill_sds <- bind_rows(
  .id = "target",
  reading_piaac = sd_pooler("reading_piaac"),
  math_piaac = sd_pooler("math_piaac"),
  reading_neps = sd_pooler("reading_neps"),
  math_neps = sd_pooler("math_neps")
)

skill_means <- bind_rows(
  .id = "target",
  reading_piaac = means_pooler("reading_piaac"),
  math_piaac = means_pooler("math_piaac"),
  reading_neps = means_pooler("reading_neps"),
  math_neps = means_pooler("math_neps")
) %>%
  left_join(skill_sds, by = "target")

age_diffs <- skill_means %>%
  group_by(target) %>%
  mutate(
    young_old_raw = first(skillmean) - last(skillmean),
    young_old = (first(skillmean) - last(skillmean)) / SD
  ) %>%
  group_by(target) %>%
  summarise(
    young_old_raw = first(young_old_raw),
    young_old = first(young_old)
  )

# Estimate hypothetical linear age effects per year, study period and decade
# in the cross-sectional data

age_fx <- function(study) {
  temp <- get(study) %>%
    split(.$.imp) %>%
    imputationList()

  age_linear <- with(
    temp,
    lm(t1_pv ~ age,
      weights = weight
    )
  ) %>%
    MIcombine()

  period_length <- if_else(grepl("piaac", study), 3, 6)

  age_linear <- age_linear %>%
    coefficients() %>%
    pluck("age") %>%
    as_tibble_col(column_name = "per_year") %>%
    mutate(
      per_period = per_year * period_length,
      per_decade = per_year * 10,
      across(everything(),
        ~ .x / filter(skill_sds, target == study)[["SD"]],
        .names = "{.col}_sd"
      )
    ) %>%
    rename(
      "Per year (raw)" = per_year,
      "Per year (SD)" = per_year_sd,
      "Per period (raw)" = per_period,
      "Per period (SD)" = per_period_sd,
      "Per decade (raw)" = per_decade,
      "Per decade (SD)" = per_decade_sd
    )

  age_linear
}

age_effects <- bind_cols(
  "Change" = names(age_fx("reading_neps")),
  "Literacy (PIAAC-L)" = age_fx("reading_piaac") %>% unlist(),
  "Numeracy (PIAAC-L)" = age_fx("math_piaac") %>% unlist(),
  "Literacy (NEPS)" = age_fx("reading_neps") %>% unlist(),
  "Numeracy (NEPS)" = age_fx("math_neps") %>% unlist()
) %>%
  mutate(across(is.numeric, printnum)) %>%
  arrange(rev(Change))
