# Project Info: ----------------------------------------------------------------
# Lechner et al. (2020). Stability and Change in Literacy and Numeracy
# Step 4: Analyses in PIAAC-L.
# This code was written by clemens.lechner@gesis.org
# R 4.0.2

# Basic settings ---------------------------------------------------------------
rm(list = ls())

# List of subdirectories
dirs <- list(
  main = "D:/Dropbox/Forschung und Lehre/Stability_PIAAC_NEPS",
  results = "./02_results"
)


# Load required packages
library(tidyverse)
library(glue)
library(sjlabelled)
library(mice)
library(miceadds)
library(mitools)

# Load data containing the previously estimated plausible values (PVs)
# and add new column "total" (needed only for simplified computation inside the
# functions defined next)

map(c(
  "math_neps.Rda", "reading_neps.Rda",
  "math_piaac.Rda", "reading_piaac.Rda"
), load, .GlobalEnv)

# Analyzing mean-level change in competences ------------------------------


# Write a function that computes difference scores for a given group in the
# pools the results across the PVs (imputations) and saves everything including

deltas_pooler <- function(grouping, group, target) {

  # Get the target data (defined by study and competence domain)
  # from the global environment and convert it to a imputation list
  pvlist <- get(target) %>%
    filter(.data[[grouping]] == {{ group }}) %>%
    split(.$.imp) %>%
    imputationList()

  # Switch off messages from mitools
  sink("NUL")

  # Perform analysis of mean-level change and pool it
  deltas <- with(
    pvlist,
    lm(t2_pv - t1_pv ~ 1,
      weights = weight
    )
  ) %>%
    MIcombine() %>%
    summary()

  # Compute pooled  SDs in the full sample

  sds <- get(target) %>%
    group_by(.imp) %>%
    summarise(
      pooled = 0.5 * (sd(t1_pv) + sd(t2_pv)),
      delta = sd(t2_pv - t1_pv)
    ) %>%
    summarise(across(where(is.numeric), mean))

  sink()

  # Extract results in tidy format
  resultlist <- tibble(
    delta = deltas[["results"]], # T2-T1 difference score
    se = deltas[["se"]], # SE of the difference score
    lower = deltas[["(lower"]], # lower bound of difference score
    sd = sds[["delta"]], # SD of the difference score
    upper = deltas[["upper)"]], # upper bound
    d_z = delta / sds[["delta"]], # Cohen's d_z
    d_av = delta / sds[["pooled"]] # Cohens's d_av
  )

  resultlist
}

# Create a tibble with combinations of targets and subgroups
deltas <- expand_grid(
  target = c("reading_piaac", "math_piaac", "reading_neps", "math_neps"),
  grouping = c("total", "gender", "agegr", "edugr"),
  group = c(0:3)
) %>%
  filter(!(grouping == "gender" & group %in% c(0, 3)) &
    !(grouping == "edugr" & group == 3) &
    !(grouping == "total" & group != 1)) %>%
  arrange(desc(target), desc(grouping), group)

# Map the function to the tibble to obtain the results
deltas <- deltas %>%
  rowwise() %>%
  mutate(results = list(deltas_pooler(grouping, group, target))) %>%
  unnest(results) %>%
  mutate(
    domain = str_extract(target, pattern = "reading|math"),
    study = str_extract(target, pattern = "neps|piaac")
  )

deltas

# Add information for the total sample

# Equivalently,
# diffs <- diffs %>%
#  mutate(ergebnis = pmap(list(grouping, group, target), pooler)) %>%
#  unnest(ergebnis)
# or
# diffs <- diffs %>%  rowwise %>%
#  mutate(ergebnis = pmap(list(grouping, group, target), pooler)) %>%
#  unnest(ergebnis)


# Analysis: Rank-order stability ------------------------------------------
# Write a function that computes difference scores for a given group in the
# pools the results across the PVs (imputations) and saves everything including

cors_pooler <- function(grouping, group, target) {
  pvlist <- get(target) %>%
    filter(.data[[grouping]] == {{ group }}) %>%
    split(.$.imp) %>%
    imputationList()

  sink("NUL")

  results <- with(pvlist, lm(scale(t2_pv) ~ scale(t1_pv),
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

# Create a tibble for computing the PV-based correlations per subgroup
cors <- expand_grid(
  target = c("reading_piaac", "math_piaac", "reading_neps", "math_neps"),
  grouping = c("total", "gender", "agegr", "edugr"),
  group = c(0:3)
) %>%
  filter(!(grouping == "gender" & group %in% c(0, 3)) &
    !(grouping == "edugr" & group == 3) &
    !(grouping == "total" & group != 1)) %>%
  arrange(desc(target), desc(grouping), group)

# Map the function to the tibble to obtain the results
cors <- cors %>%
  rowwise() %>%
  mutate(ergebnis = list(cors_pooler(grouping, group, target))) %>%
  unnest(ergebnis) %>%
  mutate(
    domain = str_extract(target, pattern = "reading|math"),
    study = str_extract(target, pattern = "neps|piaac")
  )

cors

# Reliable change indices (RCIs) -----------------------------------------------


rci_pooler <- function(grouping, group, target) {
  # Define generic function to compute RCI, using the correlation between the
  # two measurement occasions as the reliability / stability measure
  # Three indices are computed:
  # (1) the raw RCI (akin to an effect size measure of change per individual,
  # (2) upward change > 1.96 SDdiff or not,
  # (3) downward change smaller than 1.96 SDdiff or not)
  # Note: RCI is subgroup-specific but rxx and SDt1 are from the full sample
  rci_data <- get(target) %>%
    group_by(.imp) %>%
      mutate(sd_t1 = sd(t1_pv), # Full sample sd and rxx within each imputation
             rxx = cor(t1_pv, t2_pv)) %>%
      ungroup() %>% 
      mutate(
        rci_raw = (t2_pv - t1_pv) /
          (sqrt(2 * (.data$sd_t1 * sqrt(1 - .data$rxx))^2)),
        rci_up = .data$rci_raw >= 1.96,
        rci_down = .data$rci_raw <= -1.96
      )

  rci_pooled <- rci_data %>%
    filter(.data[[grouping]] == {{ group }}) %>%
    summarise(
      raw = mean(rci_raw),
      up = mean(rci_up) * 100,
      down = mean(rci_down) * 100
    )
  
  rci_pooled
  
}

# Create a tibble with combinations of targets and subgroups
rcis <- expand_grid(
  target = c("reading_piaac", "math_piaac", "reading_neps", "math_neps"),
  grouping = c("total", "gender", "agegr", "edugr"),
  group = c(0:3)
) %>%
  filter(!(grouping == "gender" & group %in% c(0, 3)) &
    !(grouping == "edugr" & group == 3) &
    !(grouping == "total" & group != 1)) %>%
  arrange(desc(target), desc(grouping), group)

# Map the function to the tibble to obtain the results
rcis <- rcis %>%
  rowwise() %>%
  mutate(ergebnis = list(rci_pooler(grouping, group, target))) %>%
  unnest(ergebnis )%>%
  mutate(
    domain = str_extract(target, pattern = "reading|math"),
    study = str_extract(target, pattern = "neps|piaac")
  )


rcis



# Create publication-ready tables -----------------------------------------

# Correlation tables
cor_shaper <- function(which.domain, which.study) {
  rho.name <- glue("r.{which.study}.{which.domain}")
  ci.name <- glue("ci.{which.study}.{which.domain}")
  cors %>%
    filter(study == which.study & domain == which.domain) %>%
    mutate(
      !!rho.name := str_sub(rho, 2, 4),
      !!ci.name := str_c(
        "[",
        str_sub(lower, 2, 4),
        ", ",
        str_sub(upper, 2, 4), "]",
        sep = ""
      ),
      Group = str_c(
        grouping, group,
        sep = "="
      ),
    ) %>%
    select(Group, rho.name, ci.name)
}

cors.table <-
  bind_cols(
    read.piaac = cor_shaper("reading", "piaac"),
    math.piaac = cor_shaper("math", "piaac"),
    read.neps = cor_shaper("reading", "neps"),
    math.neps = cor_shaper("math", "neps"),
    .name_repair = "unique"
  ) %>%
  mutate(Group = str_replace_all(
    Group...1,
    c(
      "total=1" = "Total sample",
      "gender=1" = "Male",
      "gender=2" = "Female",
      "edugr=0" = "low (ISCED 0–3)",
      "edugr=1" = "intermediate (ISCED 4/5B)",
      "edugr=2" = "high (ISCED 5A/6)",
      "agegr=0" = "18–34 years",
      "agegr=1" = "35–44 years",
      "agegr=2" = "45-54 years",
      "agegr=3" = "55 + years"
    )
  )) %>%
  select(Group, matches("piaac|neps"))

# Deltas tables

deltas_shaper <- function(which.domain, which.study) {
  delta.name <- glue("delta.{which.study}.{which.domain}")
  ci.name <- glue("ci.{which.study}.{which.domain}")
  sd.name <- glue("sd.{which.study}.{which.domain}")
  dav.name <- glue("dav.{which.study}.{which.domain}")
  
  deltas %>%
    filter(study == which.study & domain == which.domain) %>%
    mutate(
      !!delta.name := str_sub(delta, 1, 3),
      !!ci.name := str_c(
        "[",
        str_sub(lower, 1, 3),
        ", ",
        str_sub(upper, 1, 3), "]",
        sep = ""
      ),
      !!sd.name := str_sub(sd, 1,3),
      !!dav.name := str_sub(d_av, 1,3),
       Group = str_c(
        grouping, group,
        sep = "="
      ),
    ) %>%
    select(Group, delta.name, ci.name, sd.name, dav.name)
}

deltas.table <-
  bind_cols(
    read.piaac = deltas_shaper("reading", "piaac"),
    math.piaac = deltas_shaper("math", "piaac"),
    read.neps = deltas_shaper("reading", "neps"),
    math.neps = deltas_shaper("math", "neps"),
    .name_repair = "unique"
  ) %>%
  mutate(Group = str_replace_all(
    Group...1,
    c(
      "total=1" = "Total sample",
      "gender=1" = "Male",
      "gender=2" = "Female",
      "edugr=0" = "low (ISCED 0–3)",
      "edugr=1" = "intermediate (ISCED 4/5B)",
      "edugr=2" = "high (ISCED 5A/6)",
      "agegr=0" = "18–34 years",
      "agegr=1" = "35–44 years",
      "agegr=2" = "45-54 years",
      "agegr=3" = "55 + years"
    )
  )) %>%
  select(Group, matches("piaac|neps"))


# Save all results --------------------------------------------------------

save(cors, 
     file = glue({dirs$results}, "/", "cors.Rda"))
save(deltas, 
     file = glue({dirs$results}, "/", "deltas.Rda"))

save(cors.table, 
     file = glue({dirs$results}, "/", "cors.table.Rda"))

save(deltas.table, 
     file = glue({dirs$results}, "/", "deltas.table.Rda"))

                                    
