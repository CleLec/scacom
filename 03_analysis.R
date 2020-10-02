# Project Info: ----------------------------------------------------------------
# Lechner et al. (2020). Stability and Change in Literacy and Numeracy
# Step 4: Analyses in PIAAC-L.
# This code was written by clemens.lechner@gesis.org
# R 4.0.2

# Basic settings ---------------------------------------------------------------
rm(list = ls())

# List of subdirectories
dirs <- list(
  main = "D:/Dropbox/Forschung und Lehre/Stability_PIAAC_NEPS"
  # results = "./02_results"
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
    diff = deltas[["results"]], # T2-T1 difference score
    se = deltas[["se"]], # SE of the difference score
    lower = deltas[["(lower"]], # lower bound of difference score
    upper = deltas[["upper)"]], # upper bound
    d_z = diff / sds[["delta"]], # Cohen's d_z
    d_av = diff / sds[["pooled"]] # Cohens's d_av
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
  data <- get(target)



  # Define generic function to compute RCI, using the correlation between the
  # two measurement occasions as the reliability / stability measure
  # Three indices are computed:
  # (1) the raw RCI (akin to an effect size measure of change per individual,
  # (2) upward change > 1.96 SDdiff or not,
  # (3) downward change smaller than 1.96 SDdiff or not)
  # Note: RCI is subgroup-specific but rxx and SDt1 are from the full sample
  rci <- function(data) {
    data <- data %>%
      mutate(sd_t1 = sd(t1_pv)) %>%
      filter(.data[[grouping]] == {{ group }}) %>%
      mutate(
        rci_raw = (t2_pv - t1_pv) /
          (sqrt(2 * (sd_t1 * sqrt((1 - rxx)^2)))),
        rci_up = .data$rci_raw > 1.96,
        rci_down = .data$rci_raw < -1.96
      )
    data
  }

  # Generate list of data sets containing the plausible values (PVs)
  pvlist <- data %>%
    split(.$.imp)


  # Compute rxx, the correlation between competences at both time points for the
  # full sample
  rxx <- with(
    imputationList(pvlist),
    lm(scale(t2_pv) ~ scale(t1_pv), )
  ) %>%
    MIcombine() %>%
    summary() %>%
    pluck("results", 2)

  # Add the RCI variables to each dataset in the list of datasets containing PVs
  sink("NUL")

  pvlist <- pvlist %>%
    map(rci) %>%
    map(~ select(., rci_raw, rci_up, rci_down)) %>%
    imputationList()


  results <- tibble(
    raw = with(pvlist, lm(rci_raw ~ 1)) %>%
      MIcombine() %>%
      summary() %>%
      pluck("results", 1),
    up = with(pvlist, lm(rci_up ~ 1)) %>%
      MIcombine() %>%
      summary() %>%
      pluck("results", 1) * 100,
    down = with(pvlist, lm(rci_down ~ 1)) %>%
      MIcombine() %>%
      summary() %>%
      pluck("results", 1) * 100
  )
  sink()

  results
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
  unnest(ergebnis)

rcis
