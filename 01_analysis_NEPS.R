# Project Info: ----------------------------------------------------------------
# Lechner et al. (2020). Stability and Change in Literacy and Numeracy
# Step 2: Analyses in NEPS.
# This code was written by clemens.lechner@gesis.org
# R 4.0.2

# Basic settings ---------------------------------------------------------------
rm(list = ls())

# List of subdirectories
dirs <- list(
  main = "D:/Dropbox/Forschung und Lehre/Stability_PIAAC_NEPS"
  #results = "./02_results_new"
)

setwd(dirs$main)

# Load required packages
pckloader <- function(pcklist) {
  for (pckg in pcklist) {
    if (!pckg %in% .packages(all = T)) {
      message("Installing package: ", pckg)
      install.packages(pckg, dep = T)
      library(pckg, character.only = T)
    } else {
      message("Package already installed: ", pckg)
      library(pckg, character.only = T)
    }
  }
}

pckloader(c(
  "tidyverse", "sjlabelled", "sjPlot", "xtable", "glue", "stringr",
  "mice", "miceadds", "mitools"
))

# Load data containing the previously estimated plausible values (PVs)
# and add new column "total" (needed only for simplified computation inside the
# functions defined next)
load("math.Rda") 
load("reading.Rda") 

reading <- reading %>% mutate(total = 1) 
math <- math %>% mutate(total = 1) 

# Analyzing mean-level change in competences ------------------------------


# Write a function that computes difference scores for a given group in the
# pools the results across the PVs (imputations) and saves everything including
#

means_pooler <- function(grouping, group, domain) {
  data <- get(domain) %>%
    filter(!is.na(PV_w9) & !is.na(PV_w3))

  exists <- data %>%
    filter(.data[[grouping]] == {{ group }}) %>%
    nrow()

  cat(paste0(
    "\nGroup ", grouping, " = ", group, " has ",
    exists / n_distinct(data$.imp),
    " cases for the domain of ", domain
  ))
  sink("NUL")
  pvlist <- data %>%
    filter(.data[[grouping]] == {{ group }}) %>%
    split(.$.imp) %>%
    imputationList()

  if (exists > 0) {
  
    results <- with(pvlist, lm(PV_w9 - PV_w3 ~ 1)) %>%
      MIcombine() %>%
      summary()

    
  # Computing sds using the full sample
    sds <- get(domain) %>%
      filter(!is.na(PV_w9) & !is.na(PV_w3)) %>% 
      group_by(.imp) %>% 
      summarise(pooled = 0.5*(sd(PV_w3) + sd(PV_w9)),
                delta = sd(PV_w9 - PV_w3)) %>% 
      summarise(across(everything(), mean))
    
    sink()
  # Extracting results in tidy format 
    resultlist <- tibble(
      diff = results[["results"]],
      se = results[["se"]],
      lower = results[["(lower"]],
      upper = results[["upper)"]],
      d_z = diff / sds$delta,
      d_av = diff / sds$pooled
    )
  } else {
    resultlist <- "Non-existent (sub-)group"
  }
  resultlist
}

# Create a tibble with combinations of domains and subgroups
deltas <- expand_grid(
  domain = c("reading", "math"),
  grouping = c("total", "gender", "agegr", "edugr"),
  group = c(0:3)
) %>%
  filter(!(grouping == "gender" & group %in% c(0, 3)) &
    !(grouping == "edugr" & group == 3) &
    !(grouping == "total" & group != 1)) %>%
  arrange(desc(domain), desc(grouping), group)

# Map the function to the tibble to obtain the results
deltas <- deltas %>%
  rowwise() %>%
  mutate(ergebnis = list(means_pooler(grouping, group, domain))) %>%
  unnest(ergebnis)

deltas 
# Add information for the total sample

# Equivalently,
# diffs <- diffs %>%
#  mutate(ergebnis = pmap(list(grouping, group, domain), pooler)) %>%
#  unnest(ergebnis)
# or
# diffs <- diffs %>%  rowwise %>%
#  mutate(ergebnis = pmap(list(grouping, group, domain), pooler)) %>%
#  unnest(ergebnis)


# Analysis: Rank-order stability ------------------------------------------
# Write a function that computes difference scores for a given group in the
# pools the results across the PVs (imputations) and saves everything including

cors_pooler <- function(grouping, group, domain) {
  data <- get(domain) %>%
    filter(!is.na(PV_w9) & !is.na(PV_w3))

  exists <- data %>%
    filter(.data[[grouping]] == {{ group }}) %>%
    nrow()

  cat(paste0(
    "\nGroup ", grouping, " = ", group, " has ",
    exists / n_distinct(data$.imp),
    " cases for the domain of ", domain
  ))

  pvlist <- data %>%
    filter(.data[[grouping]] == {{ group }}) %>%
    split(.$.imp) %>%
    imputationList()

  if (exists > 0) {
    sink("NUL")

    results <- with(pvlist, lm(scale(PV_w9) ~ scale(PV_w3))) %>%
      MIcombine() %>%
      summary()
   
    
    sink()

    resultlist <- tibble(
      rho = results[["results"]][2],
      se = results[["se"]][2],
      lower = results[["(lower"]][2],
      upper = results[["upper)"]][2]
    )
  } else {
    resultlist <- "Non-existent (sub-)group"
  }

  resultlist
}

# Create a tibble for computing the PV-based correlations per subgroup
cors <- expand_grid(
  domain = c("reading", "math"),
  grouping = c("total", "gender", "agegr", "edugr"),
  group = c(0:3)
) %>%
  filter(!(grouping == "gender" & group %in% c(0, 3)) &
    !(grouping == "edugr" & group == 3) &
    !(grouping == "total" & group != 1)) %>%
  arrange(desc(domain), desc(grouping), group)

# Map the function to the tibble to obtain the results
cors <- cors %>%
  rowwise() %>%
  mutate(ergebnis = list(cors_pooler(grouping, group, domain))) %>%
  unnest(ergebnis)

cors

# Reliable change indices (RCIs) -----------------------------------------------

rci_pooler <- function(grouping, group, domain) {
  data <- get(domain) %>%
    filter(!is.na(PV_w9) & !is.na(PV_w3))
  
  # Compute the SD of the competence at T1 in the full sample (not subgroup)
  
  sds <- data %>% 
    group_by(.imp) %>% 
    summarise(t1 = sd(PV_w3)) %>% 
    summarise(across(everything(), mean))
  
  # Define generic function to compute RCI, using the correlation between the
  # two measurement occasions as the reliability / stability measure
  # Three indices are computed: 
  # (1) the raw RCI (akin to an effect size measure of change per individual, 
  # (2) upward change > 1.96 SDdiff or not, 
  # (3) downward change smaller than 1.96 SDdiff or not)
  # Note: RCI is subgroup-specific but rxx and SDt1 are from the full sample
  rci <- function(data) {
    data <- data %>% 
      filter(.data[[grouping]] == {{ group }}) %>%
      mutate(rci_raw = .data$PV_w9 - .data$PV_w3 /
                       (sqrt(2*(sds$t1 * sqrt((1 - rxx)^2)))),
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
  rxx <- with(imputationList(pvlist), 
              lm(scale(PV_w9) ~ scale(PV_w3) )) %>%
    MIcombine() %>%
    summary() %>% 
    pluck("results", 2)
  
  # Add the RCI variables to each dataset in the list of datasets containing PVs
  sink("NUL")  
 
  pvlist <- pvlist %>%
    map(rci) %>%
    map(~select(., rci_raw, rci_up, rci_down))
  
  results <- tibble(
    raw = with(imputationList(pvlist), lm(rci_raw ~ 1)) %>% 
      MIcombine %>%
      summary() %>% 
      pluck("results", 1),
    up =  with(imputationList(pvlist), lm(rci_up ~ 1)) %>% 
      MIcombine %>%
      summary() %>% 
      pluck("results", 1) * 100,
    down =  with(imputationList(pvlist), lm(rci_down ~ 1)) %>% 
      MIcombine %>%
      summary() %>% 
      pluck("results", 1) * 100
  )
  sink()
  
  results
}

# Create a tibble with combinations of domains and subgroups
rcis <- expand_grid(
  domain = c("reading", "math"),
  grouping = c("total", "gender", "agegr", "edugr"),
  group = c(0:3)
) %>%
  filter(!(grouping == "gender" & group %in% c(0, 3)) &
           !(grouping == "edugr" & group == 3) &
           !(grouping == "total" & group != 1)) %>%
  arrange(desc(domain), desc(grouping), group)

# Map the function to the tibble to obtain the results
rcis <- rcis %>%
  rowwise() %>%
  mutate(ergebnis = list(rci_pooler(grouping, group, domain))) %>%
  unnest(ergebnis)

rcis 



# Consistency Checks ------------------------------------------------------


# Analysis
reading_n <- reading %>%
  filter(.imp == 1 & !is.na(PV_w3) & !is.na(PV_w9)) %>%
  summarise("n" = n()) %>%
  unlist()

# Manual pooling exercise to check consistency of results
# for the total sample
reading %>%
  summarise("cor39" = cor(PV_w3, PV_w9, use = "complete")) %>%
  mutate(FisherZ = psych::fisherz(cor39)) %>%
  summarise("pooled_z" = mean(FisherZ)) %>%
  mutate(
    var_z = (1 / (reading_n - 3)),
    pooled_cor = psych::fisherz2r(pooled_z)
  )
