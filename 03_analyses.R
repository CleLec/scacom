# Project Info: ----------------------------------------------------------------
# Stability and Change in Adults' Literacy and Numeracy
# Step 3: Analyses in PIAAC-L and NEPS
# This code was written by [blinded for review]
# R 4.0.3

# Basic settings ---------------------------------------------------------------
rm(list = ls())

# List of subdirectories
dirs <- list(
  results = "./02_results"
)


# Load required packages
library(tidyverse)
library(purrr)
library(glue)
library(sjlabelled)
library(mice)
library(miceadds)
library(mitools)
library(srvyr)
library(survey)
#library(srvyr, lib.loc = "D:/R library")


# Load data containing the previously estimated plausible values (PVs)
# and add new column "total" (needed only for simplified computation inside the
# functions defined next)

map(
  str_c(dirs$results, "/", 
         c("data_neps.Rda",
           "data_piaac.Rda")),
  load, .GlobalEnv)


# Compute addon variables: 
# T1 Skill Quartile
# Filter non-native speakers
compute_addons <- function(data) {
  get(data) %>% 
    group_by(.imp) %>%
    mutate(
     skillgr = ntile(t1_pv, 4)
       # skillgr = case_when(
       #  t1_pv > quantile(t1_pv, 0.75, na.rm = TRUE) ~ 3,
       #  t1_pv <= quantile(t1_pv, 0.25, na.rm = TRUE) ~ 0,
       #  t1_pv > quantile(t1_pv, 0.25, na.rm = TRUE) & t1_pv <= quantile(t1_pv, 0.5, na.rm = TRUE) ~ 1,
       #  t1_pv > quantile(t1_pv, 0.50, na.rm = TRUE) & t1_pv <= quantile(t1_pv, 0.75, na.rm = TRUE) ~ 2),
           ) %>%
    ungroup() %>%
    
    assign(data, value = ., envir = .GlobalEnv )
  
}

map(c(
  "math_neps", "reading_neps",
  "math_piaac", "reading_piaac"
), compute_addons)

rm(compute_addons)

# Options for survey package
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")
options(survey.multicore = TRUE)
use_weight <- ~weight

# Analyzing mean-level change in competences ------------------------------


# Write a function that computes difference scores for a given group in the
# pools the results across the PVs (imputations) and saves everything including

deltas_pooler <- function(grouping, group, target, weight = use_weight) {


  implist <- get(target) %>%
    filter(.data[[grouping]] == {{ group }}) %>%
    group_split(.imp) %>% 
    mitools::imputationList() 

  
  subsample_design <- svydesign(ids = ~psu, 
                                strata = ~stratum, 
                                nest = TRUE,
                                data = implist, 
                                weights = weight) %>%
                                subset(language == 1)
  
  
  # Compute pooled means in the (sub-)sample
  
  pool_mean <- function(fml) {
    with(
      subsample_design,
      svymean(fml) 
    ) %>%
      mitools::MIcombine() %>%
      summary()
      }
  
  delta <- pool_mean(~I(t2_pv - t1_pv))
  t1 <- pool_mean(~t1_pv)  
  t2 <- pool_mean(~t2_pv)  
  
  # Compute pooled SDs in the (sub-) sample
  
  pool_sd <- function(fml) {
    
    with(
      subsample_design,
      svyvar(fml) 
    ) %>%
      mitools::MIcombine() %>%
      summary() %>%
      pluck("results") %>% 
      sqrt()
    
  }
  
  sd_t1 <- pool_sd(~t1_pv)
  sd_t2 <- pool_sd(~t2_pv)
  sd_delta <- pool_sd(~I(t2_pv- t1_pv))
  
  
  # All effect sizes are based on the total sample SD 
  sd_pooled <- 0.5*(sd_t1 + sd_t2)

  dav <- with(
    subsample_design,
    svymean(~I((t2_pv - t1_pv) / (0.5*(sd_t1 + sd_t2))))
  ) %>%
    mitools::MIcombine() %>%
    summary() 
  
    
  # Extract results in tidy format
  resultlist <- bind_cols(
    delta = delta[["results"]], # T2-T1 difference score
    se = delta[["se"]], # SE of the difference score
    lower = delta[["(lower"]], # lower bound of difference score
    upper = delta[["upper)"]], # upper bound
    t1 = t1[["results"]],
    t2 = t2[["results"]],
    sd_t1 = sd_t1, # SD of the first measurement occation (T1)
    sd_t2 = sd_t2, # SD of the second measurement occation (T2)
    sd_pooled = sd_pooled, # SD of the first measurement occation (T1)
    sd_delta = sd_delta, # SD of the difference score
    dav2 = delta[["results"]] /  sd_pooled, # Cohen's dav (based on pooled SD)
    dav = dav[["results"]],
    dav_lower = dav[["(lower"]], # lower bound of Cohen's dav
    dav_upper = dav[["upper)"]] # lower bound of Cohen's dav
    
  )
  
  resultlist
}

# Create a tibble with combinations of targets and subgroups
deltas <- expand_grid(
  target = c("reading_neps", "math_neps", "reading_piaac", "math_piaac"),
  grouping = c("total", "gender", "agegr", "edugr", 
               # "language",
               "skillgr"
               ),
  group = c(0:3)
) %>%
  filter(!(grouping == "gender" & group %in% c(0, 3)) &
           !(grouping == "edugr" & group == 3) &
           !(grouping == "total" & group != 1) #&
         #  !(grouping == "language" & group %in% c(0, 3)) 
  ) %>%
  arrange(desc(target), desc(grouping), group)

# Map the function to the tibble to obtain the results
deltas <- deltas %>%
  rowwise() %>%
  mutate(results = list(deltas_pooler(grouping, group, target))) %>%
  unnest(results)

deltas <- 
  mutate(deltas, 
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


cors_pooler <- function(grouping, group, target, weight = use_weight) {
  
  implist <- get(target) %>%
    filter(.data[[grouping]] == {{ group }}  
         #  &  language == 1
         & !(is.na(t1_pv) | is.na(t2_pv))
           ) %>%
    #filter(.imp < 11) %>% 
    group_by(.imp) %>% 
    group_split() %>% 
    mitools::imputationList() 
  
  
  
  subsample_design <- svydesign(ids = ~psu, 
                                strata = ~stratum, 
                                nest = TRUE,
                                data = implist, 
                                weights = weight) %>%
                      subset(language == 1)  
  
  results <- with(
    subsample_design,
    svyglm(
      scale(t1_pv) ~ scale(t2_pv))
  ) %>%
    mitools::MIcombine() %>%
    summary() 
  
  resultlist <- bind_rows(
    rho = results[["results"]][2],
    se = results[["se"]][2],
    lower = results[["(lower"]][2],
    upper = results[["upper)"]][2]
  )
  
  resultlist
}


# Create a tibble for computing the PV-based correlations per subgroup
cors <- expand_grid(
  target = c("reading_neps", "math_neps", "reading_piaac", "math_piaac"),
  grouping = c("total", "gender", "agegr", "edugr"#, 
                #"language",  "skillgr"
               ),
  group = c(0:3)
) %>%
  filter(!(grouping == "gender" & group %in% c(0, 3)) &
           !(grouping == "edugr" & group == 3) &
           !(grouping == "total" & group != 1) #&
          #!(grouping == "language" & group %in% c(0, 3)) 
  ) %>%
  arrange(desc(target), desc(grouping), group)

# Map the function to the tibble to obtain the results
cors <- cors %>%
  rowwise() %>%
  mutate(ergebnis = list(cors_pooler(grouping, group, target))) %>%
  unnest(ergebnis) 

cors <- mutate(cors, 
               domain = str_extract(target, pattern = "reading|math"),
               study = str_extract(target, pattern = "neps|piaac")
)

cors



# Create  tables -----------------------------------------

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
      "edugr=0" = "low (ISCED 0--3)",
      "edugr=1" = "intermediate (ISCED 4/5B)",
      "edugr=2" = "high (ISCED 5A/6)",
      "agegr=0" = "18--34 years",
      "agegr=1" = "35--44 years",
      "agegr=2" = "45--54 years",
      "agegr=3" = "55 + years"#,
       #"language=1" = "Native speaker",
       #"language=2" = "Non-native speaker",
       #"skillgr=0" = "1st quartile",
       #"skillgr=1" = "2nd quartile",
       #"skillgr=2" = "3rd quartile",
       #"skillgr=3" = "4th quartile"
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
      !!delta.name :=  sprintf("%.2f", delta),
      !!ci.name := str_c(
        "[",
        sprintf("%.2f", lower),
        ", ",
        sprintf("%.2f", upper),
        "]",
        sep = ""
      ),
      !!sd.name :=  sprintf("%.2f", sd_pooled),
      !!dav.name := sprintf("%.2f", dav),
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
      "edugr=0" = "low (ISCED 0--3)",
      "edugr=1" = "intermediate (ISCED 4/5B)",
      "edugr=2" = "high (ISCED 5A/6)",
      "agegr=0" = "18--34 years",
      "agegr=1" = "35--44 years",
      "agegr=2" = "45--54 years",
      "agegr=3" = "55 + years" ,#,
       #"language=1" = "Native speaker",
       #"language=2" = "Non-native speaker",
       "skillgr=0" = "1st quartile",
       "skillgr=1" = "2nd quartile",
       "skillgr=2" = "3rd quartile",
      "skillgr=3" = "4th quartile"
    )
  )) %>%
  select(Group, matches("piaac|neps"))



# Save main results --------------------------------------------------------

save(list = c("cors", "deltas", "cors.table", "deltas.table"),
     file = "02_results/results.Rda")
