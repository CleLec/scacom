# Project Info: ----------------------------------------------------------------
# Stability and Change in Adults' Literacy and Numeracy
# Step 1: Data preparation
# This code was written by [blinded for review]
# R 4.0.3

# Basic settings ---------------------------------------------------------------
rm(list = ls())
# List of directories
dirs <- list(
  main = "D:/Dropbox/Forschung und Lehre/Stability_PIAAC_NEPS",
  data = "C:/users/lechnecs/Desktop/NEPS SC6 (11-0-0)/SPSS/en/",
  results = "./02_results"
)

# Version control with renv (run unce)
# renv::init()
# renv::snapshot()

# Load required packages
pckloader <- function(pcklist) {
  for (pckg in pcklist) {
    if (!pckg %in% .packages(all = T)) {
      message("Installing package: ", pckg)
      install.packages(pckg, dep = F)
      library(pckg, character.only = T)
    } else {
      message("Package already installed: ", pckg)
      library(pckg, character.only = T)
    }
  }
}

# Older version of haven is needed because the latest version creates issues
# when used in the NEPS PV tool
# install.packages("devtools")
# require(devtools)
# install_version("haven", version = "2.2.0",
#                repos = "http://cran.us.r-project.org")

# Installing the NEPS PV estimation tool
# install.packages("http://nocrypt.neps-data.de/r/nepsscaling_2.0.0.tar.gz",
#                 repos = NULL, type = "source")

library(tidyverse)
library(sjPlot)
library(sjlabelled)
library(xtable)
library(glue)
library(mvtnorm)
library(ucminf)
library(numDeriv)
library(haven)
library(TAM)
library(NEPSscaling)


# Preparing the data --------------------------------------------------------

data_basic <- sjlabelled::read_spss(
  str_c(
    dirs$data,
    "SC6_Basics_D_11-0-0.sav"
  ),
  atomic.to.fac = T, drop.labels = T
)

data_ptarget <- sjlabelled::read_spss(
  str_c(
    dirs$data,
    "SC6_pTarget_D_11-0-0.sav"
  ),
  drop.labels = T
)

# Information from Methods:
#' - tx80101: federal state
#' - tx80102: BIK category (size of town)
#' - inty. Interview year
#' - tx80107: ALWA or NEPS subsample
data_methods <- sjlabelled::read_spss(
  str_c(
    dirs$data,
    "SC6_Methods_D_11-0-0.sav"
  ),
  drop.labels = T,
) %>%
  filter(wave == 3) %>%
  select(ID_t, tx80101, tx80102, inty, tx80107)


# Information from pTarget:
#' - t34005a: number of books in the household
#' - t34001e_g1: Reading activity during leisure in hours
#' - t400500_g1: Generation status (1 and 2 are migrants)
# data_books <- data_ptarget %>%
#   select(ID_t, wave, t34005a, t400500_g1) %>%
#   arrange(ID_t, wave) %>%
#   group_by(ID_t) %>%
#   filter(t34005a %in% c(1:6)) %>%
#   summarize(books = first(t34005a)) %>%
#   ungroup() %>%
#   mutate(books = if_else(books > 3, 1, 0))

data_ptarget <- data_ptarget %>%
  select(ID_t, wave, t34005a, t34001e_g1, t400500_g1) %>%
  pivot_longer(-c("ID_t", "wave"), names_to = "variable") %>%
  na.omit() %>%
  group_by(ID_t, variable) %>%
  arrange(ID_t, variable, wave) %>%
  mutate(firstwave = first(wave)) %>% 
  filter(wave == firstwave) %>% 
  ungroup() %>% 
  select(ID_t, variable, value) %>% 
  pivot_wider(id_cols = "ID_t", names_from = "variable") %>%
  mutate(books = if_else(t34005a > 3, 1, 0), 
         migrant = case_when(t400500_g1 %in% c(1:3) ~ 1, TRUE ~ 0)) 
  


data_comp <- sjlabelled::read_spss(
  str_c(dirs$data, "SC6_xTargetCompetencies_D_11-0-0.sav")
)

data_weights <- sjlabelled::read_spss(
  str_c(
    dirs$data,
    "SC6_Weights_D_11-0-0.sav"
  )
) %>%
  select(ID_t, psu, stratum, w_t456789_std, w_t3_cal, w_t9_cal)

wletable <- tibble(
  var = data_comp %>% sjlabelled::get_label() %>% names(),
  varlab = data_comp %>% sjlabelled::get_label()
) %>%
  filter(str_detect(varlab, "WLE"))

data_merged <-
  full_join(data_basic,
    data_comp,
    by = "ID_t"
  ) %>%
  full_join(.,
    data_ptarget, # or data_books
    by = "ID_t"
  ) %>%
  full_join(.,
    data_methods,
    by = "ID_t"
  ) %>%
  select(ID_t,
    mpa3re_sc5, mpa9re_sc5, mpa3ma_sc5, mpa9ma_sc5, # procedural metacognit.
    inty, t70000y,
    isei = tx29063, 
    yborn = tx2900y, gender = t700001, eduyrs = tx28102,
    casmin = tx28101, isced = tx28103, language = tx29003,
    income = t510010_g1, books, migrant, leisure_reading = t34001e_g1,
    sample = tx80107,
    cumemp = tx29944, fases = t731453_g16, hhsize = t741001,
    math3 = maa3_sc1u, read3 = rea3_sc1u,
    math9 = maa9_sc1u, read9 = rea9_sc1u,
    ict = ica5_sc1, science = sca5_sc1,
    reasoning = dga7_sc3b, processing_speed = dga7_sc3a,
    state = tx80101, townsize = tx80102
  ) %>%
  mutate(
    age = inty - t70000y,
    age2 = age^2, age3 = age^3, 
    edugr = case_when(
      isced %in% c(0:5) ~ 0,
      isced %in% c(6:8) ~ 1,
      isced %in% c(9:10) ~ 2,
      TRUE ~ NA_real_)
  ) %>%
  # convert categorical variables to factors
  mutate(across(
    c(
      gender, edugr, isced, language, casmin, state,
      migrant, sample,
    ),
    as_factor
  ))


# Save data for later use
save(data_merged, file = file.path(dirs$results, "data_merged.Rda"))

# load("data_merged.Rda")

# Estimating PVs for reading  -----------------------------------------------

# Globally set the number of PVs
n_pvs <- 30

# Estimate the PVs
read_pvs <- plausible_values(
  SC = 6,
  wave = 3,
  path = dirs$data,
  domain = "RE",
  bgdata = select(
    data_merged,
    ID_t, sample, 
    age, age2, gender, migrant, 
    hhsize, state, townsize, 
    cumemp, fases, eduyrs,
    isced,  income, isei,
    books,  leisure_reading,
    reasoning, processing_speed,
    math3, math9, science, ict, 
    mpa3re_sc5, mpa9re_sc5
  ),
  npv = n_pvs,
  longitudinal = TRUE,
  min_valid = 3,
  include_nr = FALSE,
  verbose = TRUE,
  control = list(
    WLE = TRUE, EAP = TRUE,
    ML = list(
      nmi = n_pvs,
      itermcmc = 200, burnin = 100, thin = 1
    )
  )
)


# Estimating PVs for Math -------------------------------------------------


# Estimate the PVs for math
math_pvs <- plausible_values(
  SC = 6,
  wave = 3,
  path = dirs$data,
  domain = "MA",
  bgdata = select(
    data_merged,
    ID_t, sample,
    age, age2, gender, migrant, 
    hhsize, state, townsize, 
    cumemp, fases, 
    isced, isei, income,
    books,  leisure_reading,
    reasoning, processing_speed,
    read3, read9, science, ict, 
    mpa3ma_sc5, mpa9ma_sc5
    ),
  npv = n_pvs,
  longitudinal = TRUE,
  min_valid = 3,
  include_nr = FALSE,
  verbose = TRUE,
  control = list(
    WLE = TRUE, EAP = TRUE,
    ML = list(
      nmi = n_pvs,
      itermcmc = 200, burnin = 100, thin = 1
    )
  )
)




# Prepare data  -----------------------------------------------------

# Extract WLEs and EAPs
math_wles <- get_wle(math_pvs)
read_wles <- get_wle(read_pvs)
math_eaps <- get_eap(math_pvs)
read_eaps <- get_eap(read_pvs)

math_eaps %>%
  select(contains("eap")) %>%
  cor(use = "pairwise")

read_eaps %>%
  select(contains("eap")) %>%
  cor(use = "pairwise")


reading_neps <- read_pvs %>%
  get_pv_list() %>%
  set_names(1:n_pvs) %>%
  enframe(x = ., name = ".imp") %>%
  unnest(., value) %>%
  left_join(., read_wles, by = "ID_t") %>%
  left_join(., read_eaps, by = "ID_t") %>%
  left_join(., data_weights, by = "ID_t") %>%
  left_join(., data_merged[c("ID_t", "language")], by = "ID_t") %>% 
  mutate(
    .imp = as.numeric(.imp),
     agegr = case_when(age <= 34 ~ 0,
                       age > 34 & age <= 44 ~ 1,
                       age > 44 & age <= 54 ~ 2,
                       age > 54 ~ 3
      ),
    edugr = case_when(
      isced %in% c(0:5) ~ 0,
      isced %in% c(6:8) ~ 1,
      isced %in% c(9:10) ~ 2,
      TRUE ~ NA_real_),
      total = 1 # needed for selecting all observations in subsequent analysis
  ) %>%
  rename(
    t1_pv = PV_w3, t2_pv = PV_w9,
    t1_wle = wle_w3, t2_wle = wle_w9,
    t1_eap = eap_w3, t2_eap = eap_w9,
    weight = w_t456789_std, weight1 = w_t3_cal, weight2 = w_t9_cal
  ) %>%
  filter(!(is.na(t1_pv)  | is.na(t2_pv)) & language == 1)

math_neps <- math_pvs %>%
  get_pv_list() %>%
  set_names(1:n_pvs) %>%
  enframe(x = ., name = ".imp") %>%
  unnest(., value) %>%
  left_join(., math_wles, by = "ID_t") %>%
  left_join(., math_eaps, by = "ID_t") %>%
  left_join(., data_weights, by = "ID_t") %>% 
  left_join(., data_merged[c("ID_t", "language")], by = "ID_t") %>% 
  mutate(
    .imp = as.numeric(.imp),
     agegr = case_when(age <= 34 ~ 0,
                       age > 34 & age <= 44 ~ 1,
                       age > 44 & age <= 54 ~ 2,
                       age > 54 ~ 3
    ),
    edugr = case_when(
      isced %in% c(0:5) ~ 0,
      isced %in% c(6:8) ~ 1,
      isced %in% c(9:10) ~ 2,
      TRUE ~ NA_real_),
    total = 1 # needed for selecting all observations in subsequent analysis
  ) %>%
  rename(
    t1_pv = PV_w3, t2_pv = PV_w9,
    t1_wle = wle_w3, t2_wle = wle_w9,
    t1_eap = eap_w3, t2_eap = eap_w9,
    weight = w_t456789_std, weight1 = w_t3_cal, weight2 = w_t9_cal
  ) %>%
  filter(!(is.na(t1_pv)  | is.na(t2_pv)) & language == 1)


# Save the results --------------------------------------------------------

# Save the merged PV data for further analysis
save(list = c("reading_neps", "math_neps"), 
              file = file.path(dirs$results, "data_neps.Rda"))

# Save the raw PV file
save(list = c("read_pvs", "math_pvs"),
     file = file.path(dirs$results, "neps_pvs.Rda"))