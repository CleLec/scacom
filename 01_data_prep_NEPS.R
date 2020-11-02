# Project Info: ----------------------------------------------------------------
# Lechner et al. (2020). Stability and Change in Literacy and Numeracy
# Step 1: Data preparation
# This code was written by clemens.lechner@gesis.org
# R 4.0.2

# Basic settings ---------------------------------------------------------------
rm(list= ls())
# List of directories
dirs <- list(
  main = "D:/Dropbox/Forschung und Lehre/Stability_PIAAC_NEPS",
  data = "C:/users/lechnecs/Desktop/NEPS SC6 (11-0-0)/SPSS/en/"
)

#setwd(dirs$main)

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

# Older version of haven is needed because the katest version creates issues
# when used in the NEPS PV tool
# install.packages("devtools")
 #require(devtools)
 #install_version("haven", version = "2.2.0",
#                repos = "http://cran.us.r-project.org")

# Installing the NEPS PV estimation tool
 #install.packages("http://nocrypt.neps-data.de/r/nepsscaling_1.0.0.tar.gz",
#                 repos = NULL, type = "source")

library(tidyverse)
library(sjPlot)
library(sjlabe)
library(xtable)
library(glue)
library(mvtnorm)
library(ucminf)
library(numDeriv)
library(haven)
library(TAM)
 

# Preparing the data --------------------------------------------------------

data_basic <- sjlabelled::read_spss(
  atomic.to.fac = T,
  str_c(
    dirs$data,
    "SC6_Basics_D_11-0-0.sav"
  )
)

data_ptarget <- sjlabelled::read_spss(
  str_c(
    dirs$data,
    "SC6_pTarget_D_11-0-0.sav"
  )
)

data_methods <- sjlabelled::read_spss(
  str_c(
    dirs$data,
    "SC6_Methods_D_11-0-0.sav"
  )
) %>% 
filter(wave == 3) %>% 
select(ID_t, tx80101, tx80102)

#' - tx80101: federal state
#' - tx80102: BIK category (size of town)

data_books <- data_ptarget %>%
  select(ID_t, wave, t34005a) %>%
  arrange(ID_t, wave) %>%
  group_by(ID_t) %>%
  filter(t34005a %in% c(1:6)) %>%
  summarize(books = first(t34005a)) %>%
  ungroup() %>%
  mutate(books = if_else(books > 3, 1, 0))

data_comp <- sjlabelled::read_spss(
  str_c(dirs$data, "SC6_xTargetCompetencies_D_11-0-0.sav")
)

data_weights <- sjlabelled::read_spss(
  str_c(dirs$data, 
        "SC6_Weights_D_11-0-0.sav"
        )
) %>%
  select(ID_t, w_t456789_std, w_t3_cal, w_t9_cal)

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
    data_books,
    by = "ID_t"
  ) %>%
  full_join(.,
            data_methods,
            by = "ID_t"
  ) %>%
  select(ID_t, 
         mpa3re_sc5, mpa9re_sc5, mpa3ma_sc5, mpa9ma_sc5, #procedural metacognit.
    yborn = tx2900y, gender = t700001, eduyrs = tx28102,
    casmin = tx28101, isced = tx28103, language = tx29003,
    income = t510010_g1, books,
    cumemp = tx29944, fases = t731453_g16, hhsize = t741001,
    math3 = maa3_sc1u, read3 = rea3_sc1u,
    math9 = maa9_sc1u, read9 = rea9_sc1u,
    ict = ica5_sc1, science = sca5_sc1,
    gf = dga7_sc3b, state = tx80101, townsize = tx80102,
  ) %>%
  mutate(
    age = 2011 - yborn, age2 = age^2, age3 = age^3, eduyrs2 = eduyrs^2,
    edugr = if_else(isced %in% c(0:5), 0,
      if_else(isced %in% c(6:8), 1, 2)
    )
  ) %>%
  mutate(across(
    c(
      gender, edugr, language, casmin, isced, state, townsize), # categorical variables
    as_factor
  ))

# Estimating PVs for reading  -----------------------------------------------

library(NEPSscaling)

read_pvs <- plausible_values(
  SC = 6,
  wave = 3,
  path = dirs$data,
  domain = "RE",
  bgdata = select(
    data_merged,
    ID_t, age, age2, age3, gender,
    edugr, language, hhsize, books, income,
    state, townsize, cumemp, fases,
    math3, science, ict, gf, mpa3re_sc5, mpa9re_sc5
  ),
  npv = 30,
  longitudinal = TRUE,
  min_valid = 3,
  include_nr = FALSE,
  verbose = TRUE,
  control = list(WLE = TRUE, EAP = TRUE)
)


# Estimating PVs for Math -------------------------------------------------

math_pvs <- plausible_values(
  SC = 6,
  wave = 3,
  path = dirs$data,
  domain = "MA",
  bgdata = select(
    data_merged,
    ID_t, age, age2, age3, gender, income,
    edugr, language, hhsize, books,
    state, townsize, cumemp, fases,
    read3, science, ict, gf,  mpa3ma_sc5, mpa9ma_sc5,
  ),
  npv = 30,
  longitudinal = TRUE,
  min_valid = 3,
  include_nr = FALSE,
  verbose = TRUE,
  control = list(WLE = TRUE, EAP = TRUE)
)

# math_pvlist <- get_pv_list(math_pvs)
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
# math_wles %>% select(wle_w3, wle_w9) %>% cor(use = "pairwise")
# math_eaps %>% select(eap_w3, eap_w9) %>% cor(use = "pairwise")
# math_pvlist %>% select(PV_w3, PV_w9) %>% cor(use = "pairwise")

# Prepare data  -----------------------------------------------------

reading_neps <- read_pvs %>%
  get_pv_list() %>%
  enframe(x = ., name = ".imp") %>%
  unnest(., value) %>%
  left_join(., read_wles, by = "ID_t") %>%
  left_join(., read_eaps, by = "ID_t") %>%
  left_join(., data_weights, by = "ID_t") %>%
  mutate(
    agegr = if_else(age <= 34, 0,
                    if_else(age > 34 & age <= 44, 1,
                            if_else(age > 44 & age <= 54, 2, 3)
                    )
    ),
    total = 1 #needed for selecting all observations in subsequent analysis 
  ) %>%
  rename(t1_pv = PV_w3, t2_pv = PV_w9,
         t1_wle = wle_w3, t2_wle = wle_w9, 
         weight = w_t456789_std) %>% 
  filter(!is.na(t1_pv) & !is.na(t2_pv)) # drop incomplete respondents

math_neps <- math_pvs %>%
  get_pv_list() %>%
  enframe(x = ., name = ".imp") %>%
  unnest(., value) %>%
  left_join(., math_wles, by = "ID_t") %>%
  left_join(., math_eaps, by = "ID_t") %>%
  left_join(., data_weights, by = "ID_t") %>%
  mutate(
    agegr = if_else(age <= 34, 0,
                    if_else(age > 34 & age <= 44, 1,
                            if_else(age > 44 & age <= 54, 2, 3)
                    )
    ),
    total = 1 #needed for selecting all observations in subsequent analysis 
  ) %>%
  rename(t1_pv = PV_w3, t2_pv = PV_w9,
         t1_wle = wle_w3, t2_wle = wle_w9, 
         weight = w_t456789_std) %>% 
  filter(!is.na(t1_pv) & !is.na(t2_pv))# drop incomplete respondents

# Save the imputations
save(math_pvs, file = "math_pvs_neps.Rda")
save(read_pvs, file = "read_pvs_neps.Rda")
# Save the PV files
save(math_neps, file = "math_neps.Rda")
save(reading_neps, file = "reading_neps.Rda")
