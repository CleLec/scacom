# Project Info: ----------------------------------------------------------------
# Lechner et al. (2020). Stability and Change in Literacy and Numeracy
# Step 1: Data preparation
# This code was written by clemens.lechner@gesis.org
# R 4.0.2

# Basic settings ---------------------------------------------------------------

# List of directories
dirs <- list(
  main = "D:/Dropbox/Forschung und Lehre/Stability_PIAAC_NEPS",
  data = "C:/users/lechnecs/Desktop/NEPS SC6 (11-0-0)/SPSS/en/"
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

# Older version of haven is needed because the katest version creates issues
# when used in the NEPS PV tool
# install.packages("devtools")
# require(devtools)
# install_version("haven", version = "2.2.0",
#                repos = "http://cran.us.r-project.org")

# Installing the NEPS PV estimation tool
# install.packages("http://nocrypt.neps-data.de/r/nepsscaling_1.0.0.tar.gz",
#                 repos = NULL, type = "source")

pckloader(c(
  "tidyverse", "sjPlot", "xtable", "glue",
  "mvtnorm", "ucminf", "numDeriv", "rpart", "haven", "TAM"
))


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
  select(ID_t,
    yborn = tx2900y, gender = t700001, eduyrs = tx28102,
    casmin = tx28101, isced = tx28103, language = tx29003,
    income = t510010_g1, books,
    cumemp = tx29944, fases = t731453_g16, hhsize = t741001,
    math3 = maa3_sc1u, read3 = rea3_sc1u,
    math9 = maa9_sc1u, read9 = rea9_sc1u,
    ict = ica5_sc1, science = sca5_sc1,
    gf = dga7_sc3b
  ) %>%
  mutate(
    age = 2011 - yborn, age2 = age^2, age3 = age^3, eduyrs2 = eduyrs^2,
    edugr = if_else(isced %in% c(0:5), 0,
      if_else(isced %in% c(6:8), 1, 2)
    )
  ) %>%
  mutate(across(
    c(
      gender, edugr, language, casmin, # books - #books is optional
    ),
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
    edugr, language, hhsize, books,
    math3,  science, ict, #math9,
    gf, cumemp, fases
  ),
  npv = 20,
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
    ID_t, age, age2, age3, gender,
    edugr, language, hhsize, books,
    read3, science, ict, # read9, 
    gf, cumemp, fases
  ),
  npv = 20,
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

reading <- read_pvs %>%
  get_pv_list() %>%
  enframe(x = ., name = ".imp") %>%
  unnest(., value) %>%
  left_join(., read_wles, by = "ID_t") %>%
  left_join(., read_eaps, by = "ID_t") %>%
  mutate(agegr = if_else(age < 35, 0,
    if_else(age >= 35 & age < 45, 1,
      if_else(age >= 45 & age < 55, 2, 3)
    )
  ))

math <- math_pvs %>%
  get_pv_list() %>%
  enframe(x = ., name = ".imp") %>%
  unnest(., value) %>%
  left_join(., math_wles, by = "ID_t") %>%
  left_join(., math_eaps, by = "ID_t") %>%
  mutate(agegr = if_else(age < 35, 0,
    if_else(age >= 35 & age < 45, 1,
      if_else(age >= 45 & age < 55, 2, 3)
    )
  ))


# Save the PV files
save(math, file = "math.Rda")
save(reading, file = "reading.Rda")
