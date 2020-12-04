# Project Info: ----------------------------------------------------------------
# Lechner et al. (2020). Stability and Change in Literacy and Numeracy
# Step 3: Data preparation in PIAAC-L
# This code was written by clemens.lechner@gesis.org
# R 4.0.2

# Basic settings ---------------------------------------------------------------
#rm(list = ls())

# List of directories
dirs <- list(
  main = "D:/Dropbox/Forschung und Lehre/Stability_PIAAC_NEPS",
  data12 = "J:/Work/SCACOM/Daten/ZA5845_PIAAC_German_SUF_v2-2-0",
  data = "J:/Work/SCACOM/Daten/ZA5989_PIAAC-L_Welle3/ZA5989_PIAAC-L_v3-0-0/SPSS"
)

# Load requisite packages
library(tidyverse)
library(sjlabelled)
library(glue)

# Read data from SPSS distribution ----------------------------------------

# PIAAC 2012 - only needed for age and gender
piaac12 <- read_spss(glue(
  {
    dirs$data12
  },
  "/ZA5845_v2-2-0.sav"
)) %>%
  select(seqid = SEQID, age = AGE_R, gender = GENDER_R, weight12 = SPFWT0,
         hhsize = J_Q01_T2, language = NATIVESPEAKER)

# PIAAC-L 2012 - contains competence data, education, and other things
piaacl15 <- read_spss(glue(
  {
    dirs$data
  },
  "/ZA5989_Persons_15_v3-0-0.sav"
)) %>%
  filter(anchor_15 == 1) %>%
  select(seqid, pnrfestid, starts_with("PV"), 
    #  starts_with("assess"),
    edu = B_Q01a_15
  )

# Weights from 2015
weights15 <- read_spss(glue(
  {
    dirs$data
  },
  "/ZA5989_Weights_15_v3-0-0.sav"
)) %>%
  select(seqid, bleib_15, hrf_15)


# Merge and reshape data --------------------------------------------------

# Merge the data and keep only respondents who participated at both time points
# (i.e., have PVs at both time points)
piaacl <- piaac12 %>%
  left_join(., piaacl15, by = "seqid") %>%
  left_join(., weights15, by = "seqid") %>%
  filter(!is.na(PVLIT1_12_15) & !is.na(PVLIT1_15_15) &
           age >= 18)  %>%
  mutate(weight = weight12 * bleib_15 / mean(weight12 * bleib_15), #normalize wt
         agegr = case_when(
           age <= 34 ~ 0,
           age > 34 & age <= 44 ~ 1,
           age > 44 & age <= 54 ~ 2, 
           age > 54 ~ 3
           ),
         edugr = case_when(
                  edu %in% c(1:6) ~ 0,
                  edu %in% c(7:11) ~ 1,
                  edu %in% c(12:14) ~ 2, 
                  TRUE ~ NA_real_))

psych::describe(piaacl$weight)
rm(list = c("piaac12", "piaacl15", "weights15"))
# Reshape the data such that
# - data are in long format (person-period format) for the two waves
# - literacy and numeracy are separate files

# Create reshaped reading data
reading_piaac <- piaacl %>%
  select(-starts_with("PVNUM")) %>%
  pivot_longer(
    cols = matches("PVLIT(.+)_(1.)_15"),
    values_to = "score",
    names_to = c(".imp", "year"),
    names_pattern = "PVLIT(.+)_(.+)_15"
  ) %>%
  pivot_wider(.,
    values_from = c("score"),
    names_from = c("year")
  ) %>%
  relocate(.imp, before =  seqid) %>%
  rename(t1_pv = '12', t2_pv = '15') %>%
  mutate(total = 1, 
         .imp = as.numeric(.imp))   #needed for selecting all observations in the analyses
  

# Create reshaped math data
math_piaac <- piaacl %>%
  select(-starts_with("PVLIT")) %>%
  pivot_longer(
    cols = matches("PVNUM(.+)_(1.)_15"),
    values_to = "score",
    names_to = c(".imp", "year"),
    names_pattern = "PVNUM(.+)_(.+)_15"
  ) %>%
  pivot_wider(.,
              values_from = c("score"),
              names_from = c("year")
  ) %>%
  relocate(.imp, before =  seqid) %>%
  rename(t1_pv = '12', t2_pv = '15') %>%
  mutate(total = 1,   #needed for selecting all observations in the analyses
         .imp = as.numeric(.imp))


# Save the new data in .Rdata ---------------------------------------------
save(reading_piaac, file = "reading_piaac.Rda")
save(math_piaac, file = "math_piaac.Rda")
