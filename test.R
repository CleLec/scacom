
rci_pooler <- function(grouping, group, domain) {
  data <- get(domain) %>%
    filter(!is.na(PV_w9) & !is.na(PV_w3))
  
  # Compute sds using the full sample
  sds <- get(domain) %>%
    filter(!is.na(PV_w9) & !is.na(PV_w3)) %>% 
    group_by(.imp) %>% 
    summarise(pooled = 0.5*(sd(PV_w3) + sd(PV_w9)),
              delta = sd(PV_w9 - PV_w3),
              t1 = sd(PV_w3)) %>% 
    summarise(across(everything(), mean))
  
  # Define function to compute RCI
  rci <- function(data) {
    data <- data %>% #group_by(.imp) %>%
      mutate(rci_raw = .data$PV_w9 - .data$PV_w3 /
               (sqrt(2*sds$t1 * sqrt((1 - rxx)^2))),
             rci_up = .data$rci_raw > 1.96,
             rci_down = .data$rci_raw < -1.96
      )
    data
  }

  # Add RCIs to each dataset in PV list
  pvlist <- data %>%
    filter(.data[[grouping]] == {{ group }}) %>%
    split(.$.imp) 

  sink("NUL")  
    # Compute rxx 
  rxx <- with(imputationList(pvlist), 
              lm(scale(PV_w9) ~ scale(PV_w3) )) %>%
    MIcombine() %>%
    summary() %>% 
    pluck("results", 2)
  
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
