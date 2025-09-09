#============================================================================================#
# Project: ALMP NMA                                                                          #
# Aim: Convert reported result from LATE to ITT then into standardised effect size           #
# Author: David Taylor                                                                       #
# Date: 09/09/2025                                                                           #
# Study ID: yin2023vocationalrehabilitationservices                                          #
#============================================================================================#

# load required packages
library(tidyverse)

# load custom functions
source("./es_transformation/code/effect_size_functions.R")

# read outcome data file
outcome_data_location <- "./es_transformation/inputs/almp_nma_outcome_data.rds"
outcome_data <- readRDS(outcome_data_location)

# prepare data for transformation
yin2023vocationalrehabilitationservices_outcome_data <- outcome_data |>
  filter(
    # filter data by study id
    study_id == "yin2023vocationalrehabilitationservices",
    # exclude outcomes with missing data
    is.na(exclude_missing_data) | exclude_missing_data != "Yes",
    # exclude outcomes that report duplicate constructs
    is.na(exclude_duplicate_construct) | exclude_duplicate_construct != "Yes",
    # exclude outcomes that have been deprioritised for other reasons (for example, they are not relevant to this analysis)
    is.na(deprioritised) | deprioritised != "Yes"
  ) |>
  # convert list vars to numeric
  mutate(
    across(
      c(
        outcome_timing,
        treatment_n,
        comparison_n,
        treatment_proportion,
        comparison_proportion,
        treatment_mean,
        treatment_sd,
        treatment_se,
        comparison_mean,
        comparison_sd,
        comparison_se,
        pooled_sd,
        odds_ratio,
        se,
        totaln,
        chisq,
        t,
        t_pvalue,
        treatment_effect,
        treatment_effect_se,
        treatment_effect_ci_low,
        treatment_effect_ci_high,
        treatment_effect_p_value
      ),
      convert_input_data_to_numeric
    )
  ) |>
  # round sample sizes to whole numbers
  mutate(
    treatment_n = round(treatment_n, 0),
    comparison_n = round(comparison_n, 0)
  )

## -----------------------------
## Yin et al. (2023) – LATE -> ITT -> SMD (d, g)
## -----------------------------

library(dplyr)
library(tibble)

# specify inputs from data frame
n1 <- yin2023vocationalrehabilitationservices_outcome_data$treatment_n[1]
n2 <- yin2023vocationalrehabilitationservices_outcome_data$comparison_n[1]


## Inputs from the paper (instrumented/LATE estimates, post-closure window)
## Employment is in percentage points (probability units); earnings in 2018 $.
late_p <- yin2023vocationalrehabilitationservices_outcome_data$treatment_effect[
  1
] # LATE on quarterly employment rate
late_p_se <- yin2023vocationalrehabilitationservices_outcome_data$treatment_effect_se[
  1
]
late_y <- yin2023vocationalrehabilitationservices_outcome_data$treatment_effect[
  2
] # LATE on quarterly earnings (2018 $)
late_y_se <- yin2023vocationalrehabilitationservices_outcome_data$treatment_effect_se[
  2
]

# specify additional information from paper
pi_c <- 0.36 # compliance rate (share on the margin)
pbar <- 0.435 # mean quarterly employment rate post-closure

# specify additional info
df <- n1 + n2 - 2 # degrees of freedom
J <- 1 - 3 / (4 * df - 1) # Hedges' correction

# convert LATE to ITT (standard: ITT = LATE * π_c; SE scales the same)
itt_p <- late_p * pi_c
itt_p_se <- late_p_se * pi_c

itt_y <- late_y * pi_c
itt_y_se <- late_y_se * pi_c

# Employed since baseline (binary rate)

## We standardise a risk-difference ITT by the Bernoulli SD at the sample mean:
## d = (Δp_ITT) / sqrt(pbar*(1-pbar)).
## SE(d) follows by the delta method: SE_d = SE(Δp_ITT) / sqrt(pbar*(1-pbar)).

## Cohen’s d
sd_bern <- sqrt(pbar * (1 - pbar))
d_emp <- itt_p / sd_bern
d_emp_se <- itt_p_se / sd_bern
d_emp_var <- d_emp_se^2

## apply small-sample correction
g_emp <- J * d_emp
g_emp_var <- (J^2) * d_emp_var
g_emp_se <- sqrt(g_emp_var)

# Quarterly earnings (continuous)

## Treat the ITT as a mean difference (Δy_ITT). We don’t have a reported pooled SD,
## so infer it from the ITT SE under equal-variance two-sample theory:
## SE(Δ) = sp * sqrt(1/n1 + 1/n2)  =>  sp = SE(Δ) / sqrt(1/n1 + 1/n2).

denom_md <- sqrt(1 / n1 + 1 / n2)
sp_earn <- itt_y_se / denom_md

## Cohen’s d
d_earn <- itt_y / sp_earn
d_earn_var <- (n1 + n2) / (n1 * n2) + d_earn^2 / (2 * df)
d_earn_se <- sqrt(d_earn_var)

## apply small-sample correction
g_earn <- J * d_earn
g_earn_var <- (J^2) * d_earn_var
g_earn_se <- sqrt(g_earn_var)

# prepare data for export
yin2023vocationalrehabilitationservices_export <-
  tibble::tribble(
    ~study_id,
    ~outcome_domain,
    ~outcome,
    ~outcome_source,
    ~favourable_direction,
    ~outcome_timing,
    ~estimand,
    ~intention_to_treat,
    ~conditional,
    ~d,
    ~d_se,
    ~d_var,
    ~g,
    ~g_se,
    ~g_var,

    yin2023vocationalrehabilitationservices_outcome_data$study_id[1],
    yin2023vocationalrehabilitationservices_outcome_data$outcome_domain[1],
    yin2023vocationalrehabilitationservices_outcome_data$outcome[1],
    yin2023vocationalrehabilitationservices_outcome_data$outcome_source[1],
    yin2023vocationalrehabilitationservices_outcome_data$favourable_direction[
      1
    ],
    yin2023vocationalrehabilitationservices_outcome_data$outcome_timing[1],
    yin2023vocationalrehabilitationservices_outcome_data$estimand[1],
    yin2023vocationalrehabilitationservices_outcome_data$intention_to_treat[1],
    yin2023vocationalrehabilitationservices_outcome_data$conditional[1],
    d_emp,
    d_emp_se,
    d_emp_var,
    g_emp,
    g_emp_se,
    g_emp_var,

    yin2023vocationalrehabilitationservices_outcome_data$study_id[2],
    yin2023vocationalrehabilitationservices_outcome_data$outcome_domain[2],
    yin2023vocationalrehabilitationservices_outcome_data$outcome[2],
    yin2023vocationalrehabilitationservices_outcome_data$outcome_source[2],
    yin2023vocationalrehabilitationservices_outcome_data$favourable_direction[
      2
    ],
    yin2023vocationalrehabilitationservices_outcome_data$outcome_timing[2],
    yin2023vocationalrehabilitationservices_outcome_data$estimand[2],
    yin2023vocationalrehabilitationservices_outcome_data$intention_to_treat[2],
    yin2023vocationalrehabilitationservices_outcome_data$conditional[2],
    d_earn,
    d_earn_se,
    d_earn_var,
    g_earn,
    g_earn_se,
    g_earn_var
  ) |>
  mutate(
    estimand = case_when(
      estimand == "Local Average Treatment Effect (LATE)" ~
        "Average Treatment Effect on the Treated (ATT)"
    )
  )

# export data
saveRDS(
  yin2023vocationalrehabilitationservices_export,
  "./es_transformation/output/yin2023vocationalrehabilitationservices.RDS"
)
