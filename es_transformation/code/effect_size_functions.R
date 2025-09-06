#============================================================================================#
# Project: ALMP NMA                                                                          #
# Author: David Taylor                                                                       #
# Date: 05/09/2025                                                                           #
# Purpose: Functions for effect size transformation                                          #
#============================================================================================#

# load required functions
library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

# data cleaning functions

# 1. resusitate numeric variables from whatever torture Google Sheets applied to them

convert_input_data_to_numeric <- function(x) {
  if (is.list(x)) {
    map_dbl(
      x,
      ~ ifelse(is.null(.x) || length(.x) == 0, NA_real_, as.numeric(.x[1]))
    )
  } else {
    as.numeric(x)
  }
}

# 1. convert results reported as binary proportions to Hedges' g

proportion_to_smd <- function(
  treatment_n,
  comparison_n,
  treatment_proportion,
  comparison_proportion,
  method = c("cox_logit", "logit", "probit"),
  mask = NULL
) {
  method <- match.arg(method)
  n1 <- treatment_n
  n2 <- comparison_n
  p1 <- treatment_proportion
  p2 <- comparison_proportion

  # mask: rows to compute; others set to NA
  if (is.null(mask)) {
    mask <- rep(TRUE, length(n1))
  }

  # initialise NA vectors
  d <- d_var <- d_se <- g <- g_var <- g_se <- rep(NA_real_, length(n1))

  # compute only where mask == TRUE (and all needed inputs are present)
  ok <- mask & !is.na(n1) & !is.na(n2) & !is.na(p1) & !is.na(p2)

  if (any(ok)) {
    i <- which(ok)

    df <- n1[i] + n2[i] - 2
    J <- 1 - 3 / (4 * df - 1)

    if (method %in% c("logit", "cox_logit")) {
      a <- p1[i] * n1[i]
      b <- (1 - p1[i]) * n1[i]
      c <- p2[i] * n2[i]
      d_ <- (1 - p2[i]) * n2[i]

      # continuity correction where any cell == 0
      zero <- (a == 0 | b == 0 | c == 0 | d_ == 0)
      a[zero] <- a[zero] + 0.5
      b[zero] <- b[zero] + 0.5
      c[zero] <- c[zero] + 0.5
      d_[zero] <- d_[zero] + 0.5

      lnOR <- log((a * d_) / (b * c))
      v_lnOR <- 1 / a + 1 / b + 1 / c + 1 / d_

      if (method == "logit") {
        d[i] <- lnOR * sqrt(3) / pi
        d_var[i] <- v_lnOR * (3 / pi^2)
      } else {
        # cox_logit
        d[i] <- lnOR / 1.65
        d_var[i] <- v_lnOR / (1.65^2)
      }
    } else {
      # probit
      z1 <- qnorm(p1[i])
      z2 <- qnorm(p2[i])
      d[i] <- z1 - z2
      d_var[i] <- (2 * pi * p1[i] * (1 - p1[i]) * exp(z1^2)) /
        n1[i] +
        (2 * pi * p2[i] * (1 - p2[i]) * exp(z2^2)) / n2[i]
    }

    d_se[i] <- sqrt(d_var[i])
    g[i] <- J * d[i]
    g_var[i] <- (J^2) * d_var[i]
    g_se[i] <- sqrt(g_var[i])
  }

  # return a named list of vectors -> can be spliced with !!!
  list(
    d = d,
    d_se = d_se,
    d_var = d_var,
    g = g,
    g_se = g_se,
    g_var = g_var
  )
}
