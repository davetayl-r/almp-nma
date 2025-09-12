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

#-------------------------------------------------------------------------------
# 1. Data cleaning functions
#-------------------------------------------------------------------------------

# resusitate numeric variables from whatever torture Google Sheets applied to them
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

#-------------------------------------------------------------------------------
# 2. Effect size transformation functions
#-------------------------------------------------------------------------------

# convert results reported as binary proportions to SMD
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

# convert results reported as means and standard deviation to SMD
mean_sd_to_smd <- function(
  treatment_n,
  comparison_n,
  treatment_mean,
  comparison_mean,
  treatment_sd,
  comparison_sd,
  mask = NULL
) {
  n1 <- treatment_n
  n2 <- comparison_n
  m1 <- treatment_mean
  m2 <- comparison_mean
  s1 <- treatment_sd
  s2 <- comparison_sd

  if (is.null(mask)) {
    mask <- rep(TRUE, length(n1))
  }

  d <- d_var <- d_se <- g <- g_var <- g_se <- rep(NA_real_, length(n1))
  ok <- mask &
    is.finite(n1) &
    is.finite(n2) &
    is.finite(m1) &
    is.finite(m2) &
    is.finite(s1) &
    is.finite(s2)

  if (any(ok)) {
    i <- which(ok)
    df <- n1[i] + n2[i] - 2
    sp <- sqrt(((n1[i] - 1) * s1[i]^2 + (n2[i] - 1) * s2[i]^2) / df)

    d[i] <- (m1[i] - m2[i]) / sp
    # Hedges & Olkin var(d)
    d_var[i] <- (n1[i] + n2[i]) / (n1[i] * n2[i]) + d[i]^2 / (2 * df)
    d_se[i] <- sqrt(d_var[i])
    # small-sample correction
    J <- 1 - 3 / (4 * df - 1)
    g[i] <- J * d[i]
    g_var[i] <- (J^2) * d_var[i]
    g_se[i] <- sqrt(g_var[i])
  }

  list(d = d, d_se = d_se, d_var = d_var, g = g, g_se = g_se, g_var = g_var)
}

# convert results reported as means and pooled standard deviation to SMD
mean_pooled_sd_to_smd <- function(
  treatment_n,
  comparison_n,
  treatment_mean,
  comparison_mean,
  pooled_sd,
  mask = NULL
) {
  n1 <- treatment_n
  n2 <- comparison_n
  m1 <- treatment_mean
  m2 <- comparison_mean
  sp <- pooled_sd

  if (is.null(mask)) {
    mask <- rep(TRUE, length(n1))
  }

  d <- d_var <- d_se <- g <- g_var <- g_se <- rep(NA_real_, length(n1))

  ok <- mask &
    is.finite(n1) &
    is.finite(n2) &
    is.finite(m1) &
    is.finite(m2) &
    is.finite(sp) &
    sp > 0

  if (any(ok)) {
    i <- which(ok)
    df <- n1[i] + n2[i] - 2

    # Cohen's d using the provided pooled SD
    d[i] <- (m1[i] - m2[i]) / sp[i]

    # Hedges & Olkin variance
    d_var[i] <- (n1[i] + n2[i]) / (n1[i] * n2[i]) + d[i]^2 / (2 * df)
    d_se[i] <- sqrt(d_var[i])

    # Small-sample correction
    J <- 1 - 3 / (4 * df - 1)
    g[i] <- J * d[i]
    g_var[i] <- (J^2) * d_var[i]
    g_se[i] <- sqrt(g_var[i])
  }

  list(
    d = d,
    d_se = d_se,
    d_var = d_var,
    g = g,
    g_se = g_se,
    g_var = g_var
  )
}

# convert results reported as means and standard error to SMD
mean_se_to_smd <- function(
  treatment_n,
  comparison_n,
  treatment_mean,
  comparison_mean,
  treatment_se,
  comparison_se,
  mask = NULL
) {
  n1 <- treatment_n
  n2 <- comparison_n
  m1 <- treatment_mean
  m2 <- comparison_mean
  se1 <- treatment_se
  se2 <- comparison_se

  if (is.null(mask)) {
    mask <- rep(TRUE, length(n1))
  }

  d <- d_var <- d_se <- g <- g_var <- g_se <- rep(NA_real_, length(n1))
  ok <- mask &
    is.finite(n1) &
    is.finite(n2) &
    is.finite(m1) &
    is.finite(m2) &
    is.finite(se1) &
    is.finite(se2)

  if (any(ok)) {
    i <- which(ok)
    sd1 <- se1[i] * sqrt(n1[i])
    sd2 <- se2[i] * sqrt(n2[i])

    df <- n1[i] + n2[i] - 2
    sp <- sqrt(((n1[i] - 1) * sd1^2 + (n2[i] - 1) * sd2^2) / df)

    d[i] <- (m1[i] - m2[i]) / sp
    d_var[i] <- (n1[i] + n2[i]) / (n1[i] * n2[i]) + d[i]^2 / (2 * df)
    d_se[i] <- sqrt(d_var[i])

    J <- 1 - 3 / (4 * df - 1)
    g[i] <- J * d[i]
    g_var[i] <- (J^2) * d_var[i]
    g_se[i] <- sqrt(g_var[i])
  }

  list(d = d, d_se = d_se, d_var = d_var, g = g, g_se = g_se, g_var = g_var)
}

# convert results reported as t-value to SMD
t_value_to_smd <- function(
  t_value,
  treatment_n,
  comparison_n,
  mask = NULL
) {
  tval <- t_value
  n1 <- treatment_n
  n2 <- comparison_n

  if (is.null(mask)) {
    mask <- rep(TRUE, length(n1))
  }

  d <- d_var <- d_se <- g <- g_var <- g_se <- rep(NA_real_, length(n1))
  ok <- mask & is.finite(tval) & is.finite(n1) & is.finite(n2)

  if (any(ok)) {
    i <- which(ok)
    df <- n1[i] + n2[i] - 2

    # d = t * sqrt(1/n1 + 1/n2)
    d[i] <- tval[i] * sqrt(1 / n1[i] + 1 / n2[i])
    d_var[i] <- (n1[i] + n2[i]) / (n1[i] * n2[i]) + d[i]^2 / (2 * df)
    d_se[i] <- sqrt(d_var[i])

    J <- 1 - 3 / (4 * df - 1)
    g[i] <- J * d[i]
    g_var[i] <- (J^2) * d_var[i]
    g_se[i] <- sqrt(g_var[i])
  }

  list(d = d, d_se = d_se, d_var = d_var, g = g, g_se = g_se, g_var = g_var)
}

# convert results reported as an odds ratio to SMD
or_to_smd <- function(
  odds_ratio,
  var_or = NULL,
  se_or = NULL,
  or_is_log = FALSE,
  se_is_log = TRUE,
  ci_low = NULL,
  ci_high = NULL,
  ci_level = 0.95,
  ci_is_log = FALSE,
  a = NULL,
  b = NULL,
  c = NULL,
  d = NULL,
  method = c("cox_logit", "logit"),
  mask = NULL,
  n1 = NULL,
  n2 = NULL
) {
  method <- match.arg(method)

  OR_in <- odds_ratio
  k <- length(OR_in)
  if (is.null(mask)) {
    mask <- rep(TRUE, k)
  }

  # outputs
  d <- d_var <- d_se <- g <- g_var <- g_se <- rep(NA_real_, k)

  # containers for lnOR and its variance
  lnOR <- rep(NA_real_, k)
  v_lnOR <- rep(NA_real_, k)

  # when main effect is on log scale
  ok_or <- mask & is.finite(OR_in)

  if (any(ok_or)) {
    if (or_is_log) {
      lnOR[ok_or] <- OR_in[ok_or]
    } else {
      lnOR[ok_or] <- log(OR_in[ok_or])
    }
  }

  # helper to get OR if required
  get_OR <- function(idx) if (or_is_log) exp(OR_in[idx]) else OR_in[idx]

  # variance/SE inputs
  if (!is.null(var_or)) {
    idx <- mask & is.finite(var_or)
    if (any(idx)) {
      if (se_is_log) {
        v_lnOR[idx] <- var_or[idx]
      } else {
        OR_here <- get_OR(idx)
        # use delta method to estimate SE
        v_lnOR[idx] <- var_or[idx] / (OR_here^2)
      }
    }
  }

  if (!is.null(se_or)) {
    idx <- mask & is.finite(se_or)
    if (any(idx)) {
      if (se_is_log) {
        v_lnOR[idx] <- (se_or[idx])^2
      } else {
        OR_here <- get_OR(idx)
        # use delta method to estimate SE
        v_lnOR[idx] <- (se_or[idx] / OR_here)^2
      }
    }
  }

  # confidence interval inputs
  if (!is.null(ci_low) && !is.null(ci_high)) {
    idx <- mask & is.finite(ci_low) & is.finite(ci_high)
    if (any(idx)) {
      z <- qnorm(1 - (1 - ci_level) / 2)
      if (ci_is_log) {
        se_from_ci <- (ci_high[idx] - ci_low[idx]) / (2 * z)
        v_lnOR[idx] <- se_from_ci^2
        # fill lnOR if missing: midpoint on log scale
        need_ln <- is.na(lnOR[idx])
        if (any(need_ln)) {
          lnOR[idx][need_ln] <- (ci_high[idx][need_ln] + ci_low[idx][need_ln]) /
            2
        }
      } else {
        se_from_ci <- (log(ci_high[idx]) - log(ci_low[idx])) / (2 * z)
        v_lnOR[idx] <- se_from_ci^2
        need_ln <- is.na(lnOR[idx])
        if (any(need_ln)) {
          lnOR[idx][need_ln] <- (log(ci_high[idx][need_ln]) +
            log(ci_low[idx][need_ln])) /
            2
        }
      }
    }
  }

  # from counts (continuity correction if any zero)
  if (!is.null(a) && !is.null(b) && !is.null(c) && !is.null(d)) {
    idx <- mask & is.finite(a) & is.finite(b) & is.finite(c) & is.finite(d)
    if (any(idx)) {
      ai <- a[idx]
      bi <- b[idx]
      ci_ <- c[idx]
      di <- d[idx]
      zero <- (ai == 0 | bi == 0 | ci_ == 0 | di == 0)
      ai[zero] <- ai[zero] + 0.5
      bi[zero] <- bi[zero] + 0.5
      ci_[zero] <- ci_[zero] + 0.5
      di[zero] <- di[zero] + 0.5
      v_lnOR[idx] <- 1 / ai + 1 / bi + 1 / ci_ + 1 / di
      need_ln <- is.na(lnOR[idx])
      if (any(need_ln)) {
        lnOR[idx][need_ln] <- log(
          (ai[need_ln] * di[need_ln]) / (bi[need_ln] * ci_[need_ln])
        )
      }
    }
  }

  # rows where both lnOR and v_lnOR are known
  ok <- mask & is.finite(lnOR) & is.finite(v_lnOR)

  if (any(ok)) {
    i <- which(ok)

    if (method == "cox_logit") {
      d[i] <- lnOR[i] / 1.65
      d_var[i] <- v_lnOR[i] / (1.65^2)
    } else {
      # "logit"
      d[i] <- lnOR[i] * sqrt(3) / pi
      d_var[i] <- v_lnOR[i] * (3 / pi^2)
    }

    d_se[i] <- sqrt(d_var[i])

    # Hedges' g — apply small-sample J if n1, n2 available; else g=d
    if (!is.null(n1) && !is.null(n2)) {
      df <- n1 + n2 - 2
      J <- 1 - 3 / (4 * df - 1)
      g[i] <- J * d[i]
      g_var[i] <- (J^2) * d_var[i]
      g_se[i] <- sqrt(g_var[i])
    } else {
      g <- d
      g_var <- d_var
      g_se <- d_se
    }
  }

  list(d = d, d_se = d_se, d_var = d_var, g = g, g_se = g_se, g_var = g_var)
}

# convert results reported as a binary treatment effect (risk difference) to SMD
treatment_effect_binary_to_smd <- function(
  treatment_n,
  comparison_n,
  treatment_effect,
  treatment_effect_se,
  mask = NULL
) {
  n1 <- treatment_n
  n2 <- comparison_n
  att <- treatment_effect

  if (is.null(mask)) {
    mask <- rep(TRUE, length(n1))
  }

  k <- length(n1)
  d <- d_var <- d_se <- g <- g_var <- g_se <- rep(NA_real_, k)

  # derive standard error from reported information if required
  se <- rep(NA_real_, k)
  if (!is.null(treatment_effect_se)) {
    se <- treatment_effect_se
  }

  ok <- mask &
    is.finite(n1) &
    n1 > 0 &
    is.finite(n2) &
    n2 > 0 &
    is.finite(att) &
    abs(att) <= 1 &
    is.finite(se) &
    se >= 0

  if (any(ok)) {
    for (ii in which(ok)) {
      a <- att[ii]
      v <- se[ii]^2

      # Solve for pC in [L,U] with pT = pC + a
      # v = ((pC+a)(1 - pC - a))/n1 + (pC(1 - pC))/n2
      A <- -(1 / n1[ii] + 1 / n2[ii])
      B <- ((1 - a) / n1[ii] + 1 / n2[ii])
      C <- (a * (1 - a)) / n1[ii] - v

      disc <- B^2 - 4 * A * C
      if (disc < 0) {
        next
      }

      roots <- c((-B + sqrt(disc)) / (2 * A), (-B - sqrt(disc)) / (2 * A))

      # admissible region: 0 <= pC <= 1, 0 <= pT = pC + a <= 1
      L <- max(0, -a)
      U <- min(1, 1 - a)

      candidates <- roots[roots >= L & roots <= U]
      if (length(candidates) == 0) {
        next
      }

      # pick candidate closest to midpoint of feasible interval
      pC <- candidates[which.min(abs(candidates - (L + U) / 2))]
      pT <- pC + a
      if (!is.finite(pC) || !is.finite(pT)) {
        next
      }

      # pooled SD from Bernoulli variances
      s1_sq <- pT * (1 - pT)
      s2_sq <- pC * (1 - pC)
      df <- n1[ii] + n2[ii] - 2
      sp <- sqrt(((n1[ii] - 1) * s1_sq + (n2[ii] - 1) * s2_sq) / df)
      if (!is.finite(sp) || sp <= 0) {
        next
      }

      d[ii] <- a / sp
      d_var[ii] <- (n1[ii] + n2[ii]) / (n1[ii] * n2[ii]) + d[ii]^2 / (2 * df)
      d_se[ii] <- sqrt(d_var[ii])

      J <- 1 - 3 / (4 * df - 1)
      g[ii] <- J * d[ii]
      g_var[ii] <- (J^2) * d_var[ii]
      g_se[ii] <- sqrt(g_var[ii])
    }
  }

  list(d = d, d_se = d_se, d_var = d_var, g = g, g_se = g_se, g_var = g_var)
}

# convert results reported as a continuous treatment effect to SMD
treatment_effect_continuous_to_smd <- function(
  treatment_n,
  comparison_n,
  treatment_effect,
  # pooled SD is preferred by the function
  pooled_sd,
  # SE for TE if no pooled SD is reported
  treatment_effect_se = NULL,
  mask = NULL
) {
  n1 <- treatment_n
  n2 <- comparison_n
  md <- treatment_effect
  sp_reported <- pooled_sd

  # robust mask handling — idea is to prevent NA/length issues when applying functiom across rows
  k <- length(n1)
  if (is.null(mask)) {
    mask <- rep(TRUE, k)
  } else {
    mask <- as.logical(mask)
    if (length(mask) != k) {
      mask <- rep_len(mask, k)
    }
    mask[is.na(mask)] <- FALSE
  }

  d <- d_var <- d_se <- g <- g_var <- g_se <- rep(NA_real_, k)

  # derive SE if needed (already precomputed upstream is preferred)
  se_md <- rep(NA_real_, k)
  if (!is.null(treatment_effect_se)) {
    se_md <- treatment_effect_se
  }

  # choose pooled SD: reported if valid, else derived from SE(Δ) under equal-variance assumption
  denom <- sqrt(1 / n1 + 1 / n2)
  denom_ok <- is.finite(denom) & denom > 0
  sp_derived <- ifelse(denom_ok, abs(se_md) / denom, NA_real_)
  use_reported <- dplyr::coalesce(
    is.finite(sp_reported) & (sp_reported > 0),
    FALSE
  )
  sp <- ifelse(use_reported, sp_reported, sp_derived)

  # allow md to be NA or 0, provided we can compute sp
  md_available_or_zero <- is.finite(md) | is.na(md)
  ok <- mask &
    is.finite(n1) &
    n1 > 1 &
    is.finite(n2) &
    n2 > 1 &
    md_available_or_zero &
    is.finite(sp) &
    sp > 0

  if (any(ok)) {
    i <- which(ok)
    df <- n1[i] + n2[i] - 2

    # If md is NA, treat as 0 (only for rows where SD is computable)
    md_used <- md[i]
    md_used[!is.finite(md_used)] <- 0

    # Cohen's d from (possibly zero) MD and pooled SD
    d[i] <- md_used / sp[i]
    d_var[i] <- (n1[i] + n2[i]) / (n1[i] * n2[i]) + d[i]^2 / (2 * df)
    d_se[i] <- sqrt(d_var[i])

    # Hedges' g small-sample correction
    J <- 1 - 3 / (4 * df - 1)
    g[i] <- J * d[i]
    g_var[i] <- (J^2) * d_var[i]
    g_se[i] <- sqrt(g_var[i])
  }

  list(
    d = d,
    d_se = d_se,
    d_var = d_var,
    g = g,
    g_se = g_se,
    g_var = g_var
  )
}

# estimate treatment effect se from reported p values or confidence intervals
derive_treatment_effect_se <- function(
  dat,
  ci_level = 0.95,
  p_is_two_sided = TRUE,
  assumed_dp = 2, # <- NEW: assume TE printed to k dp when exact string isn't available
  zero_te_tol = 1e-12 # <- NEW: tolerance to treat TE as "effectively zero"
) {
  z_ci <- qnorm(1 - (1 - ci_level) / 2)

  # epsilon implied by reporting precision: 0.5 * 10^-k
  eps_effect <- 0.5 * 10^(-assumed_dp)

  dat %>%
    dplyr::mutate(
      treatment_effect_se = {
        in_scope <- esc_type %in%
          c("Treatment Effect (Continuous)", "Treatment Effect (Binary)")

        has_reported_se <- in_scope & is.finite(treatment_effect_se)
        need_se <- in_scope & !has_reported_se

        # p-value route --------------------------------------------------------
        p_is_valid <- need_se &
          is.finite(treatment_effect) &
          is.finite(treatment_effect_p_value) &
          dplyr::between(treatment_effect_p_value, .Machine$double.eps, 1)

        p_comp <- if (p_is_two_sided) {
          1 - treatment_effect_p_value / 2
        } else {
          1 - treatment_effect_p_value
        }
        p_comp <- pmin(
          pmax(p_comp, .Machine$double.eps),
          1 - .Machine$double.eps
        )
        z_from_p <- qnorm(p_comp)

        # standard route (non-zero effect)
        se_from_p_std <- ifelse(
          p_is_valid &
            abs(treatment_effect) > zero_te_tol &
            is.finite(z_from_p) &
            z_from_p != 0,
          abs(treatment_effect) / z_from_p,
          NA_real_
        )

        # fallback route for printed-zero effects: use epsilon inferred from dp
        se_from_p_eps <- ifelse(
          p_is_valid &
            (abs(treatment_effect) <= zero_te_tol) &
            is.finite(z_from_p) &
            z_from_p != 0,
          eps_effect / z_from_p, # upper-bound SE (conservative)
          NA_real_
        )

        se_from_p <- dplyr::coalesce(se_from_p_std, se_from_p_eps)

        # CI route -------------------------------------------------------------
        ci_is_valid <- need_se &
          is.finite(treatment_effect_ci_low) &
          is.finite(treatment_effect_ci_high)

        se_from_ci <- ifelse(
          ci_is_valid,
          (treatment_effect_ci_high - treatment_effect_ci_low) / (2 * z_ci),
          NA_real_
        )

        # final: prefer reported, then p-value (std/eps), then CI
        ifelse(
          need_se,
          dplyr::coalesce(se_from_p, se_from_ci, treatment_effect_se),
          treatment_effect_se
        )
      }
    )
}

# estimate treatment effect se from reported p values or confidence intervals
derive_treatment_effect_se <- function(
  dat,
  ci_level = 0.95,
  p_is_two_sided = TRUE,
  # assume TE printed to k dp when exact string isn't available
  assumed_dp = 2,
  # tolerance to treat TE as "effectively zero"
  zero_te_tol = 1e-12
) {
  z_ci <- qnorm(1 - (1 - ci_level) / 2)

  # epsilon implied by reporting precision: 0.5 * 10^-k
  eps_effect <- 0.5 * 10^(-assumed_dp)

  dat %>%
    dplyr::mutate(
      treatment_effect_se = {
        in_scope <- esc_type %in%
          c("Treatment Effect (Continuous)", "Treatment Effect (Binary)")

        has_reported_se <- in_scope & is.finite(treatment_effect_se)
        need_se <- in_scope & !has_reported_se

        # p-value route
        p_is_valid <- need_se &
          is.finite(treatment_effect) &
          is.finite(treatment_effect_p_value) &
          dplyr::between(treatment_effect_p_value, .Machine$double.eps, 1)

        p_comp <- if (p_is_two_sided) {
          1 - treatment_effect_p_value / 2
        } else {
          1 - treatment_effect_p_value
        }
        p_comp <- pmin(
          pmax(p_comp, .Machine$double.eps),
          1 - .Machine$double.eps
        )
        z_from_p <- qnorm(p_comp)

        # standard route (non-zero effect)
        se_from_p_std <- ifelse(
          p_is_valid &
            abs(treatment_effect) > zero_te_tol &
            is.finite(z_from_p) &
            z_from_p != 0,
          abs(treatment_effect) / z_from_p,
          NA_real_
        )

        # fallback route for printed-zero effects: use epsilon inferred from dp
        se_from_p_eps <- ifelse(
          p_is_valid &
            (abs(treatment_effect) <= zero_te_tol) &
            is.finite(z_from_p) &
            z_from_p != 0,
          eps_effect / z_from_p, # upper-bound SE (conservative)
          NA_real_
        )

        se_from_p <- dplyr::coalesce(se_from_p_std, se_from_p_eps)

        # CI route
        ci_is_valid <- need_se &
          is.finite(treatment_effect_ci_low) &
          is.finite(treatment_effect_ci_high)

        se_from_ci <- ifelse(
          ci_is_valid,
          (treatment_effect_ci_high - treatment_effect_ci_low) / (2 * z_ci),
          NA_real_
        )

        # final: prefer reported, then p-value (std/eps), then CI
        ifelse(
          need_se,
          dplyr::coalesce(se_from_p, se_from_ci, treatment_effect_se),
          treatment_effect_se
        )
      }
    )
}

#-------------------------------------------------------------------------------
# 3. Pool studies across subgroups
#-------------------------------------------------------------------------------

pool_studies <- function(dat, study_ids, output_study_id) {
  # Requires: dplyr, stringr
  # - dat: data.frame/tibble with your ALMP NMA schema
  # - study_ids: character vector of base IDs
  # - output_study_id: single character; becomes the final pooled study_id

  # helper function
  wmean_safe <- function(x, w) {
    keep <- is.finite(x) & is.finite(w)
    if (!any(keep)) {
      return(NA_real_)
    }
    if (sum(w[keep], na.rm = TRUE) <= 0) {
      return(NA_real_)
    }
    stats::weighted.mean(x[keep], w = w[keep], na.rm = TRUE)
  }

  # identify rows to pool: match either exact ID or base ID (suffix "_[a-z]" dropped)
  rows_to_pool <- dat %>%
    dplyr::mutate(
      base_from_data = stringr::str_remove(.data$study_id, "_[a-z]$")
    ) %>%
    dplyr::filter(
      .data$study_id %in% study_ids | .data$base_from_data %in% study_ids
    ) %>%
    dplyr::select(-.data$base_from_data)

  if (nrow(rows_to_pool) == 0L) {
    return(dplyr::tibble()) # nothing to pool
  }

  # standardise base id to the requested output id and create per-row weights
  dat_sub <- rows_to_pool %>%
    dplyr::mutate(
      base_study_id = output_study_id,
      weight_d = 1 / .data$d_var,
      weight_g = 1 / .data$g_var,
      w_treat = dplyr::coalesce(.data$treatment_n, 0),
      w_comp = dplyr::coalesce(.data$comparison_n, 0),
      w_total = .data$w_treat + .data$w_comp,
      study_age_mean_filled = dplyr::coalesce(
        .data$study_age_mean,
        dplyr::if_else(
          is.finite(.data$study_age_min) & is.finite(.data$study_age_max),
          (.data$study_age_min + .data$study_age_max) / 2,
          NA_real_
        )
      )
    )

  # constant per-study female proportion (treatment-n weighted across ALL rows)
  female_props <- dat_sub %>%
    dplyr::group_by(.data$base_study_id) %>%
    dplyr::summarise(
      proportion_female_treatment_study = wmean_safe(
        .data$proportion_female_treatment,
        .data$w_treat
      ),
      .groups = "drop"
    )

  # pool within (study, outcome_domain, outcome, outcome_timing)
  pooled <- dat_sub %>%
    dplyr::left_join(female_props, by = "base_study_id") %>%
    dplyr::group_by(
      .data$base_study_id,
      .data$outcome_domain,
      .data$outcome,
      .data$outcome_timing
    ) %>%
    dplyr::summarise(
      # inverse-variance pooled effects
      d = sum(.data$d * .data$weight_d, na.rm = TRUE) /
        sum(.data$weight_d, na.rm = TRUE),
      g = sum(.data$g * .data$weight_g, na.rm = TRUE) /
        sum(.data$weight_g, na.rm = TRUE),
      d_se = sqrt(1 / sum(.data$weight_d, na.rm = TRUE)),
      g_se = sqrt(1 / sum(.data$weight_g, na.rm = TRUE)),
      d_var = 1 / sum(.data$weight_d, na.rm = TRUE),
      g_var = 1 / sum(.data$weight_g, na.rm = TRUE),

      # sample sizes
      treatment_n = sum(.data$w_treat, na.rm = TRUE),
      comparison_n = sum(.data$w_comp, na.rm = TRUE),

      # subgroup-sensitive descriptors
      proportion_female_treatment = dplyr::first(
        .data$proportion_female_treatment_study
      ),
      study_age_min = {
        m <- suppressWarnings(min(.data$study_age_min, na.rm = TRUE))
        if (is.infinite(m)) NA_real_ else m
      },
      study_age_max = {
        M <- suppressWarnings(max(.data$study_age_max, na.rm = TRUE))
        if (is.infinite(M)) NA_real_ else M
      },
      study_age_mean = {
        wmean_safe(.data$study_age_mean_filled, .data$w_total)
      },

      # invariant-ish metadata
      outcome_source = dplyr::first(.data$outcome_source),
      favourable_direction = dplyr::first(.data$favourable_direction),
      estimand = dplyr::first(.data$estimand),
      intention_to_treat = dplyr::first(.data$intention_to_treat),
      conditional = dplyr::first(.data$conditional),
      year_published = dplyr::first(.data$year_published),
      source = dplyr::first(.data$source),
      peer_reviewed = dplyr::first(.data$peer_reviewed),
      study_design_type = dplyr::first(.data$study_design_type),
      study_design_detail = dplyr::first(.data$study_design_detail),
      location = dplyr::first(.data$location),
      year_start = dplyr::first(.data$year_start),
      intervention_length_n = dplyr::first(.data$intervention_length_n),
      intervention_intensity_n = dplyr::first(.data$intervention_intensity_n),
      youth_focused = dplyr::first(.data$youth_focused),
      intervention_developer = dplyr::first(.data$intervention_developer),
      intervention_implementor = dplyr::first(.data$intervention_implementor),
      program_scope = dplyr::first(.data$program_scope),
      intervention_funding = dplyr::first(.data$intervention_funding),
      conditionality = dplyr::first(.data$conditionality),
      incentives = dplyr::first(.data$incentives),
      certification = dplyr::first(.data$certification),
      study_funding = dplyr::first(.data$study_funding),
      evaluator = dplyr::first(.data$evaluator),
      int_basic_skills_training = dplyr::first(.data$int_basic_skills_training),
      int_soft_skills_training = dplyr::first(.data$int_soft_skills_training),
      int_behavioural_skills_training = dplyr::first(
        .data$int_behavioural_skills_training
      ),
      int_job_specific_technical_skills_off_job_training = dplyr::first(
        .data$int_job_specific_technical_skills_off_job_training
      ),
      int_business_skills_training = dplyr::first(
        .data$int_business_skills_training
      ),
      int_business_advisory_and_mentoring = dplyr::first(
        .data$int_business_advisory_and_mentoring
      ),
      int_financial_and_start_up_support = dplyr::first(
        .data$int_financial_and_start_up_support
      ),
      int_job_search_preparation = dplyr::first(
        .data$int_job_search_preparation
      ),
      int_job_search_assistance = dplyr::first(.data$int_job_search_assistance),
      int_employment_counselling = dplyr::first(
        .data$int_employment_counselling
      ),
      int_employment_coaching = dplyr::first(.data$int_employment_coaching),
      int_financial_assistance = dplyr::first(.data$int_financial_assistance),
      int_job_specific_technical_skills_on_job_training = dplyr::first(
        .data$int_job_specific_technical_skills_on_job_training
      ),
      int_paid_temporary_work_experience = dplyr::first(
        .data$int_paid_temporary_work_experience
      ),
      int_unpaid_temporary_work_experience = dplyr::first(
        .data$int_unpaid_temporary_work_experience
      ),
      int_wage_subsidies = dplyr::first(.data$int_wage_subsidies),
      int_public_works = dplyr::first(.data$int_public_works),
      int_other_active_component_nec = dplyr::first(
        .data$int_other_active_component_nec
      ),
      com_services_as_usual = dplyr::first(.data$com_services_as_usual),
      com_basic_skills_training = dplyr::first(.data$com_basic_skills_training),
      com_soft_skills_training = dplyr::first(.data$com_soft_skills_training),
      com_behavioural_skills_training = dplyr::first(
        .data$com_behavioural_skills_training
      ),
      com_job_specific_technical_skills_off_job_training = dplyr::first(
        .data$com_job_specific_technical_skills_off_job_training
      ),
      com_business_skills_training = dplyr::first(
        .data$com_business_skills_training
      ),
      com_business_advisory_and_mentoring = dplyr::first(
        .data$com_business_advisory_and_mentoring
      ),
      com_financial_and_start_up_support = dplyr::first(
        .data$com_financial_and_start_up_support
      ),
      com_job_search_preparation = dplyr::first(
        .data$com_job_search_preparation
      ),
      com_job_search_assistance = dplyr::first(.data$com_job_search_assistance),
      com_employment_counselling = dplyr::first(
        .data$com_employment_counselling
      ),
      com_employment_coaching = dplyr::first(.data$com_employment_coaching),
      com_financial_assistance = dplyr::first(.data$com_financial_assistance),
      com_job_specific_technical_skills_on_job_training = dplyr::first(
        .data$com_job_specific_technical_skills_on_job_training
      ),
      com_paid_temporary_work_experience = dplyr::first(
        .data$com_paid_temporary_work_experience
      ),
      com_unpaid_temporary_work_experience = dplyr::first(
        .data$com_unpaid_temporary_work_experience
      ),
      com_wage_subsidies = dplyr::first(.data$com_wage_subsidies),
      com_public_works = dplyr::first(.data$com_public_works),
      com_other_active_component_nec = dplyr::first(
        .data$com_other_active_component_nec
      ),
      low_study_quality = dplyr::first(.data$low_study_quality),
      .groups = "drop"
    ) %>%
    dplyr::rename(study_id = .data$base_study_id) %>%
    dplyr::select(
      study_id,
      outcome_domain,
      outcome,
      outcome_source,
      favourable_direction,
      outcome_timing,
      estimand,
      intention_to_treat,
      conditional,
      treatment_n,
      comparison_n,
      d,
      d_se,
      d_var,
      g,
      g_se,
      g_var,
      year_published,
      source,
      peer_reviewed,
      study_design_type,
      study_design_detail,
      proportion_female_treatment,
      study_age_min,
      study_age_max,
      study_age_mean,
      location,
      year_start,
      intervention_length_n,
      intervention_intensity_n,
      youth_focused,
      intervention_developer,
      intervention_implementor,
      program_scope,
      intervention_funding,
      conditionality,
      incentives,
      certification,
      study_funding,
      evaluator,
      int_basic_skills_training,
      int_soft_skills_training,
      int_behavioural_skills_training,
      int_job_specific_technical_skills_off_job_training,
      int_business_skills_training,
      int_business_advisory_and_mentoring,
      int_financial_and_start_up_support,
      int_job_search_preparation,
      int_job_search_assistance,
      int_employment_counselling,
      int_employment_coaching,
      int_financial_assistance,
      int_job_specific_technical_skills_on_job_training,
      int_paid_temporary_work_experience,
      int_unpaid_temporary_work_experience,
      int_wage_subsidies,
      int_public_works,
      int_other_active_component_nec,
      com_services_as_usual,
      com_basic_skills_training,
      com_soft_skills_training,
      com_behavioural_skills_training,
      com_job_specific_technical_skills_off_job_training,
      com_business_skills_training,
      com_business_advisory_and_mentoring,
      com_financial_and_start_up_support,
      com_job_search_preparation,
      com_job_search_assistance,
      com_employment_counselling,
      com_employment_coaching,
      com_financial_assistance,
      com_job_specific_technical_skills_on_job_training,
      com_paid_temporary_work_experience,
      com_unpaid_temporary_work_experience,
      com_wage_subsidies,
      com_public_works,
      com_other_active_component_nec,
      low_study_quality
    )

  pooled
}
