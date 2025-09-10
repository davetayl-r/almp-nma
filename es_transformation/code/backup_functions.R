# backups

# convert results reported as a continuous treatment effect to SMD
treatment_effect_continuous_to_smd <- function(
  treatment_n,
  comparison_n,
  treatment_effect,
  # pooled SD is preferred by the function
  pooled_sd,
  # SE for TE if no pooled SD is reported
  treatment_effect_se = NULL,
  # p-value for TE if no SE reported
  treatment_effect_p_value = NULL,
  # confidence intervals for TE is no SE is reported
  treatment_effect_ci_low = NULL,
  treatment_effect_ci_high = NULL,
  ci_level = 0.95,
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

  # derive SE if needed
  se_md <- rep(NA_real_, k)
  if (!is.null(treatment_effect_se)) {
    se_md <- treatment_effect_se
  }

  # from p-value (two-sided normal approx)
  if (!is.null(treatment_effect_p_value)) {
    idx <- mask &
      is.finite(md) &
      is.finite(treatment_effect_p_value) &
      treatment_effect_p_value > 0 &
      treatment_effect_p_value <= 1
    if (any(idx)) {
      z <- qnorm(pmax(
        1 - treatment_effect_p_value[idx] / 2,
        .Machine$double.eps
      ))
      se_md[idx] <- abs(md[idx]) / z
    }
  }

  # from confidence intervals
  if (!is.null(treatment_effect_ci_low) && !is.null(treatment_effect_ci_high)) {
    idx <- mask &
      is.finite(treatment_effect_ci_low) &
      is.finite(treatment_effect_ci_high)
    if (any(idx)) {
      z <- qnorm(1 - (1 - ci_level) / 2)
      se_md[idx] <- (treatment_effect_ci_high[idx] -
        treatment_effect_ci_low[idx]) /
        (2 * z)
      # if md itself is missing, you could also set md[idx] <- (high+low)/2, but we leave md as provided
    }
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

  ok <- mask &
    is.finite(n1) &
    n1 > 1 &
    is.finite(n2) &
    n2 > 1 &
    is.finite(md) &
    is.finite(sp) &
    sp > 0

  if (any(ok)) {
    i <- which(ok)
    df <- n1[i] + n2[i] - 2

    # Cohen's d from MD and pooled SD
    d[i] <- md[i] / sp[i]
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

# convert results reported as a binary treatment effect (risk difference) to SMD
treatment_effect_binary_to_smd <- function(
  treatment_n,
  comparison_n,
  treatment_effect,
  treatment_effect_se = NULL,
  treatment_effect_p_value = NULL,
  treatment_effect_ci_low = NULL,
  treatment_effect_ci_high = NULL,
  ci_level = 0.95,
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

  # from p-value (follows two-sided normal approx)
  if (!is.null(treatment_effect_p_value)) {
    idx <- mask &
      is.finite(att) &
      is.finite(treatment_effect_p_value) &
      treatment_effect_p_value > 0 &
      treatment_effect_p_value <= 1
    if (any(idx)) {
      z <- qnorm(1 - treatment_effect_p_value[idx] / 2)
      se[idx] <- abs(att[idx]) / z
    }
  }

  # from confidence intervals
  if (!is.null(treatment_effect_ci_low) && !is.null(treatment_effect_ci_high)) {
    idx <- mask &
      is.finite(att) &
      is.finite(treatment_effect_ci_low) &
      is.finite(treatment_effect_ci_high)
    if (any(idx)) {
      z <- qnorm(1 - (1 - ci_level) / 2)
      se[idx] <- (treatment_effect_ci_high[idx] -
        treatment_effect_ci_low[idx]) /
        (2 * z)
    }
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
