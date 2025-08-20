n1 <- 120
n2 <- 150

prop_event_1 <- .5
prop_event_2 <- .4

# inputs
n1 <- 120
n2 <- 150
p1 <- 0.50
p2 <- 0.40

# function
prop_to_hedges_g <- function(n1, n2, p1, p2) {
  s1_sq <- p1 * (1 - p1)
  s2_sq <- p2 * (1 - p2)
  sp <- sqrt(((n1 - 1) * s1_sq + (n2 - 1) * s2_sq) / (n1 + n2 - 2))
  d <- (p1 - p2) / sp
  df <- n1 + n2 - 2
  J <- 1 - 3 / (4 * df - 1)
  g <- J * d
  # optional SE for g (useful in meta-analysis)
  var_d <- (n1 + n2) / (n1 * n2) + d^2 / (2 * (n1 + n2 - 2))
  se_g <- sqrt((J^2) * var_d)
  list(g = g, d = d, se_g = se_g)
}

out <- prop_to_hedges_g(n1, n2, p1, p2)
out$g
# [1] 0.2016951

out$se_g
# optional: standard error for g
