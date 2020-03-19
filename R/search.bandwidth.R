search.bandwidth <- function (formula, data, offset = NULL, weights = NULL, bandwidth = seq(0.01, 0.99, length = 30), KfoldCV = 5, kbin = 25, family = c("gaussian", "binomial", "poisson")) {
  family <- match.arg(family)
  h <- interpret.sbformula(formula)$h
  if (min(h) >= 0) {
    res <- list(formula = formula)
  } else {
    err <- NULL
    for (h0 in bandwidth) {
      err <- c(err, mean(calculate.CV(formula = create.formula(formula = formula, h0 = h0, data = data), data = data, offset = offset, weights = weights, kbin = kbin, family = family, KfoldCV = KfoldCV)))
    }
    err.CV <- data.frame(bandwidth, err)
    h0 <- bandwidth[which.min(err)]
    res <- list(formula = create.formula(formula = formula, h0 = h0, data = data), err.CV = err.CV)
  }
  res
}