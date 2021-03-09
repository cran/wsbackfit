search.alpha <- function (formula, data, offset = NULL, weights = NULL, kernel = c("Gaussian", "Epanechnikov"), alpha = seq(0.5, 1.5, length = 10), KfoldCV = 5, kbin = 25, family = c("gaussian", "binomial", "poisson")) {
  family <- match.arg(family)
  kernel <- match.arg(kernel)

  h <- interpret.sbformula(formula)$h
  if (max(h) < 0) {
    stop("For the alpha correction the user needs to specify bandwidth parameters for all nonparametric functions.")
  } else {
    err <- NULL
    for (alpha0 in alpha) {
      err <- c(err, mean(calculate.CV(formula = create.formula.alpha(formula = formula, alpha0 = alpha0, data = data), data = data, offset = offset, weights = weights, kernel = kernel, kbin = kbin, family = family, KfoldCV = KfoldCV), na.rm = TRUE))
    }
    err.CV <- data.frame(alpha, err)
    alpha0 <- alpha[which.min(err)]
    res <- list(formula = create.formula.alpha(formula = formula, alpha0 = alpha0, data = data), err.CV = err.CV)
  }
  res
}