dev <- function(y, mu, w, family = c("gaussian", "binomial", "poisson")) {
  family <- match.arg(family)
  if (family == "gaussian") {
    err <- sum(w*(y-mu)^2)
  } else if (family == "binomial") { 
    mu[mu > 0.9999] = 0.999
    mu[mu < 0.0001] = 0.001
    err <- sum(-2*w*(y*log(mu)+(1-y)*log(1-mu)))
  } else if (family == "poisson") {
    r <- mu*w
    p <- which(y > 0)
    r[p] <- (w*(y*log(y/mu) - (y - mu)))[p]
    err <- sum(2*r)
  }
  err <- err/sum(w)
  err
}
dev.residuals <- function(y, mu, w, family = c("gaussian", "binomial", "poisson")) {
  family <- match.arg(family)
  if (family == "gaussian") {
    dev.residuals <- w*(y-mu)^2
  } else if (family == "binomial") { 
    mu[mu > 0.9999] = 0.999
    mu[mu < 0.0001] = 0.001
    dev.residuals <- -2*w*(y*log(mu)+(1-y)*log(1-mu))
  } else if (family == "poisson") {
    r <- mu*w
    p <- which(y > 0)
    r[p] <- (w*(y*log(y/mu) - (y - mu)))[p]
    dev.residuals <- 2*r
  }
  s <- sign(y - mu)
  dev.residuals <- sqrt(pmax(dev.residuals, 0))*s
  dev.residuals
}
dev.pearson <- function(y, mu, w, family = c("gaussian", "binomial", "poisson")) {
  family <- match.arg(family)
  if (family == "gaussian") {
    dev.pearson <- (y - mu) * sqrt(w)
  } else if (family == "binomial") {
    mu[mu > 0.9999] = 0.999
    mu[mu < 0.0001] = 0.001
    dev.pearson <- (y - mu) * sqrt(w)/sqrt(mu*(1-mu))
  } else if (family == "poisson") {
     dev.pearson <- (y - mu) * sqrt(w)/sqrt(mu)
  }  
  dev.pearson

}
dev.working <- function(y, mu, w, family = c("gaussian", "binomial", "poisson")) {
  family <- match.arg(family)
  if (family == "gaussian") {
    dev.working <- (y - mu)
  } else if (family == "binomial") {
    mu[mu > 0.9999] = 0.999
    mu[mu < 0.0001] = 0.001
    dev.working <- (y - mu)/(mu*(1-mu))
  } else if (family == "poisson") {
     dev.working <- (y - mu)/pmax(mu, .Machine$double.eps)
  }  
  dev.working

}
residuals.sback <- function (object, type = c("deviance", "pearson", "working", "response"), ...) {
    type <- match.arg(type)
    fsb <- interpret.sbformula(object$formula)
    y <- object$data[,fsb$response]
    mu <- object$fitted.values
    w <- object$weights
    family <- object$family
    res <- switch(type, 
            deviance = dev.residuals(y, mu, w, family), 
            pearson =  dev.pearson(y, mu, w, family), 
            working = dev.working(y, mu, w, family), 
            response = y - mu)
    res
}