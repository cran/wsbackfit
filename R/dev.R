dev <- function(y, mu, w, family = c("gaussian", "binomial", "poisson")) {
  family <- match.arg(family)
  if (family == "gaussian") {
    err <- sum(w*(y-mu)^2)
  } else if (family == "binomial") { 
    mu[mu > 0.9999] = 0.999
    mu[mu < 0.0001] = 0.001
    err <- sum(w*(y*log(mu)+(1-y)*log(1-mu)))
  } else if (family == "poisson") {
    #mu[mu < 0.0001] <- 0.001
    #err <-  sum(w*(-y*log(mu) + (mu-y)))
    r <- mu*w
    p <- which(y > 0)
    r[p] <- (w*(y*log(y/mu) - (y - mu)))[p]
    err <- sum(2*r)
  }
  err <- err/sum(w)
}