calculate.CV <- function(formula, data, offset = NULL, weights = NULL, kernel = c("Gaussian", "Epanechnikov"), kbin = 25, family = c("gaussian", "binomial", "poisson"), KfoldCV = 5) {
  family <- match.arg(family)
  kernel <- match.arg(kernel)

  n <- nrow(data)
  if(is.null(weights)) {
    weights <- rep(1, n)
  }
  ECM <- vector(length = 0)
  random <- runif(n, min = 0, max = 1)
  factor <- c(0:KfoldCV)/KfoldCV
  groups <- cut(random, factor)
  #for (k in 1:KfoldCV) {
  for (x in levels(groups)) {
    #ii <- sample(n, size = 0.70*n)
    #train <- data[ii,]
    #test <- data[-ii,]
    #wtrain <- weights[ii]
    #wtest <- weights[-ii]
    train <- data[-which(groups == x),]
    test <-  data[which(groups == x),]
    wtrain <- weights[-which(groups == x)]
    wtest <- weights[which(groups == x)]
    offtrain <- offset[-which(groups == x)]
    offtest <- offset[which(groups == x)]
    mod <- sback.fit(formula = formula, data = train, offset = offtrain, weights = wtrain, kernel = kernel, kbin = kbin, family = family, newdata = test, newoffset = offtest, pred = TRUE)
    if(mod$fit$err == 0) {
      response <- as.character(attr(terms(formula), "variables")[2]) # Response variable
      ECM <- append(ECM, dev(test[,response], mod$pfitted.values, wtest, family = family))
    } else {
      ECM <- append(ECM, NA)
    }
  }
  ECM
}