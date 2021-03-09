create.formula.alpha <-  function(formula, data, alpha0 = 0.5) {
  A <- interpret.sbformula(formula)
  aux <- vector()
  for (i in 1:A$npartial) {
    if (A$II[1,i] == "ONE")  {
      if (A$h[i] == -1) {
        stop("For the alpha correction the user needs to specify bandwidth parameters for all nonparametric functions.")
      } else if (A$h[i]>0)  {
        aux[i] <- paste ("sb(", A$II[2,i], ", h = ", round(A$h[i]*alpha0, 4), ")",sep="")
      } else {
        aux[i] <- A$II[2,i]
      }
    } else {
      if (A$h[i] == -1) {
        stop("For the alpha correction the user needs to specify bandwidth parameters for all nonparametric functions.")
      } else {
        aux[i] <- paste("sb(", A$II[2,i], ", h = ", round(A$h[i]*alpha0, 4),", by = ", A$II[1,i], ")",sep="")}
      }
  }
  res <- as.formula(paste(A$response, "~", paste(aux, collapse = "+", sep = ""), sep = ""))
  res
}