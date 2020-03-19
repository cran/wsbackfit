create.formula <-  function(formula, data, h0 = 0) {
  A <- interpret.sbformula(formula)
  aux <- vector()
  for (i in 1:A$npartial) {
    if (A$II[1,i] == "ONE")  {
      if (A$h[i] == -1) {
        range <- max(data[, A$II[2,i]], na.rm = TRUE) - min(data[, A$II[2,i]], na.rm = TRUE)
        sd <- sd(data[, A$II[2,i]], na.rm = TRUE)
        #aux[i] <- paste ("sb(", A$II[2,i], ", h = ", h0*range/sd, ")",sep="")
        aux[i] <- paste ("sb(", A$II[2,i], ", h = ", round(h0*sd, 4), ")",sep="")
      } else if (A$h[i]>0)  {
        aux[i] <- paste ("sb(", A$II[2,i], ", h = ", A$h[i], ")",sep="")
      } else {
        aux[i] <- A$II[2,i]
      }
    } else {
      if (A$h[i] == -1) {
        range <- max(data[, A$II[2,i]], na.rm = TRUE) - min(data[, A$II[2,i]], na.rm = TRUE)
        sd <- sd(data[, A$II[2,i]], na.rm = TRUE)
        #aux[i] <- paste ("sb(", A$II[2,i],", by = ", A$II[1,i], ", h = ", h0*range/sd, ")",sep="")
        aux[i] <- paste ("sb(", A$II[2,i],", by = ", A$II[1,i], ", h = ", round(h0*sd, 4), ")",sep="")
      } else {
        aux[i] <- paste ("sb(", A$II[2,i], ", h = ", A$h[i],", by = ", A$II[1,i], ")",sep="")}
      }
  }
  res <- as.formula(paste(A$response, "~", paste(aux, collapse = "+", sep = ""), sep = ""))
  res
}