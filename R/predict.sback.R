predict.sback <- function(object, newdata, newoffset = NULL, ...) {
	if(missing(newdata)) {
		newdata <- object$data
	}
	n0 <- nrow(newdata)
	if(is.null(newoffset)) {
		newoffset <- rep(0, n0)  
	}
	fsb <- interpret.sbformula(object$formula)
	z.varnames <- fsb$II[1,]
	x.varnames <- fsb$II[2,]
	
	newdata[,"ONE"] <- 1.0
	if(any(is.na(match(c(x.varnames, z.varnames), names(object$data))))) {
		stop("Not all needed variables are supplied in newdata")
	}
	formula <- create.formula(formula = object$formula, h0 = object$h, data = object$data)
	fit <- sback.fit(formula = formula, data = object$data, offset = object$offset, weights = object$weights, kbin = object$kbin, family = object$family, newdata = newdata, newoffset = newoffset, call = NULL, pred = TRUE)

	out <- list()
	out$newdata <- newdata
	out$newoffset <- newoffset
	out$coeff <- fit$coeff
	out$peffects <- fit$peffects
	out$pfitted.values <- fit$pfitted.values
	out
}