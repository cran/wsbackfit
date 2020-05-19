sback <-
function(formula, data, offset = NULL, weights = NULL, bw.grid = seq(0.01, 0.99, length = 30), KfoldCV = 5, kbin = 15, family = c("gaussian", "binomial", "poisson")) {
	family <- match.arg(family)
	if(missing(formula)) {
		stop("Argument \"formula\" is missing, with no default")
	}
	if(missing(formula)) {
		stop("Argument \"data\" is missing, with no default")
	}

	data[,"ONE"] <- 1.0
	fsb <- interpret.sbformula(formula)
	if(is.null(fsb$response)) {
		stop("Response variable should be specified in argument \"formula\"")
	}
	z.varnames <- fsb$II[1,]
	x.varnames <- fsb$II[2,]
	if(any(is.na(match(c(fsb$response, c(x.varnames, z.varnames)), names(data))))) {
		stop("Not all needed variables are supplied in data")
	}
	
	data <- na.omit(data[,c(fsb$response, unique(c(x.varnames, z.varnames)))])
	
	n <- nrow(data)
	if(is.null(weights)) {
		weights <- rep(1, n)  
	} else {
		if(sum(weights) <= 0 || any(weights < 0) || length(weights) != n)
			stop("The specified weights are not correct")
	}

	if(is.null(offset)) {
		offset <- rep(0, n)  
	}

	# Smooth effects (either varying or not)
	x.varnames.s <- x.varnames[fsb$h != 0]
	z.varnames.s <- z.varnames[fsb$h != 0]

	if(length(x.varnames.s) == 0) {
		stop("No smooth functions have been specified")
	}
	mode <- lapply(c(x.varnames.s, z.varnames.s), function(x,data) class(data[, x, drop = TRUE]), data = data)
	if(any(mode %in% "factor")) {
		stop("Only continuos covariates are allowed for non parametric effects and varying coefficient components. Factors are not allowed")
	}

	if(any(fsb$h == -1)) {
		optband <-  search.bandwidth(formula = formula, data = data, offset = offset, weights = weights, bandwidth = bw.grid, KfoldCV = KfoldCV, kbin = kbin, family = family)
	    res <- sback.fit(formula = optband$formula, data = data, offset = offset, weights = weights, kbin = kbin, family = family, newdata = data, newoffset = offset, call = match.call()) 
	    if(res$fit$err == 1) {
			stop("There has been an error during the fitting process. Most likely, the error is due to bandwidth parameters being too small.")
		}
	    res$err.CV <- optband$err.CV
	} else {
		res <- sback.fit(formula = formula, data = data, offset = offset, weights = weights, kbin = kbin, family = family, newdata = data, newoffset = offset, call = match.call()) 
		if(res$fit$err == 1) {
			stop("There has been an error during the fitting process. Most likely, the error is due to bandwidth parameters being too small.")
		}
	}
	class(res) <- "sback"
  	res
}