sback.fit <-
function(formula, data, offset = NULL, weights = NULL, kbin = 15, family = c("gaussian", "binomial", "poisson"), newdata = NULL, newoffset = NULL, call = NULL, pred = FALSE) {
	family <- match.arg(family)
	family_fortran <- switch(family, "gaussian" = 2, "binomial" = 1, "poisson" = 3)

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
	if(!is.null(newdata)) {
		newdata[,"ONE"] <- 1.0
		if(any(is.na(match(c(x.varnames, z.varnames), names(data))))) {
			stop("Not all needed variables are supplied in newdata")
		}
	} else {
		newdata <- data
	}
	data <- na.omit(data[,c(fsb$response, unique(c(x.varnames, z.varnames)))])
	newdata <- na.omit(newdata[,unique(c(x.varnames, z.varnames))])
	
	n <- nrow(data)
	n0 <- nrow(newdata)
	if(is.null(weights)) {
		weights <- rep(1, n)  
	} else {
		if(sum(weights) <= 0 || any(weights < 0) || length(weights) != n)
			stop("The specified weights are not correct")
	}

	if(is.null(offset)) {
		offset <- rep(0, n)  
	}

	if(is.null(newoffset)) {
		newoffset <- rep(0, n0)  
	}

	# Smooth effects (either varying or not)
	x.varnames.s <- x.varnames[fsb$h != 0]
	z.varnames.s <- z.varnames[fsb$h != 0]

	if(length(x.varnames.s) == 0) {
		stop("No smooth functions have been specified")
	}
	mode <- lapply(c(x.varnames.s, z.varnames.s), function(x,data) class(data[, x, drop = TRUE]), data = data)
	if(any(mode %in% "factor")) {
		stop("Only continuos covariates are allowed for non parametric effects and varying coefficients. Factors are not allowed")
	}

	# Parametric effects
		# Included as linear or parametric (i.e. categorical covariates)
			x.varnames.p <- x.varnames[fsb$h == 0]
		# Varying (if any)
			z.varnames.p <- vector()
			aux <- fsb$II[,fsb$h != 0, drop = FALSE]
			for(i in 1:ncol(aux)) {
				if(aux[1,i] == "ONE") {
					z.varnames.p <- c(z.varnames.p, aux[2,i])
					#z.varnames.p <- c(z.varnames.p) # Without linear part
				} else {
					z.varnames.p <- c(z.varnames.p, aux[1,i], paste(aux[1,i], ":", aux[2,i], sep = ""))
					#z.varnames.p <- c(z.varnames.p, aux[1,i]) # Without linear part
				}

			}
		#  Remove that which are repeated
		z.varnames.p <- unique(c(z.varnames.p, x.varnames.p))

	if(length(z.varnames.p) == 0) {
		Xl <- double(0)
		Xpl <- double(0)
		nparl <- 0
		names.param <- NULL
		
	} else {
		# Construct parametric design matrix	
		formula.p <- paste("~", paste(z.varnames.p, collapse = "+"))
		# Fit
		MMp <- construct.fixed.part(formula.p, data)
		Xl <- MMp$X
		# Prediction
		Xpl <- construct.fixed.prediction.matrix(MMp, newdata)
		nparl <- ncol(Xl)
		names.param <- colnames(MMp$X)
	}
	if(all(z.varnames.s %in% "ONE")) {
	 	fit  <- .Fortran("dllsback",
	               x       = matrix(as.double(as.matrix(data[,x.varnames.s])), ncol = length(x.varnames.s)),
	               y       = as.double(data[,fsb$response]),
	               offset  = as.double(offset),
	               w       = as.double(weights),
	               n       = as.integer(n),
	               npar    = as.integer(length(x.varnames.s)),
	               xl 	   = matrix(as.double(Xl), ncol = nparl),
				   nparl   = as.integer(nparl),
	               kbin    = as.integer(kbin),
	               h       = as.double(fsb$h[fsb$h != 0]),
	               m       = matrix(as.double(rep(0.0,n*length(x.varnames.s))), nrow = n, ncol = length(x.varnames.s)),
	               muhat   = as.double(rep(0.0,n)),
	               family  = as.double(family_fortran),
		  		   x0	   = matrix(as.double(as.matrix(newdata[,x.varnames.s])), ncol = length(x.varnames.s)),
		  		   x0l     = matrix(as.double(Xpl), ncol = nparl),
		  		   offset0 = as.double(newoffset),
		   		   m0      = matrix(as.double(rep(0.0, n0*length(x.varnames.s))), nrow = n0, ncol = length(x.varnames.s)),
		  		   muhat0  = as.double(rep(0.0,n0)),
		   		   n0	   = as.integer(n0),
		   		   B       = as.double(rep(0.0, as.integer(nparl + 1))),
		   		   err     = as.integer(0), PACKAGE = "wsbackfit")
	 	peffects <- fit$m0
		effects <- fit$m
	} else {
		fit  <- .Fortran("dllvcoef",
		x 	    = matrix(as.double(as.matrix(data[,x.varnames.s])), ncol = length(x.varnames.s)),
		z 	    = matrix(as.double(as.matrix(data[,z.varnames.s])), ncol = length(z.varnames.s)),
		offset  = as.double(offset),                  
		y 	    = as.double(data[,fsb$response]),
		w 	    = as.double(weights),
		n 	    = as.integer(n),
		npar    = as.integer(length(x.varnames.s)),
		zl 	    = matrix(as.double(Xl), ncol = nparl),
		nparl   = as.integer(nparl),
		kbin    = as.integer(kbin),
		h 	    = as.double(fsb$h[fsb$h != 0]),
		m 	    = matrix(as.double(rep(0.0, n*length(z.varnames.s))), nrow = n, ncol = length(z.varnames.s)),
		mx	    = matrix(as.double(rep(0.0, n*length(x.varnames.s))), nrow = n, ncol = length(x.varnames.s)),
		muhat   = as.double(rep(0.0, n)),
		family  = as.double(family_fortran),
		x0 	    = matrix(as.double(as.matrix(newdata[,x.varnames.s])), ncol = length(x.varnames.s)),
		z0 	    = matrix(as.double(as.matrix(newdata[,z.varnames.s])), ncol = length(z.varnames.s)),
		z0l     = matrix(as.double(Xpl), ncol = nparl),
		offset0 = as.double(newoffset),
		mx0     = matrix(as.double(rep(0.0, n0*length(x.varnames.s))), nrow = n0, ncol = length(x.varnames.s)),
		muhat0  = as.double(rep(0.0, n0)),
		n0      = as.integer(n0),
		B       = as.double(rep(0.0, as.integer(nparl + 1))), 
		err     = as.integer(0), PACKAGE = "wsbackfit")
	
		effects <- fit$mx
		peffects <- fit$mx0

	}
	colnames(effects) <- colnames(peffects)<- fsb$partial[fsb$h != 0]
	names(fit$B) <- c("Intercept", names.param)

	residuals <- dev.residuals(data[,fsb$response], fit$muhat, weights, family = family)

	res <- list(call = call, formula = formula, data = data, weights = weights, offset = offset, kbin = kbin, family = family, pdata = newdata, poffset = newoffset, effects = effects, peffects = peffects, fitted.values = fit$muhat, pfitted.values = fit$muhat0, residuals = residuals, h = fit$h, fit = fit, coeff = fit$B)
	if(!pred) {
		res$pdata <- NULL
		res$poffset <- NULL
		res$peffects <- NULL
		res$pfitted.values <- NULL
	}
  	res
}