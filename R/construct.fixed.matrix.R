construct.fixed.part <- function(formula, data) {
	#env <- environment(formula) 
	if(inherits(formula, "character"))          
		formula <- as.formula(formula)
	
	mf <- model.frame(formula, data, drop.unused.levels = TRUE)
	mt <- terms(mf)   
	X <- model.matrix(mt, mf)
	
	dim <- table(attr(X,"assign"))[-1]
	names(dim) <- attr(mt, "term.labels")
	
	attr(mt, "contrast") <- attr(X,"contrast")
	attr(mt, "xlev") <- .getXlevels(mt, mf)

	res <- list(X = X[,-1, drop = FALSE], dim = dim, terms = mt)
	res	
}
