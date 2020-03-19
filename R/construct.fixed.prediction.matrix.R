construct.fixed.prediction.matrix <- function(object, newdata) {
	#if(!is.null(object$terms$fixed)) {
		mfp <- model.frame(object$terms, newdata, xlev = attr(object$terms, "xlev"))
		Xp <- model.matrix(object$terms, data = mfp, contrasts.arg = attr(object$terms, "contrast"))
		Xp <- Xp[,-1,drop = FALSE]
	#} else {
	#	Xp <- NULL
	#}
	Xp
}
