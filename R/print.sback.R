print.sback <-
function(x, ...){
	foo0 <- cbind(colnames(x$effects), round(x$h, 4))
	colnames(foo0) <- c("Effect", "h")
	rownames(foo0) <- rep("", dim(foo0)[1])
	cat("Generalized Smooth Backfitting/wsbackfit:\n\n")
	cat("Call: "); print(x$call)
	cat("\nSample size:", length(x$fitted.values), "\n\nBandwidths used in model:\n")
	print(foo0, quote = FALSE)
	cat("\nLinear/Parametric components:\n")
	print(x$coeff, quote = FALSE)
	cat("\n")
}
