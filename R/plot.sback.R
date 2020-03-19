plot.sback <-
function(x, composed = TRUE, ask = TRUE, select = NULL, ...) {
	dots <- list(...)
	grph.opt <- names(dots)
	p.functions <- colnames(x$effects)[x$h != 0]
	if(is.null(select)) {
		ind <- 1:length(p.functions)
	} else if(!is.numeric(select) | !all(select%in%(1:length(p.functions)))) {
		stop("The model terms selected for printing do not exists")
	} else {
		ind <- select
	}
	j <- 0
	for(i in ind) {
		j <- j + 1
		if(j > 1 & length(ind) > 1) {
			if(ask) readline("Press return for next page ...")
		}
		# Partial effect
		int.f <- interpret.sbformula(as.formula(paste("~", p.functions[i]))) 
		var <- int.f$II[2,]
		var.lin.1 <- ifelse(int.f$II[1,] == "ONE", int.f$II[2,], paste(int.f$II[1,], ":", int.f$II[2,], sep = ""))
		var.lin.2 <- ifelse(int.f$II[1,] == "ONE", int.f$II[2,], paste(int.f$II[2,], ":", int.f$II[1,], sep = ""))
		var.lin <- ifelse(var.lin.1 %in% names(x$coeff), var.lin.1, var.lin.2)
		ord <- order(x$data[,var])
		x.data <- x$data[ord,var]
		y.data.nl <- x$effects[ord,i]
		y.data.l <- x$coeff[var.lin]*x.data
		if(composed) {
			y.composed <- y.data.nl + y.data.l
			range <- max(y.composed) - min(y.composed)
			min.ylim <- min(y.composed) - 0.1*range 
			max.ylim <- max(y.composed) + 0.1*range
		} else {
			range <- max(c(y.data.nl, y.data.l)) - min(c(y.data.nl, y.data.l))
			min.ylim <- min(c(y.data.nl, y.data.l)) - 0.1*range 
			max.ylim <- max(c(y.data.nl, y.data.l)) + 0.1*range
		}
		main.aux <- if(int.f$II[1,] == "ONE") {
			paste(", main = \"Additive effect of ", var, "\"", sep = "")	
		} else {
			paste(", main = \"Varying coefficient \n as function of ", var, "\"", sep = "")
		}
		stub <- paste(ifelse("xlab" %in% grph.opt, "", paste(", xlab = \"", var, "\"",sep = "")), 
                ifelse("ylab" %in% grph.opt, "", paste(", ylab = \"", p.functions[i], "\"", sep = "")), 
                ifelse("main" %in% grph.opt, "", main.aux),
                ifelse("type" %in% grph.opt, "", ", type = \"l\""),
                ifelse("ylim" %in% grph.opt, "", paste(", ylim = c(", min.ylim,",", max.ylim,")", sep = "")), ",...)", sep = "")
		
		if(composed) {			
			plot <- paste("plot(x.data, y.composed", stub, sep = "")
			eval(parse(text = plot))
			rug(x$data[,var])			
		} else {
			plot <- paste("plot(x.data, y.data.nl", stub, sep = "")
			eval(parse(text = plot))
			
			stub <- paste(ifelse("lty" %in% grph.opt, "", ",lty = 2"), ",...)", sep = "")
			lines <- paste("lines(x.data, y.data.l", stub, sep = "")
			eval(parse(text = lines))
			
			rug(x$data[,var])
			stub <- paste(ifelse("lty" %in% grph.opt, "", "lty = 1:2"), ", legend = c(\"Non linear\",\"Linear\"), bty = \"n\"", ", cex = ", dots$cex,")", sep = "")
			legend <- paste("legend(\"topleft\",", stub, sep = "")
			eval(parse(text = legend))
		}	
		# Interaction surface
		if(int.f$II[1,] != "ONE") {
			x.data <- seq(min(x$data[,var]), max(x$data[,var]), l = 50)
			y.data <- seq(min(x$data[,int.f$II[1,]]), max(x$data[,int.f$II[1,]]), l = 50)
			z.data <- suppressWarnings(outer(approxfun(x$data[ord,var], x$effects[ord,i] + x$coeff[var.lin]*x$data[ord,var])(x.data), y.data, '*'))
			if(ask) readline("Press return for next page....")
			persp(x.data, y.data, z.data, xlab = var, ylab = int.f$II[1,], zlab = p.functions[i], 
				main = paste("Estimated surface for ", var, " and ", int.f$II[1,], sep = ""), 
				theta = ifelse(is.null(dots$theta), 45, dots$theta), 
				phi = ifelse(is.null(dots$phi), 45, dots$phi), 
				ticktype = "detailed",
				shade = ifelse(is.null(dots$shade), 0.5, dots$shade),
				cex.main = dots$cex.main, cex.axis = dots$cex.axis, cex.lab = dots$cex.lab, cex.sub = dots$cex.sub, cex = dots$cex)	
		}
	}
}
