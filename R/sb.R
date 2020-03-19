sb <-
function(x1 = NULL, by = NULL, h = -1) {   
    args <- match.call()
    if(!is.null(args$x1) & is.null(args$by)) {               
        cov = c("ONE", deparse(args$x1, backtick = TRUE, width.cutoff = 500))
    } else if (!is.null(args$x1) & !is.null(args$by)) {      
        cov = c(deparse(args$by, backtick = TRUE, width.cutoff = 500), deparse(args$x1, backtick = TRUE, width.cutoff = 500))
    } else {
        stop("Invalid expression")
    }
    res <- list(cov = cov, h = h)
    res
}
