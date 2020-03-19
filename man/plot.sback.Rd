\name{plot.sback}
\alias{plot.sback}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Default sback plotting
}
\description{
Takes a fitted object produced by \code{sback()} and plots the estimates of the nonparametric functions on the scale of their respective covariates, no matter whether a particular nonparametric function is an additive component or a varying coefficient.
}
\usage{
\method{plot}{sback}(x, composed = TRUE, ask = TRUE, select = NULL, ...)
}
\arguments{
  \item{x}{an object of class \code{sback} as produced by \code{sback()}.}
  \item{composed}{a logical value. If \code{TRUE}, the default, the function plots the estimates of the composed (linear plus nonlinear) nonparametric functions (see Details).}
  \item{ask}{a logical value. If \code{TRUE}, the default, the user is asked for confirmation, before a new figure is drawn.}
  \item{select}{Allows the plot for a single model term to be selected for printing. e.g. if you just want the plot for the second smooth term set select = 2.}
  \item{\dots}{other graphics parameters to pass on to plotting commands.}
}
\details{For identifiability purposes, the estimating algorithm implemented in the \code{wsbackfit} package decomposes each nonparametric function in two components: a linear (parametric) component and a nonlinear (nonparametric) component. For plotting, the user can choose to plot these components either separately in one graph (\code{composed = FALSE}), or to only plot the resulting composed function (\code{composed = TRUE}). Also, for the varying coefficient terms, the plots show the estimated surface spanned by \eqn{(g_j , X_j , Z_j)}.}
\value{None}
%\references{}
\author{Javier Roca-Pardinas, Maria Xose Rodriguez-Alvarez and Stefan Sperlich}
%\note{}
\seealso{
\code{\link{sback}}, \code{\link{summary.sback}}
}
\examples{
library(wsbackfit)
################################################
# Gaussian Simulated Sample
###############################################
set.seed(123)
# Define the data generating process
n <- 1000
x1 <- runif(n)*4-2
x2 <- runif(n)*4-2
x3 <- runif(n)*4-2
x4 <- runif(n)*4-2
x5 <- as.numeric(runif(n)>0.6)

f1 <- 2*sin(2*x1)
f2 <- x2^2
f3 <- 0
f4 <- x4
f5 <- 1.5*x5

mu <- f1 + f2 + f3 + f4 + f5
err <- (0.5 + 0.5*x5)*rnorm(n)
y <- mu + err

df <- data.frame(x1 = x1, x2 = x2, x3 = x3, x4 = x4, x5 = as.factor(x5), y = y)

# Fit the model with a fixed bandwidth for each covariate
m0 <- sback(formula = y ~ x5 + sb(x1, h = 0.13) + sb(x2, h = 0.13) 
  + sb(x3, h = 0.13) + sb(x4, h = 0.13), kbin = 15, data = df)

plot(m0)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
