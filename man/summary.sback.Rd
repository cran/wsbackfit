\name{summary.sback}
\alias{summary.sback}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Summary for a \code{sback} fitted object
}
\description{
Takes a fitted object produced by \code{sback()} and produces various useful summaries from it.
}
\usage{
\method{summary}{sback}(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class \code{sback} as produced by \code{sback()}.}
  \item{\dots}{other arguments (not implemented)}
}
%\details{}
\value{An object of class \code{summary.sback} with the information needed to print the results.}
%\references{}
\author{
Javier Roca-Pardinas, Maria Xose Rodriguez-Alvarez, Stefan Sperlich
}
%\note{}

\seealso{
\code{\link{sback}}, \code{\link{plot.sback}}
}
\examples{
library(wsbackfit)
set.seed(123)
###############################################
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

summary(m0)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
