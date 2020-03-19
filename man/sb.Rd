\name{sb}
\alias{sb}
\title{
Specify a nonparametric and/or a varying coefficient term in a wsbackfit formula
}
\description{
Function used to indicate nonparametric terms and varying coefficient terms in a \code{\link{sback}} formula.
}
\usage{
sb(x1 = NULL, by = NULL, h = -1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x1}{the univariate predictor}
  \item{by}{numeric predictor of the same dimension as \code{x1}. If present, the coefficients of this predictor depend, nonparametrically, on \code{x1}, i.e., a varying coefficient term.}
  \item{h}{bandwidth (on the scale of the predictor) for this term. If \code{h = -1}, the bandwidth is automatically selected using k-fold cross-validation (see \code{\link{sback}}). A value of 0 would indicate a linear fit. By default -1.}
}
%\details{}
\value{A list with the following components:
\item{cov}{character vector with the name(s) of the involved predictor(s).}
\item{h}{numeric value with the specified smoothing parameter.}}
%\references{}
\author{Javier Roca-Pardinas, Maria Xose Rodriguez-Alvarez and Stefan Sperlich}
%\note{}
\seealso{
\code{\link{sback}}, \code{\link{summary.sback}}, \code{\link{plot.sback}}
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

op <- par(no.readonly = TRUE)

par(mfrow = c(2,2))
plot(m0)

\donttest{
# Fit the model with the bandwidths selected by k-fold cross-validation.
m1 <- sback(formula = y ~ x5 + sb(x1, h = -1) + sb(x2, h = -1) 
  + sb(x3, h = -1) + sb(x4, h = -1), kbin = 15, bw.grid = seq(0.01, 0.99, length = 30),
  data = df)

summary(m1)

par(mfrow = c(2,2))
plot(m1)
}

par(op)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
