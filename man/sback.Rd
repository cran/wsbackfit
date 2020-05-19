\name{sback}
\alias{sback}
\title{
Generalized additive and partially linear models
}
\description{
Main function for fitting generalized structured models by using smooth backfitting.
}
\usage{
sback(formula, data, offset = NULL, weights = NULL, 
	bw.grid = seq(0.01, 0.99, length = 30), KfoldCV = 5, kbin = 15, 
  family = c("gaussian", "binomial", "poisson"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
a \code{\link{formula}} object specifying the model to be fitted (see Details). 
}
  \item{data}{
data frame representing the data and containing all needed variables
}
\item{offset}{
an optional numerical vector containing priori known components to be included in the linear predictor during fitting. Default is zero.
}
  \item{weights}{
an optional numeric vector of `prior weights' to be used in the fitting process. By default, the weights are set to one.
}
  \item{bw.grid}{
numeric vector; a grid for for searching the bandwidth factor \eqn{h_c} when using cross-validation. The bandwidth for dimension (covariate) \eqn{j} is \eqn{h_c \sigma_j}, with \eqn{\sigma_j} being the standard deviation of \eqn{X_j} (see Details). Default is a sequence of length 30 between 0.01 and 0.99.  
}
  \item{KfoldCV}{
number of cross-validation folds to be used for automatically selecting the optimal bandwidth (in the sequence given in argument \code{bw.grid}) for each nonparametric function. Default is 5.
}
\item{kbin}{
an integer value specifying the number of binning knots. Default is 15.
}
\item{family}{
an character specifying the distribution family. Implemented are: Gaussian, Binomial and Poisson. In all cases, the link function is the canonical one (logit for binomial, identity for Gaussian and logarithm for Poisson). By default `gaussian'.
}
%\item{newdata}{
%a data frame containing the values of the covariates at which predictions are wanted. If not provided then the predictions correspond to the original data. 
%}
%\item{newoffset}{
%an optional numerical vector containing an a priori known component to be included in the linear predictor for the predictions (offset associated with the newdata).
%}
}
\details{
The argument \code{formula} corresponds to the model for the conditional mean function, i.e., 
\deqn{E[Y|X,Z] = G(g_0 + \sum_j g_j (X_j) Z_j  + Z_k^{'}\beta).}
This formula is similar to that used for the \code{\link{glm}} function, except that nonparametric functions can be added to the additive predictor by means of function \code{\link{sb}}. For instance, specification \code{y ~ x1 + sb(x2, h = -1)} assumes a parametric effect of \code{x1} (with \code{x1} either numerical or categorical), and a nonparametric effect of \code{x2}. \code{h = -1} indicates that the bandwidth should be selected using k-fold cross-validation. Varying coefficient terms get incorporated similarly. For example, \code{y ~ sb(x1, by = x2)} indicates that the coefficients of \code{x2} depend, nonparametrically, on \code{x1}. In this case both, \code{x1} and \code{x2}, should be numerical predictors. 

With respect to the bandwidths associated with each nonparametric function specified using function \code{\link{sb}}, the user has two options: a) to specify in the formula the desired bandwidth - on the scale of the predictor - through argument \code{h} of function \code{\link{sb}}; or, b) to allow the bandwidths to be automatically and data adaptively selected via cross-validation. In the latter case, the estimation procedure tests each of the bandwidth factors supplied in argument \code{bw.grid}, and selects the one that minimizes the deviance via (k-fold) cross-validation. The number k of cross-validation folds is specified through argument \code{KfoldCV}, with 5 by default. We note that when using cross-validation, to ensure that the bandwidths associated with the nonparametric functions are on the scale of the predictors, the finally used bandwidth is \eqn{h_j =  h \sigma_j} with \eqn{\sigma_j} being the standard deviation of \eqn{X_j}. That is, before fitting the model, each bandwidth factor \eqn{h} provided in \code{bw.grid} is multiplied by the standard deviation of the corresponding predictor. Note that the user has also the possibility to specify the bandwidths for some nonparametric function (through argument \code{h}), while letting for the remaining nonparametric functions the procedure select the bandwidths by cross-validation. For these functions, argument \code{h} should be set to -1.

Finally, it is worth noting that for identifiability purposes, the estimating algorithm implemented in the \code{wsbackfit} package decomposes each nonparametric function in two components: a linear (parametric) component and a nonlinear (nonparametric) component. Note that it implies that for a varying coefficient term \code{~ sb(x1, by = x2)}, the parametric part includes the linear component associated with \code{x1}, as well as the linear interaction between \code{x1} and \code{x2}.     
}
\value{
A list with the following components:
\item{call}{the matched call.}
\item{formula}{the original supplied \code{formula} argument.}
\item{data}{the original supplied \code{data} argument.}
\item{offset}{the original supplied \code{offset} argument.}
\item{weights}{the original supplied \code{weights} argument.}
\item{kbin}{the original supplied \code{kbin} argument.}
\item{family}{the original supplied \code{family} argument.}
%\item{pdata}{the original supplied \code{newdata} argument.}
%\item{poffset}{the original supplied \code{newoffset} argument.}
\item{effects}{matrix with the estimated nonparametric functions (only the nonlinear component) for each covariate value in the original supplied data.}
%\item{peffects}{matrix with the estimated nonparametric functions (only the nonlinear component) for each covariate value in the original supplied newdata.}
\item{fitted.values}{a numeric vector with the fitted values for the supplied data.}
%\item{pfitted.values}{a numeric vector with the fitted values for the supplied newdata.}
\item{h}{a numeric vector of the same length as the number of nonparametric functions, with the bandwidths actually used in the estimation (scaled. See Details).}
\item{coeff}{a numeric vector with the estimated coefficients. This vector includes both the parametric effects as well as the coefficients associated with the linear component of the nonparametric functions. }
\item{err.CV}{matrix with the cross-validated error (deviance) associated with the sequence of tested (unscaled) bandwidths. Each line corresponds to a particular bandwidth (unscaled. See Details).}
}
%\references{}
\author{Javier Roca-Pardinas, Maria Xose Rodriguez-Alvarez and Stefan Sperlich}
%\note{}
\seealso{
\code{\link{sb}}, \code{\link{print.sback}}, \code{\link{summary.sback}}, \code{\link{plot.sback}}
}
\examples{
library(wsbackfit)
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

# Fit the model with bandwidths selectec using K-fold cross-validation
\dontrun{
m0cv <- sback(formula = y ~ x5 + sb(x1) + sb(x2) 
  + sb(x3) + sb(x4), kbin = 15, bw.grid = seq(0.01, 0.99, length = 30), KfoldCV = 5, 
  data = df)

summary(m0cv)

par(mfrow = c(2,2))
plot(m0cv)
}

# Estimate Variance as a function of x5 (which is binary)
resid <- y - m0$fitted.values
sig0 <- var(resid[x5 == 0])
sig1 <- var(resid[x5 == 1])
w <- x5/sig1 + (1-x5)/sig0
m1 <- sback(formula = y ~ x5 + sb(x1, h = 0.13) + sb(x2, h = 0.13) 
  + sb(x3, h = 0.13) + sb(x4, h = 0.13), weights = w, kbin = 15, data = df)

summary(m1)

par(mfrow = c(2,2))
plot(m1)

###############################################
# Poisson Simulated Data
###############################################
set.seed(123)
# Define the data generating process
n <- 1000

x1 <- runif(n,-1,1)
x2 <- runif(n,-1,1)

eta <- 2 + 3*x1^2 + 5*x2^3

exposure <- round(runif(n, 50, 500))
y <- rpois(n, exposure*exp(eta))
df <- data.frame(y = y, x1 = x1, x2 = x2)

# Fit the model
m2 <- sback(formula = y ~ sb(x1, h = 0.1) + sb(x2, h = 0.1), 
  data = df, offset = log(exposure), 
  kbin = 15, family = "poisson")

summary(m2)

par(mfrow = c(1,2))
plot(m2)

# Dataframe and offset for prediction
n.p <- 100
newoffset <- rep(0, n.p)
df.pred <- data.frame(x1 = seq(-1, 1,l = n.p), x2 = seq(-1, 1,l = n.p))

m2p <- predict(m2, newdata = df.pred, newoffset = newoffset)
###############################################
# Postoperative Infection Data
###############################################
data(infect)

# Generalized varying coefficient model with binary response
m3 <- sback(formula = inf ~ sb(gluc, h = 10) + sb(gluc, by = linf, h = 10), 
  data = infect, family = "binomial")

summary(m3)

# Plot both linear and non linear 
# components of nonparametric functions: composed = FALSE
par(mfrow = c(1,3))
plot(m3, composed = FALSE)

# Personalized plots
# First obtain predictions in new data
# Create newdata for prediction
ngrid <- 30
gluc0 <- seq(50, 190, length = ngrid)
linf0 <- seq(0, 45, length = ngrid)
df  <- expand.grid(gluc = gluc0, linf = linf0)

m3p <- predict(m3, newdata = df)

par(mfrow = c(1,2))
ii <- order(df[,"gluc"])

## Parametric coefficients
names(m3p$coeff)

# Nonlinear components
colnames(m3p$peffects) 

# Include the linear component
plot(df[ii,"gluc"], m3p$coeff[["gluc"]]*df[ii,"gluc"] + 
  m3p$peffects[ii,"sb(gluc, h = 10)"], 
  type = 'l', xlab = "Glucose (mg/dl)", ylab = "f_1(gluc)", 
  main = "Nonparametric effect of Glucose")

# Include the linear component
plot(df[ii,"gluc"], m3p$coeff[["gluc:linf"]]*df[ii,"gluc"] + 
  m3p$peffects[ii,"sb(gluc, h = 10, by = linf)"], 
  type= 'l', xlab = "Glucose (mg/dl)", ylab = "f_2(gluc)",
  main = "Varying coefficients as a function of Glucose")

# Countour plot of the probability of post-opererational infection
n <- sqrt(nrow(df))
Z <- matrix(m3p$pfitted.values, n, n)
filled.contour(z = Z, x = gluc0, y = linf0, 
  xlab = "Glucose (mg/dl)", ylab = "Lymphocytes (\%)",
  main = "Probability of post-opererational infection")

par(op)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
