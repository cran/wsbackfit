\name{predict.sback}
\alias{predict.sback}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Predict method for sback fits
}
\description{
Predicted smooth functions and values based on an sback object
}
\usage{
\method{predict}{sback}(object, newdata, newoffset = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class \code{\link{sback}}.}
  \item{newdata}{a data frame containing the values of the covariates at which predictions are wanted. If not provided then the predictions correspond to the original data.}
  \item{newoffset}{an optional numerical vector containing an a priori known component to be included in the linear predictor for the predictions (offset associated with the newdata).}
  \item{\dots}{not yet implemented.}
}
%\details{
%}
\value{
A list with the following components:
\item{pdata}{the original supplied \code{newdata} argument.}
\item{poffset}{the original supplied \code{newoffset} argument.}
\item{coeff}{a numeric vector with the estimated coefficients. This vector includes both the parametric effects as well as the coefficients associated with the linear component of the nonparametric functions.}
\item{peffects}{matrix with the estimated nonparametric functions (only the nonlinear component) for each covariate value in the original supplied newdata.}
\item{pfitted.values}{a numeric vector with the fitted values for the supplied newdata.}
}
%\references{}
\author{Javier Roca-Pardinas, Maria Xose Rodriguez-Alvarez and Stefan Sperlich}
%\note{}
\seealso{
\code{\link{sback}}, \code{\link{summary.sback}}
}
\examples{
library(wsbackfit)
data(infect)

# Generalized varying coefficient model with binary response
m3 <- sback(formula = inf ~ sb(gluc, h = 10) + sb(gluc, by = linf, h = 10), 
  data = infect, family = "binomial", kbin = 15)

summary(m3)

# Plot both linear and non linear 
# components of nonparametric functions: composed = FALSE
op <- par(no.readonly = TRUE)
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