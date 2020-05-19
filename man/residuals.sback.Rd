\name{residuals.sback}
\alias{residuals.sback}
\title{
sback residuals
}
\description{
Returns residuals for a fitted \code{sback} object. Deviance, pearson, working and response residuals are available.
}
\usage{
\method{residuals}{sback}(object, type = c("deviance", "pearson", "working", "response"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{an object of class \code{sback} as produced by \code{\link{sback}}.}
  \item{type}{the type of residuals which should be returned: "deviance" (default), "pearson", "working" and "response".}
  \item{\dots}{further arguments passed to or from other methods. Not yet implemented.}
}
\details{
For details see \code{\link{residuals.glm}}.
}
\value{Numeric vector with the residuals.}
\author{Javier Roca-Pardinas, Maria Xose Rodriguez-Alvarez and Stefan Sperlich}
\seealso{
\code{\link{sback}}, \code{\link{summary.sback}}, \code{\link{plot.sback}}.
}
\examples{
library(wsbackfit)
data(infect)

# Generalized varying coefficient model with binary response
m3 <- sback(formula = inf ~ sb(gluc, h = 10) + sb(gluc, by = linf, h = 10), 
  data = infect, family = "binomial")

summary(m3)

# Deviance
summary(residuals(m3))

# Pearson
summary(residuals(m3, type = "pearson"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
