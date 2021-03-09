\name{wsbackfit-package}
\alias{wsbackfit-package}
\alias{wsbackfit}
\docType{package}
\title{
\packageTitle{wsbackfit}
}
\description{
Non- and semiparametric regression for generalized additive, partial linear, and varying coefficient models as well as their combinations. Specifically, the package provides estimation procedures for a large class of regression models common in applied statistics. The regression models belong to the class of the so-called generalized structured models, i.e.,
\deqn{E[Y|X,Z] = G(g_0 + \sum_j g_j (X_j) Z_j  + Z_k^{'}\beta).}
Note that, up to identification restrictions specified e.g. in Park and Mammen (2006), several of the \eqn{X_j} and \eqn{Z_j} can refer to the same variable. For example, all \eqn{X_j} may be the same but all \eqn{Z_j} different.

The estimation procedure is based on smoothed backfitting which to our knowledge is the statistically most efficient existing procedure for this model class. Additional weights allow sampling weights, trimming, or efficient estimation under heteroscedasticity. This package also allows to either set the bandwidths or automatically select them using k-fold cross-validation. The option 'offset' facilitates the application of smooth backfitting on aggregated data.   
}
\details{
\tabular{ll}{
Package: \tab wsbackfit\cr
Type: \tab Package\cr
Version: \tab 1.0-4\cr
Date: \tab 2021-03-09\cr
License: \tab GPL\cr
}
}
\author{
Javier Roca-Pardinas, Maria Xose Rodriguez-Alvarez, Stefan Sperlich

Maintainer: Javier Roca-Pardinas <roca@uvigo.es>
}

\references{
Han, K. and Park B.U. (2018). Smooth backfitting for errors-in-variables additive models. Annals of Statistics, 46, 2216-2250.

Lee, Y.K., Mammen, E. and Park, B.U. (2012). Flexible generalized varying coefficient regression models. The Annals of Statistics, 40(3), 1906-1933.

Mammen, E. and Nielsen, J. (2003). Generalised structured models. Biometrika, 90, 551-566.

Mammen, E. Linton, O. and Nielsen, J. (1999). The existence and asymptotic properties of a backfitting projection algorithm under weak conditions. Annals of Statistics, 27 (5), 1443-1490.

Mammen, E. and Park, B.U. (2006). A simple smooth backfitting method for additive models. Annals of Statistics, 34 (5), 2252-2271.

Nielsen. J. and Sperlich, S. (2005). Smooth backfitting in practice. Journal of the Royal Statistical Society, B, 67, 43-61.

Roca-Pardinas, J. and Sperlich, S. (2010). Feasible Estimation in Generalized Structured Models. Statistics and Computing, 20, 367-379.
}