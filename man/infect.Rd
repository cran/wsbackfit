\name{infect}
\alias{infect}
\docType{data}
\title{
Postoperative Infection Data.
}
\description{
Data from a prospective study conducted at the University Hospital of Santiago de Compostela (Spain). A total of 2318 patients who underwent surgery at this center between January 1996 and March 1997 were characterized post-operatively, in respect of whether they suffered or not post-operative infection.
}
\usage{data(infect)}
\format{
  A data frame with 2318 observations on the following 6 variables.
  \describe{
  	\item{\code{age}}{patient's age.}
  	\item{\code{sex}}{patient's sex. Coded as  1 = \code{Man} and 2 = \code{Woman}.}
  	\item{\code{linf}}{lymphocytes (expressed as relative counts (in \%) of the white blood cell count)}
  	\item{\code{gluc}}{plasma glucose concentration (measured in mg/dl)}
    \item{\code{diab}}{diabetes. Coded as 1 = presence and 2 = absence.}
    \item{\code{inf}}{variable indicating whether the patient suffered (inf = 1) or not (inf = 0) a post-operative infection.}
  }
}

%\source{}
%\references{}
\examples{
data(infect)
summary(infect)
}
\keyword{datasets}


