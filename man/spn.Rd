\name{spn}
\alias{sp}
\docType{data}
\title{
Human violence and animal abuse notifications in Sao Paulo city.
}
\description{
Modified notifications of human violence and animal abuse in Sao Paulo city.
}
\usage{data(sp)}
\format{
  A data frame with 96 observations on the following 5 variables.
  \describe{
    \item{\code{id}}{District ID.}
    \item{\code{district}}{District name.}
    \item{\code{aan}}{Number of animal abuse notifications.}
    \item{\code{eaan}}{Expected number of animal abuse notifications.}
    \item{\code{shvn}}{Scaled human violence notifications.}
  }
}
\examples{
data(sp)
str(spn)
head(spn)
}
\keyword{datasets}
