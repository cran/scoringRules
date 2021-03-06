% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scores.R
\name{scores}
\alias{scores}
\alias{crps}
\alias{logs}
\title{Generic Scoring Rule Calculation}
\usage{
crps(y, ...)

logs(y, ...)
}
\arguments{
\item{y}{an object for which the score is to be calculated}

\item{...}{further arguments passed to or from other methods}
}
\value{
Returns a vector of scores. One for each forecast-observation pair.
}
\description{
Generic functions for calculating the Continuous Ranked Probability Score
and the Logarithmic Score of R objects.

\code{scoringRules} provides default methods
(\code{\link{crps.numeric}}, \code{\link{logs.numeric}}) to calculate scores of forecasts
that are members of families of parametric distributions.
}
\details{
The mean logarithmic score corresponds to the negative of the
log-likelihood \code{\link{logLik}}.
}
\references{
\emph{General background and further references on scoring rules:}

Gneiting, T. and A.E. Raftery (2007):
`Strictly proper scoring rules, prediction and estimation',
Journal of the American Statistical Association 102, 359-378. \doi{10.1198/016214506000001437}

Gneiting, T. and M. Katzfuss (2014):
`Probabilistic forecasting',
Annual Review of Statistics and Its Application 1, 125-151. \doi{10.1146/annurev-statistics-062713-085831}
}
\seealso{
\code{\link{crps.numeric}}, \code{\link{logs.numeric}}
}
