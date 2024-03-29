\name{GDP data}
\alias{gdp}
\alias{gdp_mcmc}
\docType{data}
\keyword{datasets}
\title{
Data and forecasts for US GDP growth
}
\description{
Historical data and forecast distributions for the growth rate of US gross domestic product (GDP). The forecasts are generated from a Bayesian Markov Switching model as described in Section 5 of KLTG (2021). 
}

\format{
\code{gdp} is a data frame which contains the real-time data set used in Section 5 of KLTG (2021), with the following columns:
\itemize{
\item \code{dt} - date in question (e.g., \code{"2013Q2"} for the second quarter of 2013)
\item \code{vint} - data vintage (i.e., the date at which the realization was recorded); same format as \code{dt}
\item \code{val} - value of the GDP growth rate
}

\code{gdp_mcmc} is a list, whereby each element is a data frame. \code{gdp_mcmc$forecasts} contains the simulated forecast distributions. There are 20 columns (corresponding to quarters 2008:Q1 to 2012:Q4) and 5.000 rows (corresponding to simulation draws). \code{gdp_mcmc$actuals} contains the actual observations. There are 20 columns (again corresponding to quarterly dates) and a single row. 
}

\source{
The GDP growth rate is computed from real-time data provided by the Federal Reserve Bank of Philadelphia, \url{https://www.philadelphiafed.org/surveys-and-data/real-time-data-research/real-time-data-set-for-macroeconomists} (series code ``ROUTPUT'', second-vintage data). The same data also enters the model which is used to generate the forecast distribution. \emph{Disclaimer: The provider of the raw data takes no responsibility for the accuracy of the data posted here. Furthermore, the raw data may be revised over time, and the website linked above should be consulted for the official, most recent version.}

The model from which the forecast draws are generated is described in Section 5 of KLTG (2021). Forecasts are one quarter ahead (that is, they are based on data until the previous quarter).

}
\details{
The realizations in \code{gdp_mcmc$actuals} are also contained in \code{gdp}, based on the second available vintage for each date. For example, \code{gdp_mcmc$actuals$X2008Q1} is the entry in \code{gdp} for which \code{dt == "2008Q1"} and \code{vint == "2008Q3"}.
}

\examples{
\dontrun{

# Load data
data(gdp_mcmc)

# Histogram of forecast draws for 2012Q4
fc_draws <- gdp_mcmc$forecasts[, "X2012Q4"]
hist(fc_draws, main = "Forecast draws for 2012:Q4", xlab = "Value")

# Add vertical line at realizing value
rlz <- gdp_mcmc$actuals[, "X2012Q4"]
abline(v = rlz, lwd = 3)

# Compute CRPS for this forecast case
crps_sample(y = rlz, dat = fc_draws)

}
}

\references{

Krueger, F., Lerch, S., Thorarinsdottir, T.L. and T. Gneiting (2021): `Predictive inference based on Markov chain Monte Carlo output', \emph{International Statistical Review} 89, 274-301. \doi{10.1111/insr.12405}

}
