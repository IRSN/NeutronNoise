% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/feynman_hist_plot.R
\name{plot.feynman_hist}
\alias{plot.feynman_hist}
\title{Plot feynman histograms}
\usage{
\method{plot}{feynman_hist}(x, ...)
}
\arguments{
\item{x}{data.frame from \link[NeutronNoise]{feynman_hist} functions.}

\item{...}{Unused.}
}
\value{
ggplot object.
}
\description{
Plot feynman histograms from \code{\link[NeutronNoise]{feynman_hist}} function. The assumed time unit is second.
}
\examples{
as_signal(runif(0,10, n=10000), x_duration = 10) \%>\% feynman_hist(c(0.11, 0.33, 0.58)) \%>\% plot()

}
