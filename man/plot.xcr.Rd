% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/xcr_plot.R
\name{plot.xcr}
\alias{plot.xcr}
\title{Plot count rates}
\usage{
\method{plot}{xcr}(x, ...)
}
\arguments{
\item{x}{data.frame from \link[NeutronNoise]{xcr} functions.}

\item{...}{Unused.}
}
\value{
ggplot object.
}
\description{
Plot count rates from \code{\link[NeutronNoise]{xcr}} function. The assumed time unit is second.
Plot 4 graphs for Y1, Y2, Y3 and Y.
}
\examples{
artificial_signal(1000, 5) \%>\% feynman_hist(samples_widths = c(0.8, 0.9)) \%>\% xcr() \%>\% plot()

}
