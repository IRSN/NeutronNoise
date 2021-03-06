% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{feynman_v2m}
\alias{feynman_v2m}
\title{Feynman Variance-to-Mean}
\usage{
feynman_v2m(x, samples_widths, max_nb_samples = 0L, verbose = 0L)
}
\arguments{
\item{x}{A signal. The function assumes that the signal starts at the time 0 seconds.}

\item{samples_widths}{Numeric vector of samples widths.}

\item{max_nb_samples}{Maximum number of samples to take into account for the calculation. 
Unlimited if 0.}

\item{verbose}{For debugging purpose.}
}
\value{
A data.frame containing for each samples width :
- The number of samples used
- The count rate "Y1"
- The Feynamn Variance-to-Mean value "Y"
- The Feynman Variance-to-Mean standard deviation "Y_std"
}
\description{
Calculation of the Feynman curve from the ratio between the variance and 
the mean minus 1 of the number of detections in consecutive samples.
Variance is calculated using Bessel's correction.
}
\examples{
mls <- data.frame(nu = 0:4, pdf = c(0.1,0.3,0.35,0.4,0.2))
artificial_signal(1000, hists_rate = 5000, fission_multiplicity = mls, 
k = 0.9, lambda = 10) \%>\%
feynman_v2m(samples_widths = lseq(from = 0.001, to = 10, 50)) \%>\% plot()

}
