% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correct_iseli.R
\name{correct_iseli}
\alias{correct_iseli}
\title{Return corrected harmonic amplitudes}
\usage{
correct_iseli(f, fx, bx, fs)
}
\arguments{
\item{f}{Vector of frequency values of approximate harmonic locations.}

\item{fx}{Vector of frequency values of formants.}

\item{bx}{Vector of frequency values of bandwidths.}

\item{fs}{Sampling rate of the original sound file.}
}
\value{
A numeric vector to be subtracted from the estimated harmonic
amplitudes.
}
\description{
Correction of harmonic amplitudes for the influence of lower formants.
}
\examples{
# not now
}
