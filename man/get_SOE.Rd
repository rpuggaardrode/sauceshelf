% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_SOE.R
\name{get_SOE}
\alias{get_SOE}
\title{Estimate strength of harmonic excitation}
\usage{
get_SOE(
  filelist,
  inputDir,
  intervalFixed = 0.005,
  beginTime = 0,
  f0min = 50,
  f0max = 300,
  f0 = NULL,
  praatsauce_output = NULL
)
}
\arguments{
\item{filelist}{Vector of strings giving the names of sound files to analyze.}

\item{inputDir}{String giving the directory where sound files are located.}

\item{intervalFixed}{Numeric; how often should measures be taken (in
seconds)? Default is `0.005`.}

\item{beginTime}{Numeric; where should the first measure be taken (in
seconds)? Default is `0`.}

\item{f0min}{Integer giving the pitch floor. Default is `50`.}

\item{f0max}{Integer giving the pitch ceiling. Default is `300`.}

\item{f0}{Data frame consisting of at least a column `file` with sound file
names, a column `t` with time in seconds, and a column `f0` with pitch
measures. Used to estimate the length of the moving average filter; default
is `NULL`, if no data frame is provided, pitch is calculated using
[wrassp::ksvF0].}

\item{praatsauce_output}{Optional data frame containing existing measures
from PraatSauce. Used to ensure an equal number of rows.}
}
\value{
A data frame with SOE values.
}
\description{
Estimates the strength of harmonic excitation in sound files, by passing
a differenced version of the signal through a cascade of two zero frequency
filters, removing the moving average at each step.
}
\examples{
datapath <- system.file('extdata/audio', package='sauceshelf')
fls <- list.files(datapath, pattern='.wav')
soe <- get_SOE(fls, datapath)
}
