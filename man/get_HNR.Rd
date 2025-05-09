% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_HNR.R
\name{get_HNR}
\alias{get_HNR}
\title{Estimate harmonics-to-noise ratio}
\usage{
get_HNR(
  filelist,
  inputDir,
  uprFreqPass,
  f0min = 50,
  f0max = 300,
  intervalFixed = 0.005,
  beginTime = 0,
  output = NULL,
  praatsauce_output = NULL
)
}
\arguments{
\item{filelist}{Vector of strings giving the names of sound files to analyze.}

\item{inputDir}{String giving the directory where sound files are located.}

\item{uprFreqPass}{Numeric giving the highest upper frequency of the
bandpass filter applied to the sound file prior to analysis.}

\item{f0min}{Integer giving the pitch floor. Default is `50`.}

\item{f0max}{Integer giving the pitch ceiling. Default is `300`.}

\item{intervalFixed}{Numeric; how often should measures be taken (in
seconds)? Default is `0.005`.}

\item{beginTime}{Numeric; where should the first measure be taken (in
seconds)? Default is `0`.}

\item{output}{Optional data frame containing existing measures. Used to
ensure an equal number of rows if compiling a larger data frame with other
measures.}

\item{praatsauce_output}{Optional data frame containing existing measures
from PraatSauce. Used to ensure an equal number of rows.}
}
\value{
A data frame with HNR values.
}
\description{
Estimate harmonics-to-noise ratio of a list of sound files using Boersma's
autocorrelation method.
}
\examples{
datapath <- system.file('extdata/audio', package='sauceshelf')
fls <- list.files(datapath, pattern='.wav')
hnr <- get_HNR(fls, datapath, 500)
}
