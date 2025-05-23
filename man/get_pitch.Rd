% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_pitch.R
\name{get_pitch}
\alias{get_pitch}
\title{Estimate pitch}
\usage{
get_pitch(
  filelist,
  inputDir,
  intervalFixed = 0.005,
  f0min = 50,
  f0max = 300,
  beginTime = 0,
  praatsauce_output = NULL
)
}
\arguments{
\item{filelist}{Vector of strings giving the names of sound files to analyze.}

\item{inputDir}{String giving the directory where sound files are located.}

\item{intervalFixed}{Numeric; how often should measures be taken (in
seconds)? Default is `0.005`.}

\item{f0min}{Integer giving the pitch floor. Default is `50`.}

\item{f0max}{Integer giving the pitch ceiling. Default is `300`.}

\item{beginTime}{Numeric; where should the first measure be taken (in
seconds)? Default is `0`.}

\item{praatsauce_output}{Optional data frame containing existing measures
from PraatSauce. Used to ensure an equal number of rows.}
}
\value{
A data frame with pitch values.
}
\description{
Estimate pitch of sound files using the Schäffler-Vincent algorithm.
Wrapper for [wrassp::ksvF0] returning a data frame.
}
\examples{
#datapath <- system.file('extdata/audio', package='sauceshelf')
#fls <- list.files(datapath, pattern='.wav')
#f0 <- get_pitch(fls, datapath)
}
