#' Estimate harmonic amplitudes and spectral slope measures
#'
#' Based on provided pitch and formant measures, this function estimates a
#' commonly used range of harmonic amplitude and spectral slope measures
#' internally in R. Spectra are generated with Blackman windows using
#' [wrassp::dftSpectrum].
#'
#' @param filelist Vector of strings giving the names of sound files to analyze.
#' @param inputDir String giving the directory where sound files are located.
#' @param f0 Data frame consisting of at least a column `file` with sound file
#' names, a column `t` with time in seconds, and a column `f0` with pitch
#' measures.
#' @param fmt Data frame consisting of at least a column `file` with sound file
#' names, a column `t` with time in seconds, and columns `F1`, `F2`, and `F3`
#' with formant measurements.
#' @param bw Data frame consisting of at least a column `file` with sound file
#' names, a column `t` with time in seconds, and columns `B1`, `B2`, and `B3`
#' with formant bandiwdths estimations.
#' @param intervalFixed Numeric; how often should measures be taken (in
#' seconds)? Default is `0.005`.
#' @param windowLength Numeric giving the length of the analysis window (in
#' seconds). Default is `0.025`.
#' @param slope Boolean; should spectral slope measures be returned? Default
#' is `TRUE`.
#' @param slopeUncorrected Boolean; should uncorrected spectral slope measures
#' be returned? Default is `TRUE`.
#' @param beginTime Numeric; where should the first measure be taken (in
#' seconds)? Default is `0`.
#' @param praatsauce_output Optional data frame containing existing measures
#' from PraatSauce. Used to ensure an equal number of rows.
#'
#' @return A data frame with harmonic amplitudes and spectral slope measures.
#' @export
#'
#' @examples
#' # not now
get_spectralMeasures <- function(filelist, inputDir, f0, fmt, bw,
                                 intervalFixed = 0.005, windowLength = 0.025,
                                 slope = TRUE, slopeUncorrected = TRUE,
                                 beginTime = 0, praatsauce_output = NULL) {

  out <- data.frame(file = NA, t = NA, H1u = NA, H2u = NA, H4u = NA, A1u = NA,
                    A2u = NA, A3u = NA, H2Ku = NA, H5Ku = NA, H1c = NA,
                    H2c = NA, H4c = NA, A1c = NA, A2c = NA, A3c = NA)
  specbw <- (1 / windowLength) / 2

  for (f in filelist) {
    fn <- paste0(inputDir, '/', f)
    specObj <- wrassp::dftSpectrum(fn, toFile=F, verbose=F,
                                windowShift = intervalFixed * 1000,
                                resolution = 1 / windowLength,
                                beginTime = beginTime)
    spec <- specObj$dft
    freqSteps <- dim(spec)[2]
    sr <- attr(specObj, 'origFreq')
    t <- seq(attr(specObj, 'startTime'),
             attr(specObj, 'endRecord') * intervalFixed,
             by = intervalFixed)
    freqRange <- seq(0, sr/2, length.out=freqSteps)
    oneFreqStep <- freqRange[2] - freqRange[1]
    f0tmp <- f0[which(f0$file==f),'f0']
    fmttmp <- fmt[which(fmt$file==f),]
    bwtmp <- bw[which(bw$file==f),]
    nFrames <- length(f0tmp)
    H1u <- H2u <- H4u <- A1u <- A2u <- A3u <- H2Ku <- H5Ku <- rep(NA, nFrames)
    for (i in 1:nFrames) {
      if (!is.na(f0tmp[i]) & f0tmp[i] > 0) {
        if ((f0tmp[i] * 0.1) * 2 < oneFreqStep) {
          mult <- oneFreqStep / f0tmp[i]
        } else {
          mult <- 0.1
        }

        H1u[i] <- max(spec[i,which(freqRange > f0tmp[i] - (f0tmp[i]*mult) &
                                      freqRange < f0tmp[i] + (f0tmp[i]*mult))])
        H2u[i] <- max(spec[i,which(freqRange > (f0tmp[i]*2) - (f0tmp[i]*mult) &
                                      freqRange < (f0tmp[i]*2) +
                                     (f0tmp[i]*mult))])
        H4u[i] <- max(spec[i,which(freqRange > (f0tmp[i]*4) - (f0tmp[i]*mult) &
                                      freqRange < (f0tmp[i]*4) +
                                     (f0tmp[i]*mult))])
        H2Ku[i] <- max(spec[i,which(freqRange > 2000 - f0tmp[i],
                                     freqRange < 2000 + f0tmp[i])])
        H5Ku[i] <- max(spec[i,which(freqRange > 5000 - f0tmp[i],
                                     freqRange < 5000 + f0tmp[i])])
        if (!is.na(fmttmp$F1[i]) & fmttmp$F1[i] > 0) {
          A1u[i] <- max(spec[i,which(freqRange > fmttmp$F1[i] -
                                        (fmttmp$F1[i]*0.2) &
                                        freqRange < fmttmp$F1[i] +
                                        (fmttmp$F1[i]*0.2))])
        }
        if (!is.na(fmttmp$F2[i]) & fmttmp$F2[i] > 0) {
          A2u[i] <- max(spec[i,which(freqRange > fmttmp$F2[i] -
                                        (fmttmp$F2[i]*0.1) &
                                        freqRange < fmttmp$F2[i] +
                                        (fmttmp$F2[i]*0.1))])
        }
        if (!is.na(fmttmp$F3[i]) & fmttmp$F3[i] > 0) {
          A3u[i] <- max(spec[i,which(freqRange > fmttmp$F3[i] -
                                        (fmttmp$F3[i]*0.1) &
                                        freqRange < fmttmp$F3[i] +
                                        (fmttmp$F3[i]*0.1))])
        }
      }
    }
    H1c <- H1u - correct_iseli(f0tmp, fmttmp$F1, bwtmp$B1, sr) -
      correct_iseli(f0tmp, fmttmp$F2, bwtmp$B2, sr)
    H2c <- H2u - correct_iseli(f0tmp*2, fmttmp$F1, bwtmp$B1, sr) -
      correct_iseli(f0tmp*2, fmttmp$F2, bwtmp$B2, sr)
    H4c <- H4u - correct_iseli(f0tmp*4, fmttmp$F1, bwtmp$B1, sr) -
      correct_iseli(f0tmp*4, fmttmp$F2, bwtmp$B2, sr)
    A1c <- A1u - correct_iseli(fmttmp$F1, fmttmp$F1, bwtmp$B1, sr) -
      correct_iseli(fmttmp$F1, fmttmp$F2, bwtmp$B2, sr)
    A2c <- A2u - correct_iseli(fmttmp$F2, fmttmp$F1, bwtmp$B1, sr) -
      correct_iseli(fmttmp$F2, fmttmp$F2, bwtmp$B2, sr)
    A3c <- A3u - correct_iseli(fmttmp$F3, fmttmp$F1, bwtmp$B1, sr) -
      correct_iseli(fmttmp$F3, fmttmp$F2, bwtmp$B2, sr) -
      correct_iseli(fmttmp$F3, fmttmp$F3, bwtmp$B3, sr)

    if (length(t) > length(H1u)) t <-
      t[-seq(length(t), length(H1u) + 1)]
    tmp <- data.frame(file = rep(f, length(t)),
                      t = t, H1u = H1u, H2u = H2u, H4u = H4u, A1u = A1u,
                      A2u = A2u, A3u = A3u, H2Ku = H2Ku, H5Ku = H5Ku,
                      H1c = H1c, H2c = H2c, H4c = H4c, A1c = A1c, A2c = A2c,
                      A3c = A3c)
    if (!is.null(praatsauce_output)) {
      psFile <- praatsauce_output[praatsauce_output$file == f,]
      tmp <- tmp[1:nrow(psFile),]
    }

    out <- rbind(out, tmp)
  }
  if (slope) {
    out$H1H2c <- out$H1c - out$H2c
    out$H2H4c <- out$H2c - out$H4c
    out$H1A1c <- out$H1c - out$A1c
    out$H1A2c <- out$H1c - out$A2c
    out$H1A3c <- out$H1c - out$A3c
    out$H2KH5Ku <- out$H2Ku <- out$H5Ku
  }
  if (slopeUncorrected) {
    out$H1H2u <- out$H1u - out$H2u
    out$H2H4u <- out$H2u - out$H4u
    out$H1A1u <- out$H1u - out$A1u
    out$H1A2u <- out$H1u - out$A2u
    out$H1A3u <- out$H1u - out$A3u
  }
  out <- out[-1,]
  return(out)
}
