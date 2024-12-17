#' Voice quality measures in R
#'
#' Estimates a range of voice quality measures over all sound files in a
#' directory relying mainly on functions from the EMU-SDMS libraries.
#'
#' @param inputDir String giving a directory containing sound files to analyze.
#' @param pitch Boolean; should pitch values be returned? Default is `TRUE`.
#' @param formant Boolean; should formant values be returned? Default is `TRUE`.
#' @param bw Boolean; should formant bandwidths be returned? Default is `TRUE`.
#' @param harmonicAmplitude Boolean; should corrected harmonic amplitudes be
#' returned? Default is `TRUE`.
#' @param slope Boolean; should corrected spectral slope values be returned?
#' Default is `TRUE`.
#' @param harmonicAmplitudeUncorrected Boolean; should uncorrected harmonic
#' amplitudes be returned? Default is `TRUE`.
#' @param slopeUncorrected Boolean; should uncorrected spectral slope values
#' be returned? Default is `TRUE`.
#' @param cpp Boolean; should cepstral peak prominence values be returned?
#' Default is `TRUE`.
#' @param hnr Boolean; should harmonics-to-noise ratios be returned? Default
#' is `TRUE`.
#' @param intensity Boolean; should root-mean-squared intensity values be
#' returned? Default is `TRUE`.
#' @param soe Boolean; should strength of excitation values be returned?
#' Default is `TRUE`.
#' @param intervalFixed Numeric; how often should measures be taken (in
#' seconds)? Default is `0.005`.
#' @param f0min Integer giving the pitch floor. Default is `50`.
#' @param f0max Integer giving the pitch ceiling. Default is `300`.
#' @param windowLength Numeric giving the length of the analysis window (in
#' seconds). Default is `0.025`.
#' @param maxNumFormants Integer giving the maximum number of formants to
#' estimate. Default is `5`.
#' @param bw_hawksMiller Boolean; should bandwidths be estimated using the
#' Hawks-Miller formula? Default is `TRUE`. If `FALSE`, raw bandwidths are
#' returned.
#' @param praatsauce_output Optional data frame containing existing measures
#' from PraatSauce.
#'
#' @return Data frame with the specified measures.
#' @export
#'
#' @examples
#' datapath <- system.file('extdata/audio', package='sauceshelf')
#' sauce <- emusauce(datapath)
emusauce <- function(inputDir, pitch = TRUE, formant = TRUE, bw = TRUE,
                     harmonicAmplitude = TRUE, slope = TRUE,
                     harmonicAmplitudeUncorrected = TRUE,
                     slopeUncorrected = TRUE, cpp = TRUE, hnr = TRUE,
                     intensity = TRUE, soe = TRUE,
                     intervalFixed = 0.005, f0min = 50, f0max = 300,
                     windowLength = 0.025, maxNumFormants = 5,
                     bw_hawksMiller = TRUE,
                     praatsauce_output = NULL) {

  pitchReq <- FALSE
  fmtReq <- FALSE
  specReq <- FALSE
  out <- NULL

  if (pitch + bw_hawksMiller + harmonicAmplitude + slope +
      soe > 0) pitchReq <- TRUE
  if (formant + bw + bw_hawksMiller + harmonicAmplitude +
      slope > 0) fmtReq <- TRUE
  if (bw + harmonicAmplitude + slope > 0) bwReq <- TRUE
  if (harmonicAmplitude + slope + harmonicAmplitudeUncorrected +
      slopeUncorrected > 0) specReq <- TRUE

  if (maxNumFormants < 4) stop('maxNumFormants should be 4 or higher')

  fls <- list.files(inputDir, pattern='*.wav')
  if (!is.null(praatsauce_output)) {
    beginTime <- praatsauce_output$t[1]
    out <- praatsauce_output
    outExists <- TRUE
  } else {
    beginTime <- 0
    outExists <- FALSE
  }

  if (pitchReq) {
    if (!'f0' %in% colnames(praatsauce_output)) {
      f0 <- get_pitch(fls, inputDir, intervalFixed, f0min, f0max,
                      beginTime, praatsauce_output)
    } else {
      f0 <- data.frame(file = praatsauce_output$file,
                       t = praatsauce_output$t,
                       f0 = praatsauce_output$f0)
    }
    if (pitch) {
      if (outExists) {
        out$f0 <- f0$f0
      } else {
        out <- f0
        outExists <- TRUE
      }
    }
  }

  if (fmtReq) {
    if (!'F1' %in% colnames(praatsauce_output)) {
      fmt <- get_formants(fls, inputDir, intervalFixed,  windowLength,
                          !bw_hawksMiller, maxNumFormants,
                          beginTime, praatsauce_output)
    } else {
      fmt <- data.frame(file = praatsauce_output$file,
                        t = praatsauce_output$t,
                        F1 = praatsauce_output$F1,
                        F2 = praatsauce_output$F2,
                        F3 = praatsauce_output$F3)
    }
    if (formant) {
      if (outExists) {
        out$F1 <- fmt$F1; out$F2 <- fmt$F2; out$F3 <- fmt$F3
      } else {
        out <- fmt
        outExists <- TRUE
      }
    }
  }

  if (bwReq) {
    if (bw_hawksMiller) {
      B1 <- get_bwHawksMiller(f0$f0, fmt$F1)
      B2 <- get_bwHawksMiller(f0$f0, fmt$F2)
      B3 <- get_bwHawksMiller(f0$f0, fmt$F3)
    } else {
      B1 <- fmt$B1; B2 <- fmt$B2; B3 <- fmt$B3
    }
    bwdf <- data.frame(file = fmt$file, B1 = B1, B2 = B2, B3 = B3)
    if (bw) {
      if (outExists) {
        out$B1 <- B1; out$B2 <- B2; out$B3 <- B3
      } else {
        out <- fmt[,-(which(colnames(fmt) %in% c('F1', 'F2', 'F3')))]
        out$B1 <- B1; out$B2 <- B2; out$B3 <- B3
        outExists <- TRUE
      }
    }
  }

  if (specReq) {
    spec <- get_spectralMeasures(fls, inputDir, f0, fmt, bwdf,
                                 intervalFixed, windowLength, slope,
                                 slopeUncorrected, beginTime, praatsauce_output)
    if (harmonicAmplitude) {
      if (outExists) {
        out$H1c <- spec$H1c; out$H2c <- spec$H2c; out$H4c <- spec$H4c
        out$A1c <- spec$A1c; out$A2c <- spec$A2c; out$A3c <- spec$A3c
        out$H2Ku <- spec$H2Ku; out$H5Ku <- spec$H5Ku
      } else {
        out <- spec[,(which(colnames(spec) %in% c('file', 't', 'H1c', 'H2c',
                                                  'H4c', 'A1c', 'A2c', 'A3c',
                                                  'H2Ku', 'H5Ku')))]
        outExists <- TRUE
      }
    }
    if (harmonicAmplitudeUncorrected) {
      if (outExists) {
        out$H1u <- spec$H1u; out$H2u <- spec$H2u; out$H4u <- spec$H4u
        out$A1u <- spec$A1u; out$A2u <- spec$A2u; out$A3u <- spec$A3u
      } else {
        out <- spec[,(which(colnames(spec) %in% c('file', 't', 'H1u', 'H2u',
                                                  'H4u', 'A1u', 'A2u', 'A3u')))]
        outExists <- TRUE
      }
    }
    if (slope) {
      if (outExists) {
        out$H1H2c <- spec$H1H2c; out$H2H4c <- spec$H2H4c
        out$H1A1c <- spec$H1A1c; out$H1A2c <- spec$H1A2c
        out$H1A3c <- spec$H1A3c; out$H2KH5Ku <- spec$H2KH5Ku
      } else {
        out <- spec[,(which(colnames(spec) %in% c('file', 't', 'H1H2c', 'H2H4c',
                                                  'H1A1c', 'H1A2c', 'H1A3c',
                                                  'H2KH5Ku')))]
        outExists <- TRUE
      }
    }
    if (slopeUncorrected) {
      if (outExists) {
        out$H1H2u <- spec$H1H2u; out$H2H4u <- spec$H2H4u
        out$H1A1u <- spec$H1A1u; out$H1A2u <- spec$H1A2u
        out$H1A3u <- spec$H1A3u
      } else {
        out <- spec[,(which(colnames(spec) %in% c('file', 't', 'H1H2u', 'H2H4u',
                                                  'H1A1u', 'H1A2u', 'H1A3u')))]
        outExists <- TRUE
      }
    }
  }

  if (cpp) {
    cpp <- get_CPP(fls, inputDir, windowLength, f0min, f0max, intervalFixed,
                   beginTime, praatsauce_output)
    if (outExists) {
      out$CPP <- cpp$CPP
    } else {
      out <- cpp
      outExists <- TRUE
    }
  }

  if (hnr) {
    hnr05 <- get_HNR(fls, inputDir, 500, f0min, f0max, intervalFixed,
                     beginTime, out, praatsauce_output)
    hnr15 <- get_HNR(fls, inputDir, 1500, f0min, f0max, intervalFixed,
                     beginTime, out, praatsauce_output)
    hnr25 <- get_HNR(fls, inputDir, 2500, f0min, f0max, intervalFixed,
                     beginTime, out, praatsauce_output)
    hnr35 <- get_HNR(fls, inputDir, 3500, f0min, f0max, intervalFixed,
                     beginTime, out, praatsauce_output)
    if (outExists) {
      out$HNR05 <- hnr05$HNR500; out$HNR15 <- hnr15$HNR1500
      out$HNR25 <- hnr25$HNR2500; out$HNR35 <- hnr35$HNR3500
    } else {
      out <- hnr05
      colnames(out)[ncol(out)] <- 'HNR05'
      out$HNR15 <- hnr15$HNR1500
      out$HNR25 <- hnr25$HNR2500; out$HNR35 <- hnr35$HNR3500
    }
  }

  if (intensity) {
    rms <- get_intensity(fls, inputDir, intervalFixed, f0min,
                         beginTime, praatsauce_output)
    if (outExists) {
      out$intensity <- rms$intensity
    } else {
      out <- rms
      outExists <- TRUE
    }
  }

  if (soe) {
    soe <- get_SOE(fls, inputDir, intervalFixed, beginTime, f0min, f0max,
                   f0, praatsauce_output)
    if (outExists) {
      out$soe <- soe$soe
    } else {
      out <- soe
      outExists <- TRUE
    }
  }

  return(out)

}
