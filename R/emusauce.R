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
#' @param recursive Logical; should sound files in subdirectories of
#' `inputDir` be analyzed? Default is `FALSE`.
#' @param existing_output Optional data frame containing existing measures
#' from e.g. PraatSauce. Can come from anywhere, including VoiceSauce, but
#' should adhere to the column naming conventions from PraatSauce output.
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
                     bw_hawksMiller = TRUE, recursive = FALSE,
                     existing_output = NULL) {

  if (inherits(inputDir, 'emuDBhandle')) {
    inputDir <- inputDir$basePath
    recursive <- TRUE
  }

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

  if (recursive) {
    fls <- list.files(inputDir, pattern='*.wav', recursive=TRUE)
  } else {
    fls <- list.files(inputDir, pattern='*.wav')
  }
  if (!is.null(existing_output)) {
    beginTime <- existing_output$t[1]
    out <- existing_output
    outExists <- TRUE
  } else {
    beginTime <- 0
    outExists <- FALSE
  }

  if (pitchReq) {
    if (!'f0' %in% colnames(existing_output)) {
      f0 <- get_pitch(fls, inputDir, intervalFixed, f0min, f0max,
                      beginTime, existing_output)
    } else {
      f0 <- data.frame(file = existing_output$file,
                       t = existing_output$t,
                       f0 = existing_output$f0)
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
    if (!'F1' %in% colnames(existing_output)) {
      fmt <- get_formants(fls, inputDir, intervalFixed,  windowLength,
                          !bw_hawksMiller, maxNumFormants,
                          beginTime, existing_output)
    } else {
      fmt <- data.frame(file = existing_output$file,
                        t = existing_output$t,
                        F1 = existing_output$F1,
                        F2 = existing_output$F2,
                        F3 = existing_output$F3)
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
    if (!'H1H2c' %in% colnames(existing_output)) {
      spec <- get_spectralMeasures(fls, inputDir, f0, fmt, bwdf,
                                   intervalFixed, windowLength, slope,
                                   slopeUncorrected, beginTime, existing_output)
    } else {
      spec <- data.frame(file = existing_output$file,
                        t = existing_output$t,
                        H1H2c = existing_output$H1H2c,
                        H2H4c = existing_output$H2H4c,
                        H1A1c = existing_output$H1A1c,
                        H1A2c = existing_output$H1A2c,
                        H1A3c = existing_output$H1A3c,
                        H2KH5Ku = existing_output$H2KH5Ku)
      if ('H1c' %in% colnames(existing_output)) {
        spec$H1c <- existing_output$H1c; spec$H2c <- existing_output$H2c
        spec$H4c <- existing_output$H4c; spec$H4c <- existing_output$H4c
        spec$A1c <- existing_output$A1c; spec$A2c <- existing_output$A2c
        spec$A3c <- existing_output$A3c; spec$H2Ku <- existing_output$H2Ku
        spec$H5Ku <- existing_output$H5Ku
      }
      if ('H1u' %in% colnames(existing_output)) {
        spec$H1u <- existing_output$H1u; spec$H2u <- existing_output$H2u
        spec$H4u <- existing_output$H4u; spec$H4u <- existing_output$H4u
        spec$A1u <- existing_output$A1u; spec$A2u <- existing_output$A2u
        spec$A3u <- existing_output$A3u
      }
      if ('H1H2u' %in% colnames(existing_output)) {
        spec$H1H2u <- existing_output$H1H2u
        spec$H2H4u <- existing_output$H2H4u
        spec$H1A1u <- existing_output$H1A1u
        spec$H1A2u <- existing_output$H1A2u
        spec$H1A3u <- existing_output$H1A3u
      }
    }

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
    if (!'CPP' %in% colnames(existing_output)) {
      cpp <- get_CPP(fls, inputDir, windowLength, f0min, f0max, intervalFixed,
                     beginTime, existing_output)
    } else {
      cpp <- data.frame(file = existing_output$file,
                        t = existing_output$t,
                        CPP = existing_output$CPP)
    }
    if (outExists) {
      out$CPP <- cpp$CPP
    } else {
      out <- cpp
      outExists <- TRUE
    }
  }

  if (hnr) {
    if (!'HNR05' %in% colnames(existing_output)) {
      hnr05 <- get_HNR(fls, inputDir, 500, f0min, f0max, intervalFixed,
                       beginTime, out, existing_output)
      hnr15 <- get_HNR(fls, inputDir, 1500, f0min, f0max, intervalFixed,
                       beginTime, out, existing_output)
      hnr25 <- get_HNR(fls, inputDir, 2500, f0min, f0max, intervalFixed,
                       beginTime, out, existing_output)
      hnr35 <- get_HNR(fls, inputDir, 3500, f0min, f0max, intervalFixed,
                       beginTime, out, existing_output)
    } else {
      hnr05 <- data.frame(file = existing_output$file,
                        t = existing_output$t,
                        HNR500 = existing_output$HNR05)
      hnr15 <- data.frame(file = existing_output$file,
                          t = existing_output$t,
                          HNR1500 = existing_output$HNR15)
      hnr25 <- data.frame(file = existing_output$file,
                          t = existing_output$t,
                          HNR2500 = existing_output$HNR25)
      hnr35 <- data.frame(file = existing_output$file,
                          t = existing_output$t,
                          HNR3500 = existing_output$HNR35)
    }
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
    if (!'intensity' %in% colnames(existing_output)) {
      rms <- get_intensity(fls, inputDir, intervalFixed, f0min,
                           beginTime, existing_output)
    } else {
      rms <- data.frame(file = existing_output$file,
                        t = existing_output$t,
                        intensity = existing_output$intensity)
    }
    if (outExists) {
      out$intensity <- rms$intensity
    } else {
      out <- rms
      outExists <- TRUE
    }
  }

  if (soe) {
    if (!'soe' %in% colnames(existing_output)) {
      soe <- get_SOE(fls, inputDir, intervalFixed, beginTime, f0min, f0max,
                     f0, existing_output)
    } else {
      soe <- data.frame(file = existing_output$file,
                        t = existing_output$t,
                        soe = existing_output$soe)
    }
    if (outExists) {
      out$soe <- soe$soe
    } else {
      out <- soe
      outExists <- TRUE
    }
  }

  return(out)

}
