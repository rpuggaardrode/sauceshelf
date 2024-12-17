#' Estimate harmonics-to-noise ratio
#'
#' Estimate harmonics-to-noise ratio of a list of sound files using Boersma's
#' autocorrelation method.
#'
#' @param filelist Vector of strings giving the names of sound files to analyze.
#' @param inputDir String giving the directory where sound files are located.
#' @param uprFreqPass Numeric giving the highest upper frequency of the
#' bandpass filter applied to the sound file prior to analysis.
#' @param f0min Integer giving the pitch floor. Default is `50`.
#' @param f0max Integer giving the pitch ceiling. Default is `300`.
#' @param intervalFixed Numeric; how often should measures be taken (in
#' seconds)? Default is `0.005`.
#' @param beginTime Numeric; where should the first measure be taken (in
#' seconds)? Default is `0`.
#' @param output Optional data frame containing existing measures. Used to
#' ensure an equal number of rows if compiling a larger data frame with other
#' measures.
#' @param praatsauce_output Optional data frame containing existing measures
#' from PraatSauce. Used to ensure an equal number of rows.
#'
#' @return A data frame with HNR values.
#' @export
#'
#' @examples
#' datapath <- system.file('extdata/audio', package='sauceshelf')
#' fls <- list.files(datapath, pattern='.wav')
#' hnr <- get_HNR(fls, datapath, 500)
get_HNR <- function(filelist, inputDir, uprFreqPass, f0min = 50, f0max = 300,
                    intervalFixed = 0.005, beginTime = 0, output = NULL,
                    praatsauce_output = NULL) {

  out <- data.frame(file = NA, t = NA, HNR = NA)

  for (f in filelist) {
    fn <- paste0(inputDir, '/', f)
    snd <- tuneR::readWave(fn, from = beginTime, units = 'seconds')
    filtered <- soundgen::bandpass(snd@left, snd@samp.rate, 0, uprFreqPass)
    filtered <- as.integer(filtered)
    filtered <- tuneR::Wave(filtered, samp.rate = snd@samp.rate,
                            bit = snd@bit)
    new_sr <- 16000
    lower_sr_snd <- tuneR::downsample(filtered, new_sr)
    temploc <- file.path(tempdir(), 'dummy.wav')
    tuneR::writeWave(lower_sr_snd, temploc)

    nSamp <- round(((new_sr) / f0min) * 4.5)
    ws <- ((1 / f0min) * 4.5) * 1000
    acfObj <- wrassp::acfana(temploc, toFile=F, verbose=F,
                             windowShift = intervalFixed * 1000,
                             analysisOrder=nSamp,
                             windowSize=ws, window='HANN')
    t <- seq(attr(acfObj, 'startTime'),
             attr(acfObj, 'endRecord') * intervalFixed,
             by = intervalFixed)

    nFrames <- dim(acfObj$acf)[1]

    x <- 1:nSamp
    nAutocorHann <- (1 - (x / nSamp)) * ((2/3) + (1/3)*cos((2*pi*x)/nSamp)) +
      (1/(2*pi))*sin((2*pi*x)/nSamp)

    maxPeakSamp <- (1 / f0max) * new_sr
    minPeakSamp <- (1 / f0min) * new_sr

    hnr <- c()

    for (i in 1:nFrames) {
      acfAnaCentered <- acfObj$acf[i,] - mean(acfObj$acf[i,])
      acfAnaScaled <- scales::rescale(acfAnaCentered, c(-1, 1),
                                      from=c(-max(acfAnaCentered),
                                             max(acfAnaCentered)))
      reWindowed <- acfAnaScaled[-length(acfAnaScaled)] / nAutocorHann
      peak <- round(which.max(reWindowed[maxPeakSamp:minPeakSamp]) + maxPeakSamp - 1)
      peakEnergy <- reWindowed[peak]
      if (peakEnergy > 1) peakEnergy <- 1 + (1 - peakEnergy)
      if (peakEnergy < 0) peakEnergy <- -peakEnergy
      hnr[i] <- 10*log10(peakEnergy/(1-peakEnergy))
    }

    tmp <- data.frame(file = rep(f, length(nFrames)),
                      t = t, HNR = hnr)
    if (!is.null(praatsauce_output)) {
      psFile <- praatsauce_output[praatsauce_output$file == f,]
      tmp <- tmp[1:nrow(psFile),]
    }

    if (!is.null(output)) {
      if (nrow(tmp) != sum(output$file==f)) tmp <- tmp[-nrow(tmp),]
    }

    out <- rbind(out, tmp)
  }

  colnames(out)[ncol(out)] <- paste0('HNR', uprFreqPass)
  out <- out[-1,]
  return(out)

}
