#' Estimate strength of harmonic excitation
#'
#' Estimates the strength of harmonic excitation in sound files, by passing
#' a differenced version of the signal through a cascade of two zero frequency
#' filters, removing the moving average at each step.
#'
#' @param filelist Vector of strings giving the names of sound files to analyze.
#' @param inputDir String giving the directory where sound files are located.
#' @param intervalFixed Numeric; how often should measures be taken (in
#' seconds)? Default is `0.005`.
#' @param beginTime Numeric; where should the first measure be taken (in
#' seconds)? Default is `0`.
#' @param f0min Integer giving the pitch floor. Default is `50`.
#' @param f0max Integer giving the pitch ceiling. Default is `300`.
#' @param f0 Data frame consisting of at least a column `file` with sound file
#' names, a column `t` with time in seconds, and a column `f0` with pitch
#' measures. Used to estimate the length of the moving average filter; default
#' is `NULL`, if no data frame is provided, pitch is calculated using
#' [wrassp::ksvF0].
#' @param praatsauce_output Optional data frame containing existing measures
#' from PraatSauce. Used to ensure an equal number of rows.
#'
#' @return A data frame with SOE values.
#' @export
#'
#' @examples
#' datapath <- system.file('extdata/audio', package='sauceshelf')
#' fls <- list.files(datapath, pattern='.wav')
#' soe <- get_SOE(fls, datapath)
get_SOE <- function(filelist, inputDir, intervalFixed = 0.005, beginTime = 0,
                    f0min = 50, f0max = 300, f0 = NULL,
                    praatsauce_output = NULL) {

  out <- data.frame(file = NA, t = NA, soe = NA)

  for (f in filelist) {
    fn <- paste0(inputDir, '/', f)
    snd <- tuneR::readWave(fn, from = beginTime, units = 'seconds')
    sr <- 16000
    snd <- tuneR::downsample(snd, sr)
    sig <- diff(snd@left, 1)

    if (is.null(f0)) {
      f0vals <- wrassp::ksvF0(fn, toFile=F)$F0[,1]
    } else {
      f0vals <- f0[f0$file==f,]$f0
    }

    f0vals[which(f0vals==0)] <- NA
    mean_f0 <- mean(sr/f0vals, na.rm=T)
    n0 <- round(mean_f0/1.5)
    wid <- 2*n0+1
    len <- length(sig)

    zfr1_filt <- signal::filter(1, c(1, -2*0.999, 0.999^2), sig)
    a <- signal::filter(rep(1, wid)/wid, 1, zfr1_filt)
    abegin <- cumsum(sig[1:(wid-2)])
    abegin <- abegin[seq(1, length(abegin), by=2)] / seq(1, wid-2, by=2)
    aend <- cumsum(sig[len:(len-wid+3)])
    aend <- aend[seq(length(aend), 1, by=-2)] / seq(wid-2, 1, by=-2)
    a <- c(abegin, a[wid:length(a)], aend)
    zfr1_trendRem <- zfr1_filt - a

    zfr2_filt <- signal::filter(1, c(1, -2*0.999, 0.999^2), zfr1_trendRem)
    a <- signal::filter(rep(1, wid)/wid, 1, zfr2_filt)
    abegin <- cumsum(zfr2_filt[1:(wid-2)])
    abegin <- abegin[seq(1, length(abegin), by=2)] / seq(1, wid-2, by=2)
    aend <- cumsum(zfr2_filt[len:(len-wid+3)])
    aend <- aend[seq(length(aend), 1, by=-2)] / seq(wid-2, 1, by=-2)
    a <- c(abegin, a[wid:length(a)], aend)
    zfr_out <- zfr2_filt - a

    z <- 0.95*zfr_out[1:(length(zfr_out)-wid)]/
      max(abs(zfr_out[1:(length(zfr_out)-wid)]))

    z1 <- c(NA, z[1:length(z)-1])
    tf <- z1 > 0 & z<=0

    pulses <- which(tf)
    pitch <- data.frame(
      pulses = pulses / sr,
      diff = c(NA, pulses[-1] / sr - pulses[-length(pulses)] / sr))
    pitch$f0 <- 1 / pitch$diff
    pitch$t <- pitch$pulses - (pitch$diff / 2)

    dx <- c()
    for (i in 1:length(pulses)) dx[i] <-
      stats::lm(z1[(pulses[i]-1):(pulses[i]+1)]~c(1:3))$coefficients[2]

    pitch$soe <- -dx
    pitch <- pitch[-1,]
    pitch$soe[which(pitch$f0 < f0min & pitch$f0 > f0max)] <- NA

    tmp <- data.frame(file = rep(f, length(seq(0, (len / sr) ,
                                               by = intervalFixed))),
                      t = seq(0, len / sr, by = intervalFixed),
                      soe = signal::interp1(pitch$t, pitch$soe,
                                            seq(0, (len / sr) ,
                                                by = intervalFixed)))

    if (!is.null(f0)) {
      if (nrow(tmp) > sum(f0$file==f)) tmp <-
          tmp[-seq(nrow(tmp), sum(f0$file==f) + 1),]
    }

    out <- rbind(out, tmp)
    if (!is.null(praatsauce_output)) {
      psFile <- praatsauce_output[praatsauce_output$file == f,]
      tmp <- tmp[1:nrow(psFile),]
    }

  }

  out <- out[-1,]
  return(out)

}
