#' Estimate cepstral peak prominence
#'
#' Estimate cepstral peak prominence from a list of sound files by comparing
#' actual cepstral peak to the corresponding location in
#' the regression-predicted linear slope of the cepstrum.
#'
#' @param filelist Vector of strings giving the names of sound files to analyze.
#' @param inputDir String giving the directory where sound files are located.
#' @param windowLength Numeric giving the length of the analysis window (in
#' seconds). Default is `0.025`.
#' @param f0min Integer giving the pitch floor. Default is `50`.
#' @param f0max Integer giving the pitch ceiling. Default is `300`.
#' @param intervalFixed Numeric; how often should measures be taken (in
#' seconds)? Default is `0.005`.
#' @param beginTime Numeric; where should the first measure be taken (in
#' seconds)? Default is `0`.
#' @param praatsauce_output Optional data frame containing existing measures
#' from PraatSauce. Used to ensure an equal number of rows.
#'
#' @return A data frame with CPP values.
#' @export
#'
#' @examples
#' datapath <- system.file('extdata/audio', package='sauceshelf')
#' fls <- list.files(datapath, pattern='.wav')
#' cpp <- get_CPP(fls, datapath)
get_CPP <- function(filelist, inputDir, windowLength = 0.025, f0min = 50,
                    f0max = 300, intervalFixed = 0.005, beginTime = 0,
                    praatsauce_output = NULL) {

  out <- data.frame(file = NA, t = NA, CPP = NA)
  specbw <- (1 / windowLength) / 2

  for (f in filelist) {
    fn <- paste0(inputDir, '/', f)
    cepObj <- wrassp::cepstrum(fn, toFile=F, verbose=F,
                               windowShift = intervalFixed * 1000,
                               resolution = 1 / windowLength,
                               beginTime = beginTime)
    cep <- cepObj$cep
    cep <- 20*log10(abs(cep))
    quefRange <- (0:(dim(cep)[2] - 1)) * (1 / specbw / dim(cep)[2])
    quef2freq <- 2/quefRange
    nFrames <- dim(cep)[1]
    t <- seq(attr(cepObj, 'startTime'),
             attr(cepObj, 'endRecord') * intervalFixed,
             by = intervalFixed)
    cppAna <- data.frame(id = 1:dim(cep)[2],
                         qr = quefRange,
                         fr = quef2freq)
    cpptmp <- rep(NA, nFrames)

    for (i in 1:nFrames) {
      cppAna_tmp <- cppAna
      cppAna_tmp$cep <- cep[i,]
      cppAna_tmp <- cppAna_tmp[-which(cppAna_tmp$qr < 0.001),]
      cp <- max(cppAna_tmp[which(cppAna_tmp$fr > f0min & cppAna_tmp$fr < f0max),'cep'])
      cpId <- which(cppAna_tmp$cep == cp)
      cppAna_tmp$fit <- stats::lm(cep ~ qr, data=cppAna_tmp)$fitted.values
      cpptmp[i] <- abs(cppAna_tmp$cep[cpId] - cppAna_tmp$fit[cpId])
    }

    tmp <- data.frame(file = rep(f, length(nFrames)),
                      t = t, CPP = cpptmp)
    if (!is.null(praatsauce_output)) {
      psFile <- praatsauce_output[praatsauce_output$file == f,]
      tmp <- tmp[1:nrow(psFile),]
    }
    out <- rbind(out, tmp)
  }

  out <- out[-1,]
  return(out)

}
