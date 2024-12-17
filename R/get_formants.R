#' Estimate formants and formant bandwidths
#'
#' Estimate formants and optionally bandwidths for a list of sound files.
#' Wrapper for [wrassp::forest] returning a data frame.
#'
#' @param filelist Vector of strings giving the names of sound files to analyze.
#' @param inputDir String giving the directory where sound files are located.
#' @param intervalFixed Numeric; how often should measures be taken (in
#' seconds)? Default is `0.005`.
#' @param windowLength Numeric giving the length of the analysis window (in
#' seconds). Default is `0.025`.
#' @param measureBandwidths Boolean; should bandwidths be returned? Default is
#' `FALSE`.
#' @param maxNumFormants Integer giving the maximum number of formants to
#' estimate. Default is `5`.
#' @param beginTime Numeric; where should the first measure be taken (in
#' seconds)? Default is `0`.
#' @param praatsauce_output Optional data frame containing existing measures
#' from PraatSauce. Used to ensure an equal number of rows.
#'
#' @return A data frame with formants and optionally bandwidths values.
#' @export
#'
#' @examples
#' datapath <- system.file('extdata/audio', package='sauceshelf')
#' fls <- list.files(datapath, pattern='.wav')
#' fmt <- get_formants(fls, datapath)
get_formants <- function(filelist, inputDir = '.',
                         intervalFixed = 0.005, windowLength = 0.025,
                         measureBandwidths = FALSE, maxNumFormants = 5,
                         beginTime = 0, praatsauce_output = NULL) {

  out <- data.frame(file = NA, t = NA, F1 = NA, F2 = NA, F3 = NA)
  if (measureBandwidths) {
    out$B1 <- NA; out$B2 <- NA; out$B3 <- NA
  }

  for (f in filelist) {
    fn <- paste0(inputDir, '/', f)
    res <- wrassp::forest(fn, toFile=F, windowShift = intervalFixed * 1000,
                          windowSize = windowLength * 1000, estimate = TRUE,
                          numFormants = maxNumFormants, beginTime = beginTime)
    F1 <- res$fm[,1]; F2 <- res$fm[,2]; F3 <- res$fm[,3]
    if (measureBandwidths) {
      B1 <- res$bw[,1]; B2 <- res$bw[,2]; B3 <- res$bw[,3]
    }
    t <- seq(attr(res, 'startTime'),
             attr(res, 'endRecord') * intervalFixed,
             by = intervalFixed)
    tmp <- data.frame(file = rep(f, length(t)),
                      t = t, F1 = F1, F2 = F2, F3 = F3)
    if (measureBandwidths) {
      tmp$B1 <- B1; tmp$B2 <- B2; tmp$B3 <- B3
    }
    if (!is.null(praatsauce_output)) {
      psFile <- praatsauce_output[praatsauce_output$file == f,]
      tmp <- tmp[1:nrow(psFile),]
    }
    out <- rbind(out, tmp)
  }

  out <- out[-1,]
  return(out)

}
