#' Estimate root-mean-squared intensity
#'
#' Estimate root-mean-squared intensity of a list of sounds files.
#' Wrapper for [wrassp::rmsana] returning a data frame.
#'
#' @param filelist Vector of strings giving the names of sound files to analyze.
#' @param inputDir String giving the directory where sound files are located.
#' @param intervalFixed Numeric; how often should measures be taken (in
#' seconds)? Default is `0.005`.
#' @param f0min Integer giving the pitch floor. Default is `50`.
#' @param beginTime Numeric; where should the first measure be taken (in
#' seconds)? Default is `0`.
#' @param praatsauce_output Optional data frame containing existing measures
#' from PraatSauce. Used to ensure an equal number of rows.
#'
#' @return A data frame with intensity values.
#' @export
#'
#' @examples
#' datapath <- system.file('extdata/audio', package='sauceshelf')
#' fls <- list.files(datapath, pattern='.wav')
#' rms <- get_intensity(fls, datapath)
get_intensity <- function(filelist, inputDir, intervalFixed = 0.005,
                          f0min = 50, beginTime = 0, praatsauce_output = NULL) {

  out <- data.frame(file = NA, t = NA, intensity = NA)

  for (f in filelist) {
    fn <- paste0(inputDir, '/', f)
    res <- wrassp::rmsana(fn, toFile=F, windowShift = intervalFixed * 1000,
                          windowSize = (1 / f0min) * 1000, beginTime=beginTime)
    rms <- res$rms[,1]
    t <- seq(attr(res, 'startTime'),
             attr(res, 'endRecord') * intervalFixed,
             by = intervalFixed)
    tmp <- data.frame(file = rep(f, length(rms)),
                      t = t, intensity = rms)
    if (!is.null(praatsauce_output)) {
      psFile <- praatsauce_output[praatsauce_output$file == f,]
      tmp <- tmp[1:nrow(psFile),]
    }
    out <- rbind(out, tmp)
  }

  out <- out[-1,]
  return(out)

}
