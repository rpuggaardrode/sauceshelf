#' Estimate pitch
#'
#' Estimate pitch of sound files using the Schäffler-Vincent algorithm.
#' Wrapper for [wrassp::ksvF0] returning a data frame.
#'
#' @param filelist Vector of strings giving the names of sound files to analyze.
#' @param inputDir String giving the directory where sound files are located.
#' @param intervalFixed Numeric; how often should measures be taken (in
#' seconds)? Default is `0.005`.
#' @param f0min Integer giving the pitch floor. Default is `50`.
#' @param f0max Integer giving the pitch ceiling. Default is `300`.
#' @param beginTime Numeric; where should the first measure be taken (in
#' seconds)? Default is `0`.
#' @param praatsauce_output Optional data frame containing existing measures
#' from PraatSauce. Used to ensure an equal number of rows.
#'
#' @return A data frame with pitch values.
#' @export
#'
#' @examples
#' #datapath <- system.file('extdata/audio', package='sauceshelf')
#' #fls <- list.files(datapath, pattern='.wav')
#' #f0 <- get_pitch(fls, datapath)
get_pitch <- function(filelist, inputDir,
                      intervalFixed = 0.005, f0min = 50, f0max = 300,
                      beginTime = 0, praatsauce_output = NULL) {

  out <- data.frame(file = NA, t = NA, f0 = NA)

  for (f in filelist) {
    fn <- paste0(inputDir, '/', f)
    res <- wrassp::ksvF0(fn, toFile=F, windowShift = intervalFixed * 1000,
                         minF=f0min, maxF=f0max, beginTime=beginTime)
    f0 <- res$F0[,1]
    t <- seq(attr(res, 'startTime'),
             attr(res, 'endRecord') * intervalFixed,
             by = intervalFixed)
    tmp <- data.frame(file = rep(f, length(f0)),
                      t = t, f0 = f0)
    tmp$f0[tmp$f0 == 0] <- NA
    if (!is.null(praatsauce_output)) {
      psFile <- praatsauce_output[praatsauce_output$file == f,]
      tmp <- tmp[1:nrow(psFile),]
    }
    out <- rbind(out, tmp)
  }

  out <- out[-1,]
  return(out)

}
