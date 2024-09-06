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
    if (!is.null(praatsauce_output)) {
      psFile <- praatsauce_output[praatsauce_output$file == f,]
      tmp <- tmp[1:nrow(psFile),]
    }
    out <- rbind(out, tmp)
  }

  out <- out[-1,]
  return(out)

}
