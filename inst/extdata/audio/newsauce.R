emusauce <- function(inputDir, pitch = TRUE, formant = TRUE, bw = TRUE,
                     intervalFixed = 0.005, f0min = 50, f0max = 300,
                     windowLength = 0.025, maxNumFormants = 5,
                     bw_hawksMiller = TRUE,
                     praatsauce_output = NULL) {

  if (maxNumFormants < 4) stop('maxNumFormants should be higher than 4')

  fls <- list.files(inputDir, pattern='*.wav')
  if (!is.null(praatsauce_output)) {
    beginTime <- praatsauce_output$t[1]
    out <- praatsauce_output
    outExists <- TRUE
  } else {
    beginTime <- 0
    outExists <- FALSE
  }

  if (pitch) {
    f0 <- get_pitch(fls, inputDir, intervalFixed, f0min, f0max,
                    beginTime, praatsauce_output)
    if (outExists) {
      out$f0 <- f0$f0
    } else {
      out <- f0
      outExists <- TRUE
    }
  }
  if (formant) {
    fmt <- get_formants(fls, inputDir, intervalFixed,  windowLength,
                        !bw_hawksMiller, maxNumFormants,
                        beginTime, praatsauce_output)
    if (outExists) {
      out$F1 <- fmt$F1; out$F2 <- fmt$F2; out$F3 <- fmt$F3
      if (!bw_hawksMiller) {
        out$B1 <- fmt$B1; out$B2 <- fmt$B2; out$B3 <- fmt$B3
      }
    } else {
      out <- fmt
      outExists <- TRUE
    }
  }

  return(out)

}

emusauce('inst/extdata/audio') -> t
