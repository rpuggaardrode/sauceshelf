#' Load voice quality measurements made in PraatSauce
#'
#' Load tab-separated file of voice quality measures made
#' by running the PraatSauce scripts and properly format data frame
#'
#' @param fileLoc String giving the location of PraatSauce output file
#' @param useTextGrid Logical; what was the value of the `useTextGrid`
#' parameter in the parameters file when calling PraatSauce? Default is `FALSE`.
#' @param emuDB Logical; is the audio data structured as an EMU database?
#' Default is `FALSE`.
#'
#' @return Formatted data frame.
#' @export
#'
#' @examples
#' datapath <- system.file('extdata/audio/out.tsv', package='sauceshelf')
#' sauce <- sauceshelf::load_sauce(datapath)
load_sauce <- function(fileLoc, useTextGrid = FALSE, emuDB = FALSE) {
  out <- read.table(fileLoc, sep = '\t', header=T)
  out[] <- lapply(out, gsub, pattern = '--undefined--', replacement = '0')
  if (useTextGrid) {
    out[,3:ncol(out)] <- lapply(out[,3:ncol(out)], as.numeric)
  } else {
    out[,2:ncol(out)] <- lapply(out[,2:ncol(out)], as.numeric)
  }
  out$file <- gsub('^ *', '', out$file, perl=T)

  if (emuDB) {
    session <- gsub('/.*', '', out$file, perl=T)
    bundle <- gsub('.*/', '', out$file, perl=T)
    bundle <- gsub('.wav', '', bundle)
    out <- out[,-1]
    out <- cbind(session, bundle, out)
  }

  return(out)
}
