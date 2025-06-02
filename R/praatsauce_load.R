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
#' @param na_output How should infelicitous values be coded? Default is `NA`.
#'
#' @return Formatted data frame based on output from PraatSauce.
#' @export
#'
#' @examples
#' datapath <- system.file('extdata/text/out.tsv', package='sauceshelf')
#' sauce <- praatsauce_load(datapath)
praatsauce_load <- function(fileLoc, useTextGrid = FALSE, emuDB = FALSE,
                            na_output = NA) {
  out <- utils::read.table(fileLoc, sep = '\t', header=T)
  out[] <- lapply(out, gsub, pattern = '--undefined--', replacement = na_output)
  out[out==0] <- na_output
  if (!is.numeric(out[,2])) useTextGrid <- TRUE
  if (useTextGrid) {
    out[,3:ncol(out)] <- lapply(out[,3:ncol(out)], as.numeric)
  } else {
    out[,2:ncol(out)] <- lapply(out[,2:ncol(out)], as.numeric)
  }
  out$file <- gsub('^ *', '', out$file, perl=T)
  if ('HNR05' %in% colnames(out)) {
    out$HNR05[out$HNR05 == -200] <- na_output
    out$HNR15[out$HNR15 == -200] <- na_output
    out$HNR25[out$HNR25 == -200] <- na_output
    out$HNR35[out$HNR35 == -200] <- na_output
  }

  return(out)
}
