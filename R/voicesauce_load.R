#' Load and format output from VoiceSauce
#'
#' Loads an output file with measures calculated in the VoiceSauce program
#' into R.
#'
#' @param fileLoc String pointing to the output file from VoiceSauce
#' @param normalize_output Boolean; should the column names and contents from
#' VoiceSauce be normalized to look like the output of [emusauce] and
#' [praatsauce]? Default is `TRUE`.
#' @param na_output How should infelicitous values be coded? Default is `NA`.
#'
#' @return A data frame with rows and columns corresponding to the VoiceSauce
#' output.
#' @export
#'
#' @examples
#' datapath <- system.file('extdata/text', package='sauceshelf')
#' sauce <- voicesauce_load(paste0(datapath, '/output.txt'))
voicesauce_load <- function(fileLoc, normalize_output = TRUE, na_output = NA) {
  out <- utils::read.delim(fileLoc, header=T)
  out[out==0] <- na_output

  if (normalize_output) {
    out$Filename <- gsub('.mat', '.wav', out$Filename)
    cn <- colnames(out)
    cn[cn == 'Filename'] <- 'file'
    cn[cn == 't_ms'] <- 't'
    cn[cn == 'Label'] <- 'label'
    cn[cn == 'Energy'] <- 'intensity'
    colnames(out) <- cn
    out$t <- out$t / 1000
  }

  return(out)
}
