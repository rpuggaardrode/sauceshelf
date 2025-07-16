#' Voice quality measures with by-speaker two-pass pitch estimation
#'
#' Estimates a range of voice quality measures using a two-pass procedure for
#' estimating suitable by-speaker pitch floor and ceiling values,
#' following e.g. De Looze & Hirst (2021). Pitch is estimated for each speaker
#' using liberal floor and ceiling values, and suitable estimates are then
#' computed from the distribution of these values.
#'
#' @param inputDir String giving the location of a directory with WAV files
#' to process. Note that this should be the entire file path; regular
#' R style path extension will not work, as the directory should  be
#' relative to the location of the Praat script. This should be a nested
#' directory, where each directory contains the sound files of a speaker.
#' Alternatively, this can be the handle of a loaded EMU database.
#' @param sauceFunction Which function should be called to estimate voice
#' quality measures? Should be either `praatsauce` (default) or `mixedsauce`,
#' but currently only works properly for `praatsauce`.
#' @param recursive Logical; should sound files in subsubdirectories of
#' `inputDir` be analyzed? Default is `FALSE`.
#' @param firstPass_f0min Integer giving the pitch floor to be used for the
#' first pass in Hz. Default is `50`.
#' @param firstPass_f0max Integer giving the pitch ceiling to be used for the
#' first pass in Hz. Default is `700`.
#' @param min_multiplier Numeric; the pitch floor in the second pass corresponds
#' to the first quartile from the first pass multiplied by this value.
#' Default is `0.75`.
#' @param max_multiplier Numeric; the pitch ceiling in the second pass
#' corresponds to the third quartile from the first pass multiplied by this
#' value.  Default is `1.5`.
#' @param verbose Logical; should a message be printed to the console whenever
#' a speaker has been processed? Default is `TRUE`.
#' @param ... Further arguments passed on to the function specified by
#' `sauceFunction`.
#'
#' @return Data frame containing results of chosen measures.
#' @export
#'
#' @examples
#' # NOT NOW
sauce_hirst2pass <- function(inputDir, sauceFunction = praatsauce,
                             recursive = FALSE, firstPass_f0min = 50,
                             firstPass_f0max = 700, min_multiplier = 0.75,
                             max_multiplier = 1.5, verbose = TRUE, ...) {

  if (class(inputDir) == 'emuDBhandle') {
    baseDir <- inputDir$basePath
    procDirs <- file.path(baseDir,
                          paste0(emuR::list_sessions(inputDir)$name, '_ses'))
    recursive <- TRUE
  } else {
    procDirs <- list.dirs(inputDir, recursive = FALSE)
  }

  dotArgs <- rlang::dots_list(...)

  for (speaker in procDirs) {
    fl <- list.files(speaker, pattern = '*.wav', recursive = recursive)

    speakerArgs <- dotArgs
    speakerArgs$inputDir <- speaker
    speakerArgs$filelist <- fl
    speakerArgs$recursive <- FALSE
    falseArgs <- c('formant', 'harmonicAmplitude',
                       'harmonicAmplitudeUncorrected', 'bw', 'bwHawksMiller',
                       'slope', 'slopeUncorrected', 'cpp', 'hnr', 'intensity',
                       'soe', 'pitchSave')
    pitchOnlyArgs <- speakerArgs
    pitchOnlyArgs[falseArgs] <- FALSE
    pitchOnlyArgs$f0min <- firstPass_f0min
    pitchOnlyArgs$f0max <- firstPass_f0max

    firstPass <- do.call(sauceFunction, pitchOnlyArgs)

    q <- stats::quantile(firstPass$f0, probs = c(0.25, 0.75), na.rm = T,
                         names = F)
    speakerArgs$f0min <- min_multiplier * q[1]
    speakerArgs$f0max <- max_multiplier * q[2]

    secondPass <- do.call(sauceFunction, speakerArgs)
    secondPass$file <- paste0(gsub('.*_emuDB/', '', speaker),
                              '/', secondPass$file)

    if (exists('out')) {
      out <- rbind(out, secondPass)
    } else {
      out <- secondPass
    }

    if (verbose) print(paste(speaker, 'done!'))
  }

  return(out)

}
