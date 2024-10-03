#' Run PraatSauce from R
#'
#' Shell wrapper for running the PraatSauce suite of Praat scripts.
#' Returns the results as a data frame.
#'
#' @param inputDir String giving the location of a directory with WAV files
#' to process. Note that this should be the entire file path; regular
#' R style path extension will not work, as the directory should  be
#' relative to the location of the Praat script.
#' Alternatively, this can be the handle of a loaded EMU database.
#' @param channel Integer; if the audio has multiple channels, which one should
#' be extracted and used for analysis? Default is `1`.
#' @param intervalEquidistant Integer; if measures should be taken at
#' equidistant intervals, how many measures should be taken? Only relevant if
#' TextGrids are used to determine where to take measures in a sound file.
#' Default is `FALSE`, in which case measures are taken at fixed intervals.
#' @param intervalFixed Numeric; how often should measures be taken (in
#' seconds)? Default is `0.005`.
#' @param pitch Logical; should pitch be reported? Default is `TRUE`.
#' @param formant Logical; should formants be reported? Default is `TRUE`.
#' @param harmonicAmplitude Logical; should harmonic amplitudes (H1\*, H2\*,
#' H4\*, A1\*, A2\*, A3\*, H2K, and H5K) be reported? Default is `TRUE`.
#' @param harmonicAmplitudeUncorrected Logical; should uncorrected harmonic
#' amplitudes (H1, H2, H4, A1, A2, A3) be reported? Default is `TRUE`.
#' @param bw Logical; should formant bandwidths be reported? Default is `TRUE`.
#' @param bwHawksMiller Logical; should formant bandwidths be estimated using the
#' Hawks-Miller formula? Default is `TRUE`; if `FALSE` and `bw` is `TRUE`,
#' empirical bandwidths are reported and used for corrections.
#' @param slope Logical; should spectral slope (H1\*-H2\*, H2\*-H4\*,
#' H1\*-A1\*, H1\*-A2\*, H1\*-A3\*, H2K-H5K) be reported? Default is `TRUE`.
#' @param slopeUncorrected Logical; should uncorrected spectral slope (H1-H2,
#' H2-H4, H1-A1, H1-A2, H1-A3) be reported? Default is `TRUE`.
#' @param cpp Logical; should cepstral peak prominence be reported? Default is
#' `TRUE`.
#' @param hnr Logical; should other harmonics-to-noise ratios be reported?
#' Default is `TRUE`.
#' @param intensity Logical; should intensity be reported? Default is `TRUE`.
#' @param resample16kHz Logical; should files be resampled to 16 kHz before
#' analysis? Default is `FALSE`.
#' @param windowLength Numeric giving the length of the analysis window (in
#' seconds). Default is `0.025`.
#' @param f0min Integer giving the pitch floor. Default is `50`.
#' @param f0max Integer giving the pitch ceiling. Default is `300`.
#' @param maxNumFormants Integer giving the maximum number of formants to
#' estimate. Default is `5`.
#' @param preEmphFrom Numeric giving the frequency floor for pre-emphasis.
#' Default is `50`.
#' @param f1ref Numeric giving the reference frequency for the first formant.
#' Default is `500`.
#' @param f2ref Numeric giving the reference frequency for the second formant.
#' Default is `1500`.
#' @param f3ref Numeric giving the reference frequency for the third formant.
#' Default is `2500`.
#' @param maxFormantHz Numeric giving the maximum frequency to search for
#' formants. Default is `5000`.
#' @param useTextGrid Logical; should TextGrids be used to determine which
#' parts of sound files to measure? Default is `FALSE`.
#' @param tgDir String giving the location of a directory of TextGrid files.
#' Note that this should be the entire file path; regular
#' R style path extension will not work, as the directory should  be
#' relative to the location of the Praat script.
#' @param filelist String giving the location of a plain text file with the
#' file names of sound files to be processed. Important: these files should be
#' relative to `inputDir`! Alternatively, can be a vector of strings giving
#' the locations of sound files.
#' @param intervalTier Numeric giving an identifier for an interval tier in
#' TextGrids where relevant labels are located.
#' @param includeTheseLabels String giving a well-formed regex of labels to
#' search for in TextGrids. Default is `^(?!\\s*$).+`, i.e. any non-empty
#' interval.
#' @param praatLocation String giving the location of Praat on your computer.
#' The function will fail silently if this string is not correct.
#' Default is `praat`, which will be correct if Praat is stored in `System32`
#' on Windows, or similar locations on a Unix-alike OS. You can test whether
#' this is correct by calling `system(praat)` and checking if a Praat window
#' opens.
#' @param recursive Logical; should sound files in subdirectories of
#' `inputDir` be analyzed? Default is `FALSE`.
#'
#' @return Data frame containing results of chosen measures for each
#' sound file in `inputDir`.
#' @export
#'
#' @examples
#' \dontrun{
#' datapath <- system.file('extdata/audio', package='sauceshelf')
#' sauce <- sauceshelf::praatsauce(datapath)
#' }
praatsauce <- function(inputDir, outputDir = tempdir(), outputFile = 'out.tsv',
                       channel = 1, intervalEquidistant = FALSE,
                       intervalFixed = 0.005, pitch = TRUE, formant = TRUE,
                       harmonicAmplitude = TRUE,
                       harmonicAmplitudeUncorrected = TRUE,
                       bw = TRUE, bwHawksMiller = TRUE, slope = TRUE,
                       slopeUncorrected = TRUE, cpp = TRUE, hnr = TRUE,
                       intensity = TRUE, resample16kHz = FALSE,
                       windowLength = 0.025, f0min = 50, f0max = 300,
                       maxNumFormants = 5, preEmphFrom = 50, f1ref = 500,
                       f2ref = 1500, f3ref = 2500, maxFormantHz = 5000,
                       useTextGrid = FALSE, tgDir = FALSE, filelist = 0,
                       intervalTier = 1, includeTheseLabels = '^(?!\\s*$).+',
                       praatLocation = 'praat', recursive = FALSE) {

  if (class(inputDir) == 'emuDBhandle') {
    inputDir <- inputDir$basePath
    recursive <- TRUE
    emuDB <- TRUE
  } else {
    emuDB <- FALSE
  }

  if (length(filelist) > 1) {
    fl <- filelist
    filelist <- file.path(tempdir(), 'filelist.txt')
    writeLines(fl, con=filelist)
  }

  if (recursive) {
    fl <- list.files(inputDir, pattern='*.wav', recursive=TRUE)
    filelist <- file.path(tempdir(), 'filelist.txt')
    writeLines(fl, con=filelist)
  }

  make_params(inputDir, outputDir, outputFile, channel, intervalEquidistant,
             intervalFixed, pitch, formant, harmonicAmplitude,
             harmonicAmplitudeUncorrected, bw, bwHawksMiller, slope,
             slopeUncorrected, cpp, hnr, intensity, resample16kHz,
             windowLength, f0min, f0max, maxNumFormants, preEmphFrom, f1ref,
             f2ref, f3ref, maxFormantHz, useTextGrid, tgDir, filelist,
             intervalTier, includeTheseLabels)
  praatsauceLocation <- system.file('extdata/praatsauce/praatsauce.praat',
                                           package='sauceshelf')
  praatsauceLocation <- paste0('"', praatsauceLocation, '"')
  paramsLoc <- file.path(outputDir, 'params.csv')
  paramsLoc <- paste0('"', paramsLoc, '"')
  syscall <- paste(praatLocation, praatsauceLocation, paramsLoc)
  system(syscall)

  out <- load_sauce(file.path(outputDir, outputFile), useTextGrid, emuDB)

  return(out)
}
