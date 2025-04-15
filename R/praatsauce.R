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
#' @param outputDir String giving the location of a directory to store the
#' output file. Default is to store it in a temporary directory.
#' @param outputFile String giving the name of the PraatSauce output file.
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
#' @param soe Logical; should strength of excitation be reported? Default is
#' `TRUE`.
#' @param resample16kHz Logical; should files be resampled to 16 kHz before
#' analysis? Default is `FALSE`.
#' @param windowLength Numeric giving the length of the analysis window (in
#' seconds). Default is `0.025`.
#' @param f0min Integer giving the pitch floor. Default is `50`.
#' @param f0max Integer giving the pitch ceiling. Default is `300`.
#' @param pitchMethod String giving the name of the method to use for
#' estimating pitch. Valid options are `'autocorrelation'` (default) and
#' `'crosscorrelation'`.
#' @param pitchWindowShape String giving the window shape to use for
#' estimating pitch. Valid options are `'Hanning'` (default) and `'Gaussian'`.
#' @param pitchMaxNoCandidates Integer giving the maximum number of pitch
#' candidates to estimate. Default is `15`.
#' @param silenceThreshold Numeric giving the silence threshold when estimating
#' pitch in terms of amplitude relative to the global maximum. Default is `0.01`.
#' @param voicingThreshold Numeric giving the voicing threshold when estimating
#' pitch in terms of fractional strength in the {auto|cross}correlation function.
#' Default is `NULL`, in which case the value depends on `pitchMethod`:
#' `0.5` is used with `pitchMethod='autocorrelation'` and `0.55` is used with
#' `pitchMethod='crosscorrelation'`.
#' @param octaveCost Numeric specifying how much high frequency pitch candidates
#' should be favored in terms of fractional strength in the {auto|cross}
#' correlation function. Default is `NULL`, in which case the value depends on
#' `pitchMethod`: `0.055` is used with `pitchMethod='autocorrelation'` and
#' `0.01` is used with `pitchMethod='crosscorrelation`.
#' @param octaveJumpCost Numeric specifying how much pitch changes should be
#' disfavored in terms of fractional strength in the {auto|cross} correlation
#' function. Default is `0.35`.
#' @param voicedUnvoicedCost Numeric specifying how much transitions in
#' voicing value should be disfavored in terms of fractional strength in
#' the {auto|cross} correlation function. Default is `0.14`.
#' @param killOctaveJumps Boolean; should Praat's `Kill octave jumps` function
#' be run on the pitch track? Default is `TRUE`.
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
#' @param pitchSynchronous Boolean; should harmonic amplitudes be estimated
#' using window sizes derived synchronously from the local pitch? Default is
#' `FALSE`.
#' @param cppTrendType String giving the type of regression to apply to the
#' cepstrum for estimating CPP. Valid options are `'exponential'` (default)
#' and `'linear'`.
#' @param cppFast Boolean specifying whether a much faster but slightly less
#' precise regression implementation should be used for estimating CPP.
#' Default is `TRUE`.
#' @param pitchSave Boolean; should pitch files be saved to disk? Default is
#' `FALSE`.
#' @param pitchSaveDir String giving the location to use for storing pitch
#' files. Default is `NULL`.
#' @param pitchRead Boolean; should pitch files be read from disk? Default is
#' `FALSE`.
#' @param pitchReadDir String giving the location where pitch files are stored.
#' Default is `NULL.`
#' @param formantSave Boolean; should formant files be saved to disk? Default
#' is `FALSE`.
#' @param formantSaveDir String giving the location to use for storing
#' formant files. Default is `NULL`.
#' @param formantRead Boolean; should formant files be read from disk? Default
#' is `FALSE`.
#' @param formantReadDir String giving the location where formant files are
#' stored. Default is `NULL`.
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
#' @param os String giving the name of the operating system (either `Windows`,
#' `Mac`, or `Linux`). Default is `NULL`. If a string is given, the function
#' will attempt to find Praat in the default location for your operating system.
#' @param recursive Logical; should sound files in subdirectories of
#' `inputDir` be analyzed? Default is `FALSE`.
#' @param na_output How should infelicitous values be coded? Default is `NA`.
#'
#' @return Data frame containing results of chosen measures for each
#' sound file in `inputDir`.
#' @export
#'
#' @examples
#' \dontrun{
#' datapath <- system.file('extdata/audio', package='sauceshelf')
#' sauce <- praatsauce(datapath)
#' }
praatsauce <- function(inputDir, outputDir = tempdir(), outputFile = 'out.tsv',
                       channel = 1, intervalEquidistant = FALSE,
                       intervalFixed = 0.005, pitch = TRUE, formant = TRUE,
                       harmonicAmplitude = TRUE,
                       harmonicAmplitudeUncorrected = TRUE,
                       bw = TRUE, bwHawksMiller = TRUE, slope = TRUE,
                       slopeUncorrected = TRUE, cpp = TRUE, hnr = TRUE,
                       intensity = TRUE, soe = TRUE, resample16kHz = FALSE,
                       windowLength = 0.025, f0min = 50, f0max = 300,
                       pitchMethod = 'autocorrelation',
                       pitchWindowShape = 'hanning', pitchMaxNoCandidates = 15,
                       silenceThreshold = 0.01, voicingThreshold = NULL,
                       octaveCost = NULL, octaveJumpCost = 0.35,
                       voicedUnvoicedCost = 0.14, killOctaveJumps = TRUE,
                       maxNumFormants = 5, preEmphFrom = 50, f1ref = 500,
                       f2ref = 1500, f3ref = 2500, maxFormantHz = 5000,
                       pitchSynchronous = FALSE, cppTrendType = 'exponential',
                       cppFast = TRUE, pitchSave = FALSE, pitchSaveDir = NULL,
                       pitchRead = FALSE, pitchReadDir = NULL,
                       formantSave = FALSE, formantSaveDir = NULL,
                       formantRead = FALSE, formantReadDir = NULL,
                       useTextGrid = FALSE, tgDir = NULL, filelist = 0,
                       intervalTier = 1, includeTheseLabels = '^(?!\\s*$).+',
                       praatLocation = 'praat', os = NULL, recursive = FALSE,
                       na_output = NA) {

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

  praatsauce_makeParams(inputDir, outputDir, outputFile, channel,
                        intervalEquidistant, intervalFixed, pitch, formant,
                        harmonicAmplitude, harmonicAmplitudeUncorrected, bw,
                        bwHawksMiller, slope, slopeUncorrected, cpp, hnr,
                        intensity, soe, resample16kHz, windowLength, f0min,
                        f0max, pitchMethod, pitchWindowShape,
                        pitchMaxNoCandidates, silenceThreshold,
                        voicingThreshold, octaveCost, octaveJumpCost,
                        voicedUnvoicedCost, killOctaveJumps, maxNumFormants,
                        preEmphFrom, f1ref, f2ref, f3ref, maxFormantHz,
                        pitchSynchronous, cppTrendType, cppFast, pitchSave,
                        pitchSaveDir, pitchRead, pitchReadDir, formantSave,
                        formantSaveDir, formantRead, formantReadDir,
                        useTextGrid, tgDir, filelist, intervalTier,
                        includeTheseLabels)
  praatsauceLocation <- system.file('extdata/praatsauce/praatsauce.praat',
                                           package='sauceshelf')
  praatsauceLocation <- paste0('"', praatsauceLocation, '"')
  paramsLoc <- file.path(outputDir, 'params.csv')
  paramsLoc <- paste0('"', paramsLoc, '"')

  if (!is.null(os)) {
    if (os == 'Windows') {
      if (file.exists('"C:/Program Files/Praat.exe"')) {
        praatLocation <- '"C:/Program Files/Praat.exe"'
      } else {
        stop('Could not find Praat, please specify location with praatLocation')
      }
    } else if (os == 'Mac') {
      if (file.exists('/Applications/Praat.app/Contents/MacOS/Praat')) {
        praatLocation <- '/Applications/Praat.app/Contents/MacOS/Praat'
      } else {
        stop('Could not find Praat, please specify location with praatLocation')
      }
    } else if (os == 'Linux') {
      if (file.exists('/usr/bin/praat')) {
        praatLocation <- '/usr/bin/praat'
      } else {
        stop('Could not find Praat, please specify location with praatLocation')
      }
    } else {
      stop('The os parameter has to be either Windows, Mac, or Linux')
    }
  }

  syscall <- paste(praatLocation, '--run', praatsauceLocation, paramsLoc)
  system(syscall)
  # sys::exec_wait(syscall)

  out <- praatsauce_load(file.path(outputDir, outputFile), useTextGrid, emuDB,
                         na_output)

  return(out)
}
