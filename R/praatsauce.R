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
  praatsauceLocation <- paste0(system.file('extdata/praatsauce/praatsauce.praat',
                                           package='sauceshelf'))
  praatsauceLocation <- paste0('"', praatsauceLocation, '"')
  paramsLoc <- file.path(tempdir(), 'params.csv')
  paramsLoc <- paste0('"', paramsLoc, '"')
  syscall <- paste(praatLocation, praatsauceLocation, paramsLoc)
  system(syscall)

  out <- read.table(file.path(tempdir(), 'out.tsv'),
                    sep = '\t', header=T)
  out[] <- lapply(out, gsub, pattern = '--undefined--', replacement = '0')
  out[,2:ncol(out)] <- lapply(out[,2:ncol(out)], as.numeric)
  out$file <- gsub('^ *', '', out$file, perl=T)

  if (emuDB) {
    session <- gsub('_.*$', '', out$file, perl=T)
    bundle <- gsub('.*/', '', out$file, perl=T)
    bundle <- gsub('.wav', '', bundle)
    out <- out[,-1]
    out <- cbind(session, bundle, out)
  }

  return(out)
}
