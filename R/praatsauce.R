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
                       useTextGrid = FALSE, tgDir = FALSE, intervalTier = 1,
                       includeTheseLabels = '^(?!\\s*$).+',
                       praatLocation = 'praat') {

  make_params(inputDir, outputDir, outputFile, channel, intervalEquidistant,
             intervalFixed, pitch, formant, harmonicAmplitude,
             harmonicAmplitudeUncorrected, bw, bwHawksMiller, slope,
             slopeUncorrected, cpp, hnr, intensity, resample16kHz,
             windowLength, f0min, f0max, maxNumFormants, preEmphFrom, f1ref,
             f2ref, f3ref, maxFormantHz, useTextGrid, tgDir, intervalTier,
             includeTheseLabels)
  praatsauceLocation <- system.file('extdata/praatsauce/praatsauce.praat',
                                    package='sauceplay')
  paramsLoc <- file.path(tempdir(), 'params.csv')
  syscall <- paste(praatLocation, praatsauceLocation, paramsLoc)
  system(syscall)
  out <- read.table(file.path(tempdir(), 'out.tsv'),
                    sep = '\t', header=T)
  out[] <- lapply(out, gsub, pattern = '--undefined--', replacement = '0')
  out[,2:ncol(out)] <- lapply(out[,2:ncol(out)], as.numeric)
  out$file <- gsub('^ *', '', out$file, perl=T)
  return(out)
}
