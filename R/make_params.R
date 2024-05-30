make_params <- function(inputDir, outputDir = tempdir(), outputFile = 'out.tsv',
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
                        includeTheseLabels = '^(?!\\s*$).+') {

  fileLoc <- file.path(tempdir(), 'params.csv')
  if (file.exists(fileLoc)) file.remove(fileLoc)

  p <- data.frame(
    variable = c('inputDir', 'outputDir', 'outputFile', 'channel',
                 'intervalEquidistant', 'intervalFixed', 'pitch', 'formant',
                 'harmonicAmplitude', 'harmonicAmplitudeUncorrected', 'bw',
                 'bwHawksMiller', 'slope', 'slopeUncorrected', 'cpp', 'hnr',
                 'intensity', 'resample16kHz', 'windowLength', 'f0min', 'f0max',
                 'maxNumFormants', 'preEmphFrom', 'f1ref', 'f2ref', 'f3ref',
                 'maxFormantHz', 'useTextGrid', 'tgDir', 'intervalTier',
                 'includeTheseLabels'),
    input = c(inputDir, outputDir, outputFile, channel,
              as.numeric(intervalEquidistant), intervalFixed, as.numeric(pitch),
              as.numeric(formant), as.numeric(harmonicAmplitude),
              as.numeric(harmonicAmplitudeUncorrected), as.numeric(bw),
              as.numeric(bwHawksMiller), as.numeric(slope),
              as.numeric(slopeUncorrected), as.numeric(cpp), as.numeric(hnr),
              as.numeric(intensity), as.numeric(resample16kHz), windowLength,
              f0min, f0max, maxNumFormants, preEmphFrom, f1ref, f2ref, f3ref,
              maxFormantHz, as.numeric(useTextGrid),
              as.numeric(tgDir), intervalTier, includeTheseLabels)
  )

  write.csv(p, file=fileLoc, row.names=F, quote=F)

}
