#' Voice quality measures from a combination of sources
#'
#' Estimates a range of voice quality measures over all sound files in a
#' directory relying on signal processing from EMU-SDMS (via the [emusauce]
#' function), Praat (via the [praatsauce] function), or REAPER.
#'
#' @param inputDir String giving a directory containing sound files to analyze.
#' @param pitch A string giving the desired source of pitch values; should be
#' either `praat` (default), `emu`, or `reaper`. The latter option requires the
#' `reapeR` library to be installed. Alternatively `FALSE` will suppress pitch.
#' @param formant A string giving the desired source of pitch values; should be
#' either `praat` (default) or `emu`. Alternatively `FALSE` will suppress
#' formants.
#' @param bw A string giving the desired source of bandwidth values; should be
#' either `praat` or `emu` (default). Alternatively `FALSE` will suppress
#' bandwidths.
#' @param harmonicAmplitude A string giving the desired source of corrected
#' harmonic amplitude values;  should be either `praat` or `emu` (default).
#' Alternatively `FALSE` will suppress corrected harmonic amplitudes.
#' @param slope A string giving the desired source of corrected
#' spectral slope values;  should be either `praat` or `emu` (default).
#' Alternatively `FALSE` will suppress corrected spectral slope.
#' @param harmonicAmplitudeUncorrected A string giving the desired source of
#' uncorrected harmonic amplitude values;  should be either `praat` or `emu`
#' (default).
#' Alternatively `FALSE` will suppress uncorrected harmonic amplitudes.
#' @param slopeUncorrected A string giving the desired source of uncorrected
#' spectral slope values;  should be either `praat` or `emu` (default).
#' Alternatively `FALSE` will suppress uncorrected spectral slope.
#' @param cpp A string giving the desired source of cepstral peak prominence
#' values;  should be either `praat` (default) or `emu`. Alternatively
#' `FALSE` will suppress CPP.
#' @param hnr A string giving the desired source of harmonics-to-noise ratio
#' values;  should be either `praat` (default) or `emu`.
#' `emu` is experimental and quite slow, mostly included for completeness.
#' Alternatively  `FALSE` will suppress corrected spectral slope.
#' @param intensity A string giving the desired source of intensity
#' values;  should be either `praat` (default) or `emu`. Alternatively
#' `FALSE` will suppress intensity.
#' @param soe A string giving the desired source of strength of excitation
#' values;  should be either `praat` or `emu` (default). Alternatively
#' `FALSE` will suppress corrected spectral slope.
#' @param intervalFixed Numeric; how often should measures be taken (in
#' seconds)? Default is `0.005`.
#' @param f0min Integer giving the pitch floor. Default is `50`.
#' @param f0max Integer giving the pitch ceiling. Default is `300`.
#' @param windowLength Numeric giving the length of the analysis window (in
#' seconds). Default is `0.025`.
#' @param maxNumFormants Integer giving the maximum number of formants to
#' estimate. Default is `5`.
#' @param bw_hawksMiller Boolean; should bandwidths be estimated using the
#' Hawks-Miller formula? Default is `TRUE`. If `FALSE`, raw bandwidths are
#' returned.
#' @param recursive Logical; should sound files in subdirectories of
#' `inputDir` be analyzed? Default is `FALSE`.
#' @param reaperPath String giving the path to a REAPER installation on the
#' computer. Default is `NULL`; it is assumed that REAPER is installed along
#' with the `reapeR` library.
#' @param ... Further arguments passed to [praatsauce].
#'
#' @return Data frame with the specified measures.
#' @export
#'
#' @examples
#' datapath <- system.file('extdata/audio', package='sauceshelf')
#' sauce <- mixedsauce(datapath)
mixedsauce <- function(inputDir, pitch = 'praat', formant = 'praat', bw = 'emu',
                       harmonicAmplitude = 'emu', slope = 'emu',
                       harmonicAmplitudeUncorrected = 'emu',
                       slopeUncorrected = 'emu', cpp = 'praat', hnr = 'praat',
                       intensity = 'praat', soe = 'emu',
                       intervalFixed = 0.005, f0min = 50, f0max = 300,
                       windowLength = 0.025, maxNumFormants = 5,
                       bw_hawksMiller = TRUE, recursive = FALSE,
                       reaperPath = NULL, ...) {

  if (pitch == 'reaper') {
    if (system.file(package='reapeR') == '') stop(paste(
      'Please install the reapeR library using e.g.',
      'devtools::install_github("rpuggaardrode/reapeR")'))
    ro <- file.path(tempdir(), 'reaper')
    dir.create(ro)
    tmp <- reapeR::reaper_bulk(inputDir, output = 'pitch', f0min = f0min,
                       f0max = f0max, exePath = reaperPath,
                       interval = intervalFixed,
                       praat_output = TRUE, praat_output_dir = ro)
    pitchRead <- TRUE
    pitchReadDir <- ro
    ppitch <- TRUE
    pitch <- 'praat'
  } else {
    pitchRead <- FALSE
    pitchReadDir <- NULL
  }

  ppitch <- ifelse(pitch == 'praat', TRUE, FALSE)
  pfmt <- ifelse(formant == 'praat', TRUE, FALSE)
  pbw <- ifelse(bw == 'praat', TRUE, FALSE)
  pha <- ifelse(harmonicAmplitude == 'praat', TRUE, FALSE)
  pslope <- ifelse(slope == 'praat', TRUE, FALSE)
  phau <- ifelse(harmonicAmplitudeUncorrected == 'praat', TRUE, FALSE)
  pslopeu <- ifelse(slopeUncorrected == 'praat', TRUE, FALSE)
  pcpp <- ifelse(cpp == 'praat', TRUE, FALSE)
  phnr <- ifelse(hnr == 'praat', TRUE, FALSE)
  prms <- ifelse(intensity == 'praat', TRUE, FALSE)
  psoe <- ifelse(soe == 'praat', TRUE, FALSE)

  if (sum(ppitch, pfmt, pbw, pha, pslope, phau, pslopeu, pcpp, phnr,
          prms, psoe) > 1) {
    ps <- praatsauce(inputDir, pitch = ppitch, formant = pfmt,
                     harmonicAmplitude = pha, slope = pslope,
                     bw = pbw, harmonicAmplitudeUncorrected = phau,
                     slopeUncorrected = pslopeu, cpp = pcpp,
                     hnr = phnr, intensity = prms, soe = psoe,
                     intervalFixed = intervalFixed, f0min = f0min,
                     f0max = f0max, windowLength = windowLength,
                     maxNumFormants = maxNumFormants,
                     bwHawksMiller = bw_hawksMiller, pitchRead = pitchRead,
                     pitchReadDir = pitchReadDir, recursive = recursive, ...)
    ps$t <- as.numeric(ps$t)
  } else {
    ps <- NULL
  }

  epitch <- ifelse(pitch == 'emu', TRUE, FALSE)
  efmt <- ifelse(formant == 'emu', TRUE, FALSE)
  ebw <- ifelse(bw == 'emu', TRUE, FALSE)
  eha <- ifelse(harmonicAmplitude == 'emu', TRUE, FALSE)
  eslope <- ifelse(slope == 'emu', TRUE, FALSE)
  ehau <- ifelse(harmonicAmplitudeUncorrected == 'emu', TRUE, FALSE)
  eslopeu <- ifelse(slopeUncorrected == 'emu', TRUE, FALSE)
  ecpp <- ifelse(cpp == 'emu', TRUE, FALSE)
  ehnr <- ifelse(hnr == 'emu', TRUE, FALSE)
  erms <- ifelse(intensity == 'emu', TRUE, FALSE)
  esoe <- ifelse(soe == 'emu', TRUE, FALSE)

  if (sum(epitch, efmt, ebw, eha, eslope, ehau, eslopeu, ecpp, ehnr,
          erms, esoe) > 1) {
    out <- emusauce(inputDir, pitch = epitch, formant = efmt,
                    harmonicAmplitude = eha, slope = eslope,
                    bw = ebw, harmonicAmplitudeUncorrected = ehau,
                    slopeUncorrected = eslopeu, cpp = ecpp,
                    hnr = ehnr, intensity = erms, soe = esoe,
                    intervalFixed = intervalFixed, f0min = f0min,
                    f0max = f0max, windowLength = windowLength,
                    maxNumFormants = maxNumFormants,
                    bw_hawksMiller = bw_hawksMiller,
                    existing_output = ps)
  } else {
    out <- ps
  }

  return(out)

}
