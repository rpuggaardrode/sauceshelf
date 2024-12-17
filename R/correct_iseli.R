#' Return corrected harmonic amplitudes
#'
#' Correction of harmonic amplitudes for the influence of lower formants.
#'
#' @param f Vector of frequency values of approximate harmonic locations.
#' @param fx Vector of frequency values of formants.
#' @param bx Vector of frequency values of bandwidths.
#' @param fs Sampling rate of the original sound file.
#'
#' @return A numeric vector to be subtracted from the estimated harmonic
#' amplitudes.
#' @export
#'
#' @examples
#' # not now
correct_iseli <- function(f, fx, bx, fs) {
  r <- exp(-pi*bx/fs)
  omega_x <- 2*pi*fx/fs
  omega <- 2*pi*f/fs
  a <- r ^ 2 + 1 - 2*r*cos(omega_x + omega)
  b <- r ^ 2 + 1 - 2*r*cos(omega_x - omega)
  corr <- -10*(log10(a)+log10(b))
  numerator <- r ^ 2 + 1 - 2 * r * cos(omega_x)
  corr <- -10*(log10(a)+log10(b)) + 20*log10(numerator)
  return(corr)
}
