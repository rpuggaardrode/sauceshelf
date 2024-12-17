#' Estimate bandwidths using the Hawks-Miller formula
#'
#' Formula estimation of formant bandwidths based on real pitch and formant
#' measurements.
#'
#' @param f0 Numeric vector of pitch values.
#' @param fmt Numeric vector of formant values.
#'
#' @return Numeric vector of estimated bandwidths values.
#' @export
#'
#' @examples
#' # not now
get_bwHawksMiller <- function(f0, fmt) {
  s <- 1 + 0.25*(f0-132)/88
  k <- c(15.8146139, 165.327516)
  coef <- c(8.10159009e-2, -9.79728215e-5, 5.28725064e-8,
            -1.07099364e-11, 7.91528509e-16,
            -6.73636734e-1, 1.80874446e-3, -4.52201682e-6,
            7.49514000e-9, -4.70219241e-12)
  fbw <- ifelse(fmt > 500,
                s*(k[1]+(coef[1] * fmt)+(coef[2] * fmt^2)+(coef[3] * fmt^3) +
                     (coef[4] * fmt^4)+(coef[5] * fmt^5)),
                s*(k[2]+(coef[6] * fmt)+(coef[7] * fmt^2)+(coef[8] * fmt^3) +
                     (coef[9] * fmt^4)+(coef[10] * fmt^5)))
  return(fbw)
}
