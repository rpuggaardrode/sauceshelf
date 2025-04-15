#' Install REAPER
#'
#' Install David Talkin's Robust Epoch and Pitch Estimator program as part
#' of the `sauceshelf` R package.
#'
#' @return Used for side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' reaper_install()
#' }
reaper_install <- function() {
  curwd <- getwd()
  on.exit(setwd(curwd))
  dest <- paste0(system.file('bin', package='sauceshelf'))
  if (dir.exists(dest)) stop('REAPER already installed')
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  setwd(dest)
  system('git clone https://github.com/google/REAPER.git')
  setwd('REAPER')
  dir.create('build')
  setwd('build')
  system('cmake ..')
  system('make')
  exec <- 'reaper'
  if (Sys.info()['sysname'] == 'Windows') exec <- paste0(exec, '.exe')
  file.rename(exec, paste0(dest, '/', exec))
  setwd(dest)
  unlink('REAPER', recursive = TRUE, force = TRUE)
  setwd(curwd)
}
