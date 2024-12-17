reaper_install <- function() {
  curwd <- getwd()
  on.exit(setwd(curwd))
  dest <- paste0(system.file('extdata', package='sauceshelf'), '/bin')
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
