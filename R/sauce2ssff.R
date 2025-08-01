#' Convert sauce measures to SSFF files
#'
#' Converts sauce measures to SSFF files and adds them to a loaded EMU database.
#'
#' @param emuDBhandle Handle of a loaded EMU database.
#' @param sauce Data frame containing sauce measures. This should be generated
#' by running e.g. `praatsauce` on the same loaded EMU database.
#' @param removeExisting Logical; should existing SSFF files with the `sauce`
#' extension be removed from the database?
#'
#' @return Used for side effects.
#' @export
#'
#' @examples
#' \donttest{
#' # Create demo data and load demo database
#' emuR::create_emuRdemoData(tempdir())
#' db_path <- paste0(tempdir(), '/emuR_demoData/ae_emuDB')
#' db <- emuR::load_emuDB(db_path)
#'
#' # Which SSFF tracks are available?
#' emuR <- emuR::list_ssffTrackDefinitions(db)
#'
#' # Get sauce
#' sauce <- praatsauce(db)
#' sauce2ssff(db, sauce)
#'
#' # Did it work?
#' emuR::list_ssffTrackDefinitions(db)
#' }
sauce2ssff <- function(emuDBhandle, sauce, removeExisting = FALSE) {

  if (!inherits(emuDBhandle, 'emuDBhandle')) stop(
    'Invalid emuDBhandle argument')

  if (removeExisting) {
    ssffTrax <- emuR::list_ssffTrackDefinitions(emuDBhandle)
    ssffTrax <- ssffTrax[which(ssffTrax$fileExtension == 'sauce'),]
    if (nrow(ssffTrax) > 1) {
      for (i in 2:nrow(ssffTrax)) emuR::remove_ssffTrackDefinition(
        emuDBhandle, ssffTrax$name[i], deleteFiles = FALSE)
      emuR::remove_ssffTrackDefinition(emuDBhandle, ssffTrax$name[1],
                                       deleteFiles = TRUE)
    }
  }

  session <- gsub('/.*', '', sauce$file, perl=T)
  bundle <- gsub('.*/', '', sauce$file, perl=T)
  bundle <- gsub('.wav', '', bundle)
  sauce <- sauce[,-1]
  for (x in 1:ncol(sauce)) sauce[,x] <- as.numeric(sauce[,x])
  sauce <- cbind(session, bundle, sauce)

  ssffDir <- paste0(tempdir(), '/ssff/')
  dir.create(ssffDir)

  sessions <- unique(sauce$session)

  for (s in sessions) {
    sessionDir <- paste0(ssffDir, s)
    dir.create(sessionDir)

    session_sauce <- sauce[which(sauce$session == s),]
    bundles <- unique(session_sauce$bundle)

    for (b in bundles) {
      tmp <- session_sauce[which(session_sauce$bundle == b),]

      ado <- list()
      attr(ado, 'sampleRate') <- 1 / (tmp$t[2] - tmp$t[1])
      attr(ado, 'origFreq') <- 0
      attr(ado, 'startTime') <- tmp$t[1]
      attr(ado, 'endRecord') <- nrow(tmp)
      class(ado) <- 'AsspDataObj'
      wrassp::AsspFileFormat(ado) <- 'SSFF'
      wrassp::AsspDataFormat(ado) <- as.integer(2)

      if ('f0' %in% colnames(tmp)) ado <-
        wrassp::addTrack(ado, 'f0', tmp$f0, format='REAL32')
      if ('F1' %in% colnames(tmp)) ado <-
        wrassp::addTrack(ado, 'fmt', as.matrix(tmp[,c('F1', 'F2', 'F3')]),
                         format=rep('REAL32', 3))
      if ('B1' %in% colnames(tmp)) ado <-
        wrassp::addTrack(ado, 'bw', as.matrix(tmp[,c('B1', 'B2', 'B3')]),
                         format=rep('REAL32', 3))
      if ('HNR05' %in% colnames(tmp)) ado <-
        wrassp::addTrack(ado, 'hnr',
                         as.matrix(tmp[,c('HNR05', 'HNR15', 'HNR25', 'HNR35')]),
                         format=rep('REAL32', 4))
      for (var in c('H1c', 'H2c', 'H4c', 'A1c', 'A2c', 'A3c', 'H2Ku', 'H5Ku',
                    'H1u', 'H2u', 'H4u', 'A1u', 'A2u', 'A3u', 'H1H2c', 'H2H4c',
                    'H1A1c', 'H1A2c', 'H1A3c', 'H2KH5Ku', 'H1H2u', 'H2H4u',
                    'H1A1u', 'H1A2u', 'H1A3u', 'CPP', 'intensity', 'soe')) {
        if (var %in% colnames(tmp)) {
          ado <- wrassp::addTrack(ado, var, tmp[[var]], format='REAL32')
        }
      }
      attr(ado, 'trackFormats') <- rep('REAL32', length(ado))

      ssffFile <- paste0(sessionDir, '/', b, '.sauce')
      wrassp::write.AsspDataObj(ado, file = ssffFile)
    }

    if (!s %in% emuR::list_sessions(emuDBhandle)) {
      if (gsub('_ses', '', s) %in% emuR::list_sessions(emuDBhandle)$name) {
        s <- gsub('_ses', '', s)
      } else {
        stop(paste('Could not find session', s, 'in emuDB'))
      }
    }
    emuR::add_files(emuDBhandle, sessionDir, 'sauce', s)

  }

  for (var in names(ado)) emuR::add_ssffTrackDefinition(
    emuDBhandle, var, var, 'sauce')
}
