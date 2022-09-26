#' @export
box2pkg  <- function() {
  homeDir <- getOption('box.path', 'box')
  dir_exists <- dir.exists(homeDir)
  if (dir_exists) {
    files <- lapply(
      list.files(homeDir, full.names = T),
      function(file) {
        readLines(file)
      }
    )
  } else {
    stop(paste('Directory', homeDir, 'does not exist.'))
  }

}
