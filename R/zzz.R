.onAttach <- function(lib, pkg) {
  if (stats::runif(1) > .8) {
    msg <- c("42")
    packageStartupMessage(msg)
  }
}
