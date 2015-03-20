##' swissinfo standard chart size pdf export 
##'
##' Simple wrapper for R's graphics device driver (i.e. pdf) to save charts in pre-defined sizes
##'
##' @rdname swi_export
##' @importFrom grDevices pdf 
##' @inheritParams pdf
##' @export
##' 
pdfswi_sq <- function(file = "", width = 10, height = 10, ...) {
  pdf(file, width = width, height = height, ...)
}

##' @rdname swi_export
##' @importFrom grDevices pdf 
##' @inheritParams pdf
##' @export
pdfswi_long <- function(file = "", width = 6, height = width * 1.4 , ...) {
  pdf(file, width = width, height = height, ...)
}
