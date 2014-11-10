##' swissinfo standard chart size pdf export 
##'
##' @rdname color_swi
##' @importFrom grDevices pdf 
##' @inheritParams pdf
##' @export
##' 
pdfswi <- function(file = "", width = 10, height = 10, ...) {
  pdf(file, width = width, height = height, ...)
}