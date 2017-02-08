##' Simple convenience function to load text data
##'
##' loadTranslation(): load a csv file and discard verbosely incomplete columns
##'
##' @param path, a string path to a csv file with
##' @export

loadTranslation <- function (path, ...) {
  if(file.exists(path)) {
    txt <- read.csv(path, row.names = 1, stringsAsFactors = F, ...)
    # discard incomplete translation columns
    cidx <- unique(which(txt =="" | is.na(txt), T)[,2])
    if(length(cidx > 0)) {
      warning(paste(colnames(txt)[cidx], collapse = "\t"), " languages will be discarded!", "\n")
      txt <- txt[,-cidx, drop = F]
    }
    cat("\n Remaining column names: ", colnames(txt))
    txt
  } else {
    stop("\n", path, "Not file can't be found with the path provided!\n")
  }
}
