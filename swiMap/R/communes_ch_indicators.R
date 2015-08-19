##' Swiss communes socio-demographic indicators
##'
##' Load socio-demographic data by the Swiss Statistical Office.  
##' 
##' @return a matrix
##' @seealso Office fédéral de la statistique > Les Régions > Communes > Données et explications (portraits): http://www.bfs.admin.ch/bfs/portal/fr/index/regionen/02/daten.html
##' @details The matrix has the commune BFS code as rowname and 3 attributes: \code{communeName} the text name of the commune (same length as the matrix length),  \code{indicatorYear} & \code{indicatorGroup} respectively the year and the category of the communal indicator (both of same length as the matrix width). See \url{http://www.bfs.admin.ch/bfs/portal/fr/index/regionen/02/key.html}
##' @export
##' @examples
##' communeData <- loadCommunesCHportraits()
##' colnames(communeData)
##' rownames(communeData)
##' 
##' # Select only "surface" indicators
##' colIdx <- which(attr(communeData, "indicatorGroup") == "Surface")
##' head(communeData[,colIdx])
##' 
##' 
loadCommunesCHportraits <- function() {

  # get the path to communes data txt file 
  data.path <- dir(system.file("extdata", package="swiMap"), "communesCH_2015_indicators_je-f-21.03.01.csv", full.names = T)
  data.read <- read.csv(data.path, skip = 2, header = TRUE, stringsAsFactors = F, check.names = FALSE)
 
   # save ony the indicator values as a matrix
  data <- data.matrix(data.read[,-c(1:2)])
  
  # rownames are commune BFS code
  rownames(data) <- data.read[,1]
  # attr communeName is the text name
  attr(data, "communeName") <- data.read[,2]
  
  metadata <- read.csv(data.path, nrows = 1, header = TRUE, stringsAsFactors = F, check.names = FALSE)

  attr(data, "indicatorYear") <- unlist(metadata)[-c(1:2)]
  attr(data, "indicatorGroup")<- names(metadata)[-c(1:2)]
  
  data
}