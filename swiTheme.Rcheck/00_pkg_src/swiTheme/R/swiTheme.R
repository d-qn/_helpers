##' swissinfo.ch's chart theme
##'
##' swissinfo minimal font and color ggplot2 theme
##' 
##' @name theme_swi
##' @param ticks \code{logical} Show axis ticks?
##' @param base_size Base font size
##' @param base_family Base font family
##' @import ggplot2 scales grid
##' @importFrom extrafont loadfonts
##' @export
##' @examples
##' qplot(1:10, 1:10, size = 10:1) + xlab("axis x label") + ylab ("y axis label") + theme_swi()
##' 
##' qplot(mtcars$mpg) + theme_swi()
##' 
##' 

theme_swi <- function(ticks=TRUE, base_family="Open Sans", base_size=11) {
  choose_font(base_family)
  ret <- theme_minimal(base_family=base_family, base_size=base_size) +
    theme(
      #plot.title = element_text(hjust = 0, size = rel(1.2), face = "bold"),
      axis.title.x = element_text(hjust = 1, vjust = 0, size = rel(1.6)),
      axis.title.y = element_text(vjust = 1, hjust = 1, size = rel(1.6), angle = 0),
      axis.line         =  element_line(linetype = "solid", size = 0.1),
      plot.margin = unit(c(2, 1, 1, 1), "lines"),
      panel.grid = element_blank()
      )
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  ret
}

##' swissinfo.ch scatter plot theme for ggplot2
##' 
##' ggplot2 theme with horizontal grid lines
##' 
##' @rdname theme_swi
##' @inheritParams theme_swi
##' @param axisColor the color for the axis and their ticks and labels 
##' @examples
##' qplot(1:10, 1:10, size = 10:1) + xlab("axis x label") + ylab ("y axis label") + theme_swiYLines()
##' @export 
theme_swiYLines <- function(yaxis=FALSE,base_family="Open Sans", base_size=11, axisColor = "#7E8279") {
  choose_font(base_family)
  
  ret <- theme_minimal(base_family=base_family, base_size=base_size) +
    theme(
      plot.title   = element_text(hjust = 0, vjust = 5, size = rel(2), face = "bold"),
      ## AXIS
      axis.text    = element_text(size = rel(1.3)),
      axis.title.x = element_text(hjust = 1, vjust = 0, size = rel(1.5), face = "bold", color = axisColor),
      axis.title.y = element_text(vjust = 1, hjust = 1, size = rel(1.5), face = "bold", color = axisColor),
      axis.line    =  element_line(linetype = "solid", size = 0.7, color = axisColor, lineend = "round"),
      axis.ticks   =  element_line(size = 0.6,  color = axisColor),
      axis.ticks.y =  element_blank(),
      ## PLOT MARGIN
      plot.margin = unit(c(2, 1, 1, 1), "cm"),
      ## GRID LINES
      panel.grid.major.x = element_blank(), 
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line(colour = "lightgrey", size = 0.005, lineend = "round"),
      legend.key.size = unit(0.5, "cm")
   )
  if(!yaxis) {
    ret <- ret + theme(axis.line.y = element_blank())
  }
  ret
}


##' swissinfo standard square chart
##'
##' @rdname theme_swi
##' @export
##' 
pdfswi <- function(file = "", widthFig = 10, heightFig = 10, ...) {
  pdf(file, width = widthFig, height = heightFig, ...)
}



##' swissinfo standard color palette
##'
##' @rdname theme_swi
##' @export
##' @examples
##' pie(rep(1,length(swi_pal)), col=swi_pal)
swi_pal <- c("#336666", "#368596", "#669999", "#366096",
                   "#333366", "#666699", "#996699", "#663333",
                   "#ab3d3f", "#996666", "#ac673e", "#ac7f3e",
                   "#666633", "#999966", "#89a23a", "#3a9736",
                   "#aa8959", "#bfa681", "#d4c3aa", "#e5dbcd",
                   "#efe9e0", "#f7f5ed")

##' swissinfo random color palette
##'
##' @rdname theme_swi
##' @export
##' @examples
##' pie(rep(1,length(swi_rpal)), col=swi_rpal)
swi_rpal <- swi_pal[c(9, 6, 3, 8, 1, 4, 7, 11, 16, 5, 14, 13, 15, 2, 10, 18, 20, 19, 12, 17, 22, 21)]

## swissinfo diverging color palette with 10 levels
##' @rdname theme_swi
##' @export
##' @examples
##' pie(rep(1,length(swi_dpal)), col=swi_dpal)
swi_dpal <- c("#476666", "#537373", "#5F7F7F", "#6C8C8C", "#789A99", "#DBBF8B", "#CCB27E", "#BEA472", "#B09766", "#A28959")
