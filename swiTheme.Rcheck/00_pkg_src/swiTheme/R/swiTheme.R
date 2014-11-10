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
##' qplot(1:10, 1:10, size = 10:1) + theme_swi() + scale_y_continuous(name = "y axis label", limits = expand = c(0.01,0.01))
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
##' @examples
##' qplot(1:10, 1:10, size = 10:1) + xlab("axis x label") + ylab ("y axis label") + theme_swiYLines()
##' @export 
theme_swiYLines <- function(ticks=TRUE,base_family="Open Sans", base_size=11) {
  choose_font(base_family)
  
  ret <- theme_minimal(base_family=base_family, base_size=base_size) +
    theme(
      #plot.title = element_text(hjust = 0, size = rel(1.2), face = "bold"),
      axis.title.x = element_text(hjust = 1, vjust = 0, size = rel(1.6)),
      axis.title.y = element_text(vjust = 1, hjust = 1, size = rel(1.6), angle = 0),
      axis.line.x         =  element_line(linetype = "solid", size = 1.1),
      axis.line.y         =  element_blank(),
      plot.margin = unit(c(2, 1, 1, 1), "lines"),
      panel.grid.major.x = element_blank(), 
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line(colour = "red")
   )
}