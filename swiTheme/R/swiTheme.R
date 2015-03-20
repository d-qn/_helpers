##' swissinfo.ch's chart theme
##'
##' swissinfo minimal font and color ggplot2 theme
##' 
##' @name theme_swi
##' @param ticks \code{logical} Show axis ticks?
##' @param base_size Base font size
##' @param base_family Base font family
##' @import ggplot2 scales grid
##' @importFrom extrafont loadfonts choose_font
##' @export
##' @examples
##' qplot(1:10, 1:10, size = 10:1) + xlab("axis x label") + ylab ("y axis label") + theme_swi()
##' 
##' qplot(mtcars$mpg) + theme_swi()
##' 
##' 

theme_swi <- function(ticks=TRUE, base_family="Open Sans", base_size=11) {
  choose_font(base_family, FALSE)
  ret <- theme_minimal(base_family=base_family, base_size=base_size) +
    theme(
      plot.title   = element_text(hjust = 0, vjust = 5, size = rel(2), face = "bold"),
      axis.title.x = element_text(hjust = 1, vjust = 0, size = rel(1.6)),
      axis.title.y = element_text(vjust = 1, hjust = 1, size = rel(1.6)),
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
##' @param base_family2 secondary font family
##' @examples
##' qplot(1:10, 1:10, size = 10:1) + xlab("axis x label") + ylab ("y axis label") + theme_swiYLines()
##' @export 
theme_swiYLines <- function(
  yaxis=FALSE, base_family="Open Sans", base_family2 = "Open Sans Semibold",
  base_size=11, axisColor = "#7E8279") {
  
  choose_font(base_family, FALSE)
  choose_font(base_family2, FALSE)
  
  ret <- theme_minimal(base_family=base_family, base_size=base_size) +
    theme(
      plot.title   = element_text(hjust = 0, vjust = 5, size = rel(2), face = "bold"),
      ## AXIS
      axis.text    = element_text(size = rel(1.3)),
      axis.title   = element_text(size = rel(1.6), family = base_family2, color = axisColor),
      axis.title.x = element_text(hjust = 1, vjust = -0.15),
      axis.title.y = element_text(vjust = 1, hjust = 1),
      axis.line    =  element_line(linetype = "solid", size = 0.9, color = axisColor, lineend = "round"),
      axis.ticks   =  element_line(size = 0.3,  color = axisColor),
      axis.ticks.y =  element_blank(),
      axis.ticks.length = unit(2.5, "mm"),
      ## PLOT MARGIN
      plot.margin = unit(c(2, 1, 1.5, 1), "cm"),
      ## GRID LINES
      panel.grid.major.x = element_blank(), 
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_line(colour = "lightgrey", size = 0.05, lineend = "round"),
      legend.key.size = unit(0.5, "cm")
   )
  if(!yaxis) {
    ret <- ret + theme(axis.line.y = element_blank())
  }
  ret
}

##' swissinfo.ch theme for ggplot2
##' 
##' ggplot2 minimal theme
##' 
##' @rdname theme_swi
##' @inheritParams theme_swi
##' @param axisColor the color for the axis and their ticks and labels
##' @param base_family2 secondary font family
##' @examples
##' qplot(1:10, 1:10, size = 10:1) + xlab("axis x label") + ylab ("y axis label") + theme_swi2()
##' @export 
theme_swi2 <- function(
  base_family="Open Sans", base_family2 = "Open Sans Semibold",
  base_size=11, axisColor = "#7E8279") {
  
  choose_font(base_family, FALSE)
  choose_font(base_family2, FALSE)
  
  ret <- theme_minimal(base_family=base_family, base_size=base_size) +
    theme(
      plot.title   = element_text(hjust = 0, vjust = 5, size = rel(2), face = "bold"),
      ## AXIS
      axis.title   = element_text(size = rel(1.5), family = base_family2, color = axisColor),
      axis.title.x = element_text(hjust = 1, vjust = -0.15),
      axis.title.y = element_text(vjust = 1, hjust = 1),
      ## PLOT MARGIN
      plot.margin = unit(c(2, 1, 1, 1), "lines")
      ## GRID LINES
    )
  ret
}