##' Multiple plot function 
##' 
##' To arrange multiple ggplot chart on a grid. Copied from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
##'
##'  Arrange different ggplot objects on the same page
##'
##' @param plots a list of ggplot objects
##' @param cols Number of columns in layout
##' @param layout A matrix specifying the layout. If present, 'cols' is ignored.
##' @details If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then plot 1 will go in the upper left, 2 will go in the upper right, and ll go all the way across the bottom.
##' @import gridExtra ggplot2
##' @importFrom grid unit 
##' @importFrom grid grid.newpage
##' @importFrom grid pushViewport
##' @importFrom grid viewport
##' @importFrom grid grid.layout
##' @export
##' @examples
##' require(ggplot2)
##' q1 <- qplot(1:10, 1:10, size = 10:1) + xlab("axis x label") + ylab ("y axis label") + theme_swi2()
##' q2 <- qplot(mpg, data = mtcars, geom = "dotplot") + theme_swi()
##' multiplot(list(q1, q2))
multiplot <- function(plotlist=NULL, cols=1, layout=NULL) {
  
  stopifnot(is.list(plotlist))
  numPlots = length(plotlist)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plotlist[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plotlist
            [[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

