#' Slopegraph charts
#' 
#' Plot a minimalistic Tuffte style slopegraph chart
#' 
#' @param startpts, endpts numeric vectors of same length
#' @param labels a character vector of same length as \code{startpts}
#' @references http://flowingdata.com/2013/06/03/how-to-make-slopegraphs-in-r/
#' @export
#' @examples
#' pctgdp <- data.frame(country = factor(c("Sweden", "Netherlands", "Norway", "Britain", "France", "Germany", "Belgium",
#'   "Canada", "Finland", "Italy","United States","Greece", "Switzerland", "Spain", "Japan")),
#' 	pct1970 = c(46.9,44,43.5,40.7,39,37.5,35.2,35.2,34.9,30.4,30.3,26.8,26.5,22.5,20.7),
#' 	pct1979 = c(57.4,55.8,52.2,39,43,43,43,35.8,38.2,35.7,32.5,30.6,33.2,27.1,26.6)
#' )
#' slopegraph2(pctgdp$pct1970, pctgdp$pct1979, pctgdp$country)


slopegraph2 <- function(startpts, endpts, labels) {
  
  x0 <- c()
  y0 <- c()
  x1 <- c()
  y1 <- c()
  
  startyear <- 1970
  stopyear <- 1979
  xoffset <- 2
  yoffset <- 0
  ystartprev <- 0
  ystopprev <- 0
  ythreshold <- ( max(startpts) - min(startpts) ) * 0.025
  
  for (i in length(startpts):1) {
    
    ystartdiff <- (startpts[i]+yoffset) - ystartprev
    if (abs(ystartdiff) < ythreshold) {
      yoffset <- yoffset + (ythreshold-ystartdiff)
    }
    
    # Calculate slope
    slope <- (endpts[i] - startpts[i]) / (stopyear - startyear)
    
    # Intercept
    intercept <- startpts[i] + yoffset
    
    # Start and stop coordinates for lines
    ystart <- intercept
    ystop <- slope * (stopyear-startyear) + intercept
    ystopdiff <- ystop - ystopprev
    if (abs(ystopdiff) < ythreshold) {
      yoffset <- yoffset + (ythreshold-ystopdiff)
      intercept <- startpts[i] + yoffset
      ystart <- intercept
      ystop <- slope * (stopyear-startyear) + intercept
    }
    
    # Draw the line for current country
    x0 <- c(x0, startyear)
    y0 <- c(y0, ystart)
    x1 <- c(x1, stopyear)
    y1 <- c(y1, ystop)
    
    
    ystartprev <- ystart
    ystopprev <- ystop
  }
  
  ymin <- min(startpts)
  ymax <- max(c(startpts, endpts)) + yoffset
  
  par(family="serif", mar=c(0,0,0,0))
  plot(0, 0, type="n", main="", xlab="", ylab="", xlim=c(1950,1990), ylim=c(ymin,ymax*1.1), bty="n", las=1, axes=FALSE)
  segments(x0, y0, x1, y1)
  text(x0, y0, rev(startpts), pos=2, cex=0.6)
  text(x0-xoffset, y0, rev(labels), pos=2, cex=0.6)
  text(x1, y1, rev(endpts), pos=4, cex=0.6)
  text(x1+xoffset, y1, rev(labels), pos=4, cex=0.6)
  
  # Year labels
  text(startyear, ymax*1.1, deparse(substitute(startpts)), cex=0.7, pos=2, offset=1)
  text(stopyear, ymax*1.1, deparse(substitute(endpts)), cex=0.7, pos=4, offset=0.5)
}
