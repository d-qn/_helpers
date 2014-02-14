library(ggplot2)
library(RColorBrewer)
library(reshape)
library(plyr)
library(scales)
library(extrafont)
require(gridExtra)
suppressWarnings(loadfonts())


############################################################################################
###    Minimalistic ggplot theme, no fuss
############################################################################################

ggtheme <- {
    #eliminates baground, gridlines, and chart border
  theme_bw() + theme(
   plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
   panel.border = element_blank(), panel.background = element_blank()
 )
}

ggtheme2 <- {
    #eliminates baground, gridlines, chart border and axis
  theme_bw() + theme(
   plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
   panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
   axis.text = element_blank()
 )
}


############################################################################################
###   Path to swiss maps shapfiles
############################################################################################

swissMapShp.path <- "~/swissinfo/swiss-maps/swiss-maps/shp/ch"
swissMapShp2012.path <- "~/swissinfo/swiss-maps/swiss-maps/shp2012/ch"
swissMapShp2010.path <- "~/swissinfo/swiss-maps/swiss-maps/shp2010/ch"


## Layers available: country, lakes, municipalities, districts, cantons

############################################################################################
###   format shapefiles for ggplot (https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles)
############################################################################################

formatShp <- function(shpF) {
	shpF@data$id <- rownames(shpF@data)
	shpF.points <- fortify(shpF, region = "id")
	shpF.df <- join(shpF.points, shpF@data, by = "id")
	shpF.df
}



############################################################################################
###    Slopegraph from https://github.com/jkeirstead/r-slopegraph
############################################################################################


##' EXAMPLE
# test <- data.frame(group = factor(rep(letters[1:5], 3)), x = rep(c(2008, 2010, 2012), 5), value = 1:15)
#
# #Convert raw data to right format
# df <- build_slopegraph(test, x="x", y="value", group="group", method="tufte", min.space=0.05)
#
# # Refactor the x-axis to get the right labels, round the y values for presentation
# df <- transform(df, x=factor(x),y=round(y))
# plot_slopegraph(df) + labs(title="Test")


##' @title R script for creating slopegraphs
##' @author James Keirstead
##' 12 December 2013
##' http://www.jameskeirstead.ca/r/slopegraphs-in-r/

##' Build a slopegraph data set
##'
##' Modifies a data frame so that it can be used for plotting by
##' \code{plot_slopegraph}.  The general structure of a slopegraph is
##' \itemize{
##' \item a factor giving the group labels
##' \item an ordered factor giving the x intervals
##' \item a numeric giving the y values
##' }
##'
##' @param df the raw data frame
##' @param x a character giving the name of the x-axis column.  This
##' column must be an ordered factor.
##' @param y a character giving the name of the y-axis column.  This
##' column must be a numeric.
##' @param group a character giving the name of the group column.
##' This column must be a factor.
##' @param method a character string indicating which method to use to
##' calculate the position of elements.  Values include "tufte"
##' (default), "spaced", "rank", "none".
##' @param min.space fraction of total data range to leave as a
##' minimum gap (default = 0.05, only used by methods \code{spaced}
##' and \code{tufte})
##' @details The \code{method} option allows the y-position of the
##' elements to be calculated using different assumptions.  These are:
##' \itemize{ \item \code{tufte} Values in the first x-column are
##' sorted based on their numeric value.  Subsequent group lines are
##' then shifted to ensure that the lines for two adjacent groups
##' never cross.  Vertical positions in subsequent columns are only
##' meaningful relative to the first entry in that group.  \item
##' \code{spaced} The vertical position of each element is chosen to
##' ensure a minimum spacing between all elements and preserving the
##' rank order within columns.  Group lines can cross.  \item
##' \code{rank} The vertical position of each element represents its
##' rank within the column.  \item \code{none} The vertical position
##' of each element is based solely on its value }
##' @return a data frame with labelled columns, group, x, y, and ypos
build_slopegraph <- function(df, x, y, group, method="tufte", min.space=0.05) {

    ## First rename the columns for consistency
    ids <- match(c(x, y, group), names(df))
    df <- df[,ids]
    names(df) <- c("x", "y", "group")

    ## Expand grid to ensure every combination has a defined value
    tmp <- expand.grid(x=unique(df$x), group=unique(df$group))
    tmp <- merge(df, tmp, all.y=TRUE)
    df <- mutate(tmp, y=ifelse(is.na(y), 0, y))

    ## Then select and apply the appropriate method
    if (method=="spaced") {
        df <- spaced_sort(df, min.space=min.space)
        return(df)
    } else if (method=="none") {
        df <- mutate(df, ypos=y)
        return(df)
    } else if (method=="rank") {
        df <- ddply(df, .(x), summarize, x=x, y=y, group=group, ypos=rank(y))
        return(df)
    } else if (method=="tufte") {
        df <- tufte_sort(df, min.space=min.space)
        return(df)
    } else {
        template <- "Method '%s' currently unsupported."
        warning(sprintf(template, method))
    }
}

##' Spaced sort for slopegraphs
##'
##' Calculates the position of each element to ensure a minimum
##' space between adjacent entries within a column, while preserving
##' rank order.  Group lines can cross
##' @param df the raw data frame
##' @param min.space fraction of total data range to leave as a
##' minimum gap
##' @return a data frame with the ypos column added
spaced_sort <- function(df, min.space=0.05) {
    ## Define a minimum spacing (5% of full data range)
    min.space <- min.space*diff(range(df$y))

    ## Transform the data
    df <- ddply(df, .(x), calc_spaced_offset, min.space)
    return(df)
}

##' Calculates the vertical offset between successive data points
##'
##' @param df a data frame representing a single year of data
##' @param min.space the minimum spacing between y values
##' @return a data frame
calc_spaced_offset <- function(df, min.space) {

    ## Sort by value
    ord <- order(df$y, decreasing=T)
    ## Calculate the difference between adjacent values
    delta <- -1*diff(df$y[ord])
    ## Adjust to ensure that minimum space requirement is met
    offset <- (min.space - delta)
    offset <- replace(offset, offset<0, 0)
    ## Add a trailing zero for the lowest value
    offset <- c(offset, 0)
    ## Calculate the offset needed to be added to each point
    ## as a cumulative sum of previous values
    offset <- rev(cumsum(rev(offset)))
    ## Assemble and return the new data frame
    df.new <- data.frame(group=df$group[ord],
                         x=df$x[ord],
                         y=df$y[ord],
                         ypos=offset+df$y[ord])
  return(df.new)
}


##' Calculates slope graph positions based on Edward Tufte's layout
##'
##' @param df the raw data frame with named x, y, and group columns
##' @param min.space fraction of total data range to leave as a
##' minimum gap
##' @return a data frame with an additional calculate ypos column
tufte_sort <- function(df, min.space=0.05) {

    ## Cast into a matrix shape and arrange by first column
    require(reshape2)
    tmp <- dcast(df, group ~ x, value.var="y")
    ord <- order(tmp[,2])
    tmp <- tmp[ord,]

    min.space <- min.space*diff(range(tmp[,-1]))
    yshift <- numeric(nrow(tmp))
    ## Start at "bottom" row
    ## Repeat for rest of the rows until you hit the top
    for (i in 2:nrow(tmp)) {
        ## Shift subsequent row up by equal space so gap between
        ## two entries is >= minimum
        mat <- as.matrix(tmp[(i-1):i, -1])
        d.min <- min(diff(mat))
        yshift[i] <- ifelse(d.min < min.space, min.space - d.min, 0)
    }

    tmp <- cbind(tmp, yshift=cumsum(yshift))

    scale <- 1
    tmp <- melt(tmp, id=c("group", "yshift"), variable.name="x", value.name="y")
    ## Store these gaps in a separate variable so that they can be scaled ypos = a*yshift + y

    tmp <- transform(tmp, ypos=y + scale*yshift)
    return(tmp)

}


##' A theme for plotting slopegraphs
##'
##' @param base_size a numeric giving the base font size
##' @param base_family a string giving the base font family
##' @import grid
theme_slopegraph <- function (base_size = 12, base_family = "") {
    require(grid)
    theme(axis.line = element_blank(),
          axis.text = element_text(colour="black"),
          axis.text.x = element_text(size = rel(1), lineheight = 0.9,
              vjust = 1),
          axis.text.y = element_text(size=rel(0.8)),
          axis.ticks = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.length = unit(0, "lines"),
          axis.ticks.margin = unit(0, "lines"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.margin = unit(0.25, "lines"),
          strip.background = element_blank(),
          strip.text.x = element_text(size = rel(0.8)),
          strip.text.y = element_blank(),
          plot.background = element_blank(),
          plot.title = element_text(size = rel(1)),
          plot.margin = unit(c(1, 0.5, 0.5, 0.5), "lines"),
          complete=FALSE)
}


##' Plots a slopegraph
##'
##' @param df a data frame giving the data
##' @return a ggplot object
##' @import ggplot2
plot_slopegraph <- function(df, colour = "grey80") {
    ylabs <- subset(df, x==head(x,1))$group
    yvals <- subset(df, x==head(x,1))$ypos
    fontSize <- 2.5
    gg <- ggplot(df,aes(x=x,y=ypos)) +
        geom_line(aes(group=group),colour=colour) +
        geom_point(colour="white",size=8) +
        geom_text(aes(label=y),size=fontSize) +
        scale_y_continuous(name="", breaks=yvals, labels=ylabs)
    gg.form <- gg + theme_slopegraph()
    return(gg.form)
}


############################################################################################
###    Slopegraph from https://gist.github.com/leeper/7158678
############################################################################################


slopegraphOld <- function(
	df,
	xlim = c(.5,ncol(df)+.5),
	ylim = c(min(df)-diff(range(df))/100,max(df)+diff(range(df))/100),
	main = NULL,
	bty = 'n',
	yaxt = 'n',
	xaxt = 'n',
	xlab = '',
	ylab = '',
	add.exp = NULL, # an expression to add something between drawing the blank canvas and adding the plot content (i.e., behind the slopegraph)
	labels = names(df),
	labpos.left = 2,
	labpos.right = 4,
	col.lines = par('fg'),
	col.lab = par('fg'),
	col.num = par('fg'),
	col.xaxt = par('fg'),
	offset.x = .1,
	offset.lab = .1,
	cex.lab = 1,
	cex.num = 1,
	font.lab = 1,
	font.num = 1,
	lty = par("lty"),
	lwd = par("lwd"),
	mai = NULL,
	rescaleByColumn = T,
	...)
{
	## TODO
	## check col.lines, lty, lwd are the same length as df


    if(ncol(df) < 2)
        stop('`df` must have at least two columns')
    # draw margins
    if(is.null(mai))
        par(mai=c(1, 0, if(is.null(main)) 0 else 1, 0))
    else
        par(mai=mai)

    plot(NA, y=NULL, xlim=xlim, ylim=ylim, main=main,
         bty=bty, yaxt=yaxt, xaxt=xaxt, xlab=xlab, ylab=ylab, ...)
    # optional expression
    if(!is.null(add.exp))
        eval(add.exp)
    # x-axis
    axis(1, 1:ncol(df), labels = labels, col=col.xaxt, col.ticks=col.xaxt, lwd = 0, lwd.ticks = 0)

	if(rescaleByColumn) {
    	range.bycol <- sapply(df, function(c) diff(range(c, na.rm = T)))
		rescale <-  range(df[,which.max(range.bycol)])
		df.rescale <- sapply(df, rescale, rescale)
	} else {
		df.rescale <- df
	}
	rownames(df.rescale) <- rownames(df)
    # left-side labels
    l <- df.rescale[,1] # I MAY WANT TO BIN THESE SO THAT CLOSE VALUES DON'T OVERLAP
    leftlabs <- lapply(split(rownames(df.rescale),l), paste, collapse=', ')
    text(1-offset.lab,  as.numeric(names(leftlabs)),
         col = col.lab, leftlabs, pos = labpos.left, cex=cex.lab, font=font.lab)
    # right-side labels
    r <- df.rescale[,ncol(df)] # I MAY WANT TO BIN THESE SO THAT CLOSE VALUES DON'T OVERLAP
    rightlabs <- lapply(split(rownames(df.rescale),r), paste, collapse=',')
    text(ncol(df)+offset.lab, as.numeric(names(rightlabs)),
         col=col.lab, rightlabs, pos=labpos.right, cex=cex.lab, font=font.lab)
    # numeric value labels
    # deal with duplicate value labels (i.e., not double printing anything)
    df2 <- do.call(cbind,lapply(df, function(y) {y[duplicated(y)] <- ''; y}))

    # print them
    apply(cbind(df.rescale,df2),1, function(y)
  		  text(1:ncol(df), as.numeric(y[1:ncol(df)]), y[(ncol(df)+1):(2*ncol(df))],
              col = col.num, cex = cex.num, font = font.num)
    )
    # draw lines
    col.lines <- if(length(col.lines == 1)) rep(col.lines, length.out=nrow(df)) else col.lines
    lty <- if(length(lty == 1)) rep(lty, length.out = nrow(df)) else lty
    lwd <- if(length(lwd == 1)) rep(lwd, length.out = nrow(df)) else lwd

    for(i in 1:nrow(df.rescale)){
        mapply(function(x1,y1,x2,y2,...){
            ysloped <- (y2-y1)*offset.x
            segments(x1 + offset.x, if(y1==y2) y1 else (y1+ysloped),
                     x2 - offset.x, if(y1==y2) y2 else (y2-ysloped),
                     col=col.lines[i],
                     lty=lty[i],
                     lwd=lwd[i]
                    )},
               1:(length(df.rescale[i,])-1), # x1-positions
               df.rescale[i,][-length(df.rescale[i,])], # y1-positions
               2:(length(df.rescale[i,])), # x2-positions
               df.rescale[i,][-1] # y2-positions
               )
    }
}


# EXAMPLE
# test <- data.frame(x = 1:10, y = c(-5:-1, 11:15), z = 1:10, row.names = letters[1:10])
# slopegraphOld(test, rescaleByColumn = F, col.line='red', cex.lab = 0.6, cex.num = 0.6, offset.x = 0.027)
# slopegraphOld(test, rescaleByColumn = T)

