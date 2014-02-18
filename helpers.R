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
###    Slopegraph from https://gist.github.com/leeper/7158678
############################################################################################


slopegraph <- function(
	df,
	xlim = c(0.5,ncol(df)+0.5),
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
	collapse.label = "  ",
	lab.sep = 0.75,
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
    col.lines <- if(length(col.lines == 1)) rep(col.lines, length.out=nrow(df)) else col.lines
    lty 	  <- if(length(lty == 1)) rep(lty, length.out = nrow(df)) else lty
    lwd       <- if(length(lwd == 1)) rep(lwd, length.out = nrow(df)) else lwd
	col.lab   <- if(length(col.lab == 1)) rep(col.lab, length.out=nrow(df)) else col.lab

    if(ncol(df) < 2)
        stop('`df` must have at least two columns')
    # draw margins
    if(is.null(mai))
        par(mai=c(1, 0, 1, 0))
    else
        par(mai=mai)

    plot(NA, y=NULL, xlim=xlim, ylim=ylim, main=main,
         bty=bty, yaxt=yaxt, xaxt=xaxt, xlab=xlab, ylab=ylab, ...)
    # optional expression
    if(!is.null(add.exp))
        eval(add.exp)
    # x-axis
    axis(3, 1:ncol(df), labels = labels, col=col.xaxt, col.ticks=col.xaxt, lwd = 0, lwd.ticks = 0, cex.axis = cex.lab)

	if(rescaleByColumn) {
    	range.bycol <- sapply(df, function(c) diff(range(c, na.rm = T)))
		rescale <-  range(df[,which.max(range.bycol)])
		df.rescale <- sapply(df, rescale, rescale)
	} else {
		df.rescale <- df
	}
	rownames(df.rescale) <- rownames(df)

    l <- df.rescale[,1] # I MAY WANT TO BIN THESE SO THAT CLOSE VALUES DON'T OVERLAP
    leftlabs <- lapply(split(rownames(df.rescale),l), paste, collapse.label, sep="")
    lab.dup <- sapply(leftlabs, length) > 1
    # print text for no labels on the same row
	text(1 - offset.lab, as.numeric(names(leftlabs)[!lab.dup]),
         col=col.lab[match(as.numeric(names(leftlabs)[!lab.dup]), l)], leftlabs[!lab.dup], pos=labpos.left, cex=cex.lab, font=font.lab)
	# print multiple labels on the same row

	sapply(as.numeric(names(lab.dup)[lab.dup]), function(pos) {
		idx <- l == pos
		text(c(1 - offset.lab, 1:(sum(idx)-1) * -lab.sep + (1 - offset.lab)),
			pos, col=col.lab[idx], unlist(leftlabs[as.character(pos)]), pos=labpos.left,
			cex=cex.lab, font=font.lab)
	})

    # right-side labels
    r <- df.rescale[,ncol(df)] # I MAY WANT TO BIN THESE SO THAT CLOSE VALUES DON'T OVERLAP
    #rightlabs <- lapply(split(rownames(df.rescale),r), paste, collapse=collapse.label)
    rightlabs <- lapply(split(rownames(df.rescale),r), paste, collapse.label, sep="")
    lab.dup <- sapply(rightlabs, length) > 1

    # print text for no labels on the same row
	text(ncol(df)+offset.lab, as.numeric(names(rightlabs)[!lab.dup]),
         col=col.lab[match(as.numeric(names(rightlabs)[!lab.dup]), r)], rightlabs[!lab.dup], pos=labpos.right, cex=cex.lab, font=font.lab)
	# print multiple labels on the same row

	sapply(as.numeric(names(lab.dup)[lab.dup]), function(pos) {
		idx <- r == pos
		text(c(ncol(df) + offset.lab, 1:(sum(idx)-1) * lab.sep + (ncol(df) + offset.lab)),
			pos, col=col.lab[idx], unlist(rightlabs[as.character(pos)]), pos=labpos.right,
			cex=cex.lab, font=font.lab)
	})


    # numeric value labels
    # deal with duplicate value labels (i.e., not double printing anything)
    df2 <- do.call(cbind,lapply(df, function(y) {y[duplicated(y)] <- ''; y}))

    # print them
    apply(cbind(df.rescale,df2),1, function(y)
  		  text(1:ncol(df), as.numeric(y[1:ncol(df)]), y[(ncol(df) + 1):(2*ncol(df))],
              col = col.num, cex = cex.num, font = font.num)
    )
    # draw lines
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
# slopegraph(test, rescaleByColumn = F, col.line='red', cex.lab = 0.6, cex.num = 0.6, offset.x = 0.05, xlim = c(-0.5, 3.5))
# slopegraph(test, rescaleByColumn = T)
# test <- data.frame(x = 1:10, y = c(-5:-1, c(11,11,11, 15,15)), z = c(2,2,2, 4:7, 9,9,9), row.names = letters[1:10])
# test.col <- rep(c("green", "red", "blue", "green", "blue"), 2)
# slopegraph(test, rescaleByColumn = F, col.line=test.col, col.lab=test.col, , cex.lab = 0.6, cex.num = 0.6, offset.x = 0.05, lab.sep = 0.2)






############################################################################################
###    http://flowingdata.com/2013/06/03/how-to-make-slopegraphs-in-r/
############################################################################################

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


## EXAMPLE
# pctgdp <- data.frame(country = factor(c("Sweden", "Netherlands", "Norway", "Britain", "France", "Germany", "Belgium",
# 	"Canada", "Finland", "Italy","United States","Greece", "Switzerland", "Spain", "Japan")),
# 	pct1970 = c(46.9,44,43.5,40.7,39,37.5,35.2,35.2,34.9,30.4,30.3,26.8,26.5,22.5,20.7),
# 	pct1979 = c(57.4,55.8,52.2,39,43.4,42.9,43.2,35.8,38.2,35.7,32.5,30.6,33.2,27.1,26.6)
# )
# slopegraph2(pctgdp$pct1970, pctgdp$pct1979, pctgdp$country)


