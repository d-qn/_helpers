#' Slopegraph charts
#' 
#' Plot a minimalistic Tuffte style slopegraph chart
#' 
#' @param df a data.frame that will be plotted as slopegraph
#' @param rescaleByColumn, a logical of length 1, shall each series of observations be normalized?
#' @param xlim, ylim vectors of length 2 which sets chart area based on the data unit
#' @param main a character vector used as the plot title
#' @param yaxt, xaxt, xlab, ylab, passed on to \code{\link{par}}
#' @param labels a character string, the labels to be displayed for each slope lines
#' @param labpos.left, labpos.right a numeric of length 1, the position on the x-axis for the labels
#' @param collapse.label a character, for colliding labels at the same y the spacer to use
#' @param lab.sep a numeric of length 1, for colliding labels the space in the x-axis unit
#' @param col.lines, col.lab vectors of length 1 or of length df, specifying the line and label colors
#' @param col.num, col.xaxt vectors of length 1 specifying the numerical value and x-axis colors
#' @param offset.x, offset.lab vectors of length 1 specifying in the x-axis unit the line to numerical value and labels offsets.
#' @param cex.lab, cex.num  vectors of length 1 specifying the cex of axis and nuermical values, see \code{cex.axis} and \code{cex} in \code{\link{par}} 
#' @param font.lab, a vector of length 1 specifying the font weight, as defined in \code{font} in \code{\link{par}} 
#' @param lty, lwd vectors of length 1 or of length df specifiyng the line type and width, see \code{\link{par}} 
#' @param mai, plot margin as defined in \code{\link{par}} 
#' @importFrom scales rescale
#' @references https://gist.github.com/leeper/7158678
#' @export
#' @examples
#' test <- data.frame(x = 1:10, y = c(-5:-1, 11:15), z = 1:10, row.names = letters[1:10])
#' 
#' slopegraph(test, rescaleByColumn = F, col.line='red', cex.lab = 0.6, cex.num = 0.6, offset.x = 0.05, xlim = c(-0.5, 3.5))
#' slopegraph(test, rescaleByColumn = T)
#' 
#' test <- data.frame(x = 1:10, y = c(-5:-1, c(11,11,11, 15,15)), z = c(2,2,2, 4:7, 9,9,9), row.names = letters[1:10])
#' test.col <- rep(c("green", "red", "blue", "green", "blue"), 2)
#' slopegraph(test, rescaleByColumn = F, col.line=test.col, col.lab=test.col, , cex.lab = 0.6, cex.num = 0.6, offset.x = 0.05, lab.sep = 0.2)
#' 

slopegraph <- function(
  df,
  rescaleByColumn = T,
  xlim = c(0.5,ncol(df)+0.5),
  ylim = c(min(df, na.rm = T)-diff(range(df, na.rm = T))/100,max(df, na.rm = T)+diff(range(df, na.rm = T))/100),
  main = NULL,
  yaxt = 'n',
  xaxt = 'n',
  xlab = '',
  ylab = '',
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
       bty='n', yaxt=yaxt, xaxt=xaxt, xlab=xlab, ylab=ylab, ...)

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

