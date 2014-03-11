pkgname <- "slopegraph"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('slopegraph')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("slopegraph")
### * slopegraph

flush(stderr()); flush(stdout())

### Name: slopegraph
### Title: Slopegraph chart
### Aliases: slopegraph

### ** Examples

test <- data.frame(x = 1:10, y = c(-5:-1, 11:15), z = 1:10, row.names = letters[1:10])

slopegraph(test, rescaleByColumn = F, col.line='red', cex.lab = 0.6, cex.num = 0.6, offset.x = 0.05, xlim = c(-0.5, 3.5))
slopegraph(test, rescaleByColumn = T)

test <- data.frame(x = 1:10, y = c(-5:-1, c(11,11,11, 15,15)), z = c(2,2,2, 4:7, 9,9,9), row.names = letters[1:10])
test.col <- rep(c("green", "red", "blue", "green", "blue"), 2)
slopegraph(test, rescaleByColumn = F, col.line=test.col, col.lab=test.col, , cex.lab = 0.6, cex.num = 0.6, offset.x = 0.05, lab.sep = 0.2)



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
