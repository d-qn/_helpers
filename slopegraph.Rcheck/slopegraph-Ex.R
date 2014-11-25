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
### Title: Slopegraph charts
### Aliases: slopegraph

### ** Examples

test <- data.frame(x = 1:10, y = c(-5:-1, 11:15), z = 1:10, row.names = letters[1:10])

slopegraph(test, rescaleByColumn = F, col.line='red', cex.lab = 0.6, cex.num = 0.6, offset.x = 0.05, xlim = c(-0.5, 3.5))
slopegraph(test, rescaleByColumn = T)

test <- data.frame(x = 1:10, y = c(-5:-1, c(11,11,11, 15,15)), z = c(2,2,2, 4:7, 9,9,9), row.names = letters[1:10])
test.col <- rep(c("green", "red", "blue", "green", "blue"), 2)
slopegraph(test, rescaleByColumn = F, col.line=test.col, col.lab=test.col, , cex.lab = 0.6, cex.num = 0.6, offset.x = 0.05, lab.sep = 0.2)



cleanEx()
nameEx("slopegraph2")
### * slopegraph2

flush(stderr()); flush(stdout())

### Name: slopegraph2
### Title: Slopegraph charts
### Aliases: slopegraph2

### ** Examples

pctgdp <- data.frame(country = factor(c("Sweden", "Netherlands", "Norway", "Britain", "France", "Germany", "Belgium",
  "Canada", "Finland", "Italy","United States","Greece", "Switzerland", "Spain", "Japan")),
	pct1970 = c(46.9,44,43.5,40.7,39,37.5,35.2,35.2,34.9,30.4,30.3,26.8,26.5,22.5,20.7),
	pct1979 = c(57.4,55.8,52.2,39,43,43,43,35.8,38.2,35.7,32.5,30.6,33.2,27.1,26.6)
)
slopegraph2(pctgdp$pct1970, pctgdp$pct1979, pctgdp$country)



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
