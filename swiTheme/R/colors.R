
##' swissinfo.ch standard color palette
##'
##' @rdname colors_swi
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
##' @rdname colors_swi
##' @export
##' @examples
##' pie(rep(1,length(swi_rpal)), col=swi_rpal)
swi_rpal <- swi_pal[c(9, 6, 3, 8, 1, 4, 7, 11, 16, 5, 14, 13, 15, 2, 10, 18, 20, 19, 12, 17, 22, 21)]

## swissinfo diverging color palette with 10 levels
##' @rdname colors_swi
##' @export
##' @examples
##' pie(rep(1,length(swi_dpal)), col=swi_dpal)
swi_dpal <- c("#476666", "#537373", "#5F7F7F", "#6C8C8C", "#789A99", "#DBBF8B", "#CCB27E", "#BEA472", "#B09766", "#A28959")
