pchs <- 0:25
cex <- 2.2
lwd <- 1.4
width <- 22
height <- width
scale <- 1.0
for (pch in pchs) {
    png(sprintf("pch_%02d.png", pch), width=width, height=height)
    par(mar=rep(0,4))
    plot(scale*c(-1,1), scale*c(-1,1), asp=1, xlab="", ylab="", type="n", axes=FALSE)
    points(0, 0, pch=pch, cex=cex, lwd=lwd)
    dev.off()
    cat(sprintf("\"<img src='/pch_%02d.png' alt='%d'>\",\n", pch, pch))
}

