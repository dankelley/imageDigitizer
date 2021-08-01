pchs <- 0:25
cex <- 3
lwd <- 2
width <- 32
height <- 32
scale <- 1.0
for (pch in pchs) {
    png(sprintf("pch_%02d.png", pch), width=width, height=height)
    par(mar=rep(0,4))
    plot(scale*c(-1,1), scale*c(-1,1), xlab="", ylab="", type="n", axes=FALSE)
    points(0, 0, pch=pch, cex=cex, lwd=lwd)
    dev.off()
    cat(sprintf("\"<img src='/pch_%02d.png' alt='%d'>\",\n", pch, pch))
}

