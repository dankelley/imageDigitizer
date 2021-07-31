n <- 20
cex <- 2
cat("choiceValues=1:20\n")
cat("choiceNames=c(\n")
for (i in seq_len(n)) {
    png(sprintf("pch_%02d.png", i), width=16, height=16)
    par(mar=c(0,0,0,0))
    plot(c(-1,1), c(-1,1), xlab="", ylab="", type="n", axes=FALSE)
    points(0.05, 0.05, pch=i, cex=cex)
    dev.off()
    cat(sprintf("\"<img src='pch_%02d.png' alt='%d'>\",\n", i, i))
}
cat(")\n")

