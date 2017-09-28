
# Author: tim
###############################################################################

source("/home/tim/git/APCT/APCT/R/Timelines.R")

# -----------------------------------
#
setwd("/home/tim/git/LexisProperties/LexisProperties/Oxford")

source("R/TimeLines.R")
source("R/Functions.R")

args(darken.to)


pal10 <- c("#ffbaa8","#61df6a","#6a41a6","#c5d643",
		"#c1b5ff","#b69600","#00b2c1","#c70034","#98d9c7","#d36500")
pal10eq <- sapply(pal10,darken.to,.3)

n4 <- lapply(generateSpanningTrees(4), sort)
plot.order <- matrix(1:130,ncol = 13)
xc         <- (col(plot.order) - 1) * 2.2 + 1
yc         <- (row(plot.order)-1) * 2.2 + 1
yc         <- abs(yc-max(yc)) + 1

pdf("Figures/n4MST.pdf",height=10,width=13)
par(xpd=TRUE,bg="white", xaxs="i",yaxs="i",mai=c(.5,.5,.5,.5))
plot(NULL,type='n',xlim=range(xc)+c(-1,1),ylim=range(yc)+c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
for (i in 1:length(n4)){
	draw.tree(4,n4[[i]],label=FALSE, add = TRUE,x=xc[i], y = yc[i], col = pal10eq, lwd = 2)
}
dev.off()

# 125
# 1296
# (4+1)^(4-1)
# (5+1)^(5-1)
n3 <- lapply(generateSpanningTrees(3), sort)

plot.order <- matrix(1:16,ncol = 4)
xc         <- (col(plot.order) - 1) * 2.5 + 1
yc         <- (row(plot.order)-1) * 2.5 + 1
yc         <- abs(yc-max(yc)) + 1

colsin <- sapply(c("A","P","C","T","D","L"),AssignColour)


#APCTDL
#p1p2p3d1d2d3
#CPDALT
ord <- c(3,2,5,1,6,4)
pdf("Figures/n3MST.pdf",height=10,width=10)
par(xpd=TRUE,bg="white", xaxs="i",yaxs="i",mai=c(.5,.5,.5,.5))
plot(NULL,type='n',xlim=range(xc)+c(-1,1),ylim=range(yc)+c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
for (i in 1:length(n3)){
	draw.tree(3,n3[[i]],label=FALSE, lprop=.3,add = TRUE,cex=.5,x=xc[i], y = yc[i], col = colsin[ord], lwd =  3)
}
dev.off()

draw.tree(3,c("p1","p2","p3","d1","d2","d3"),label=TRUE, lprop=.3,col = colsin[ord], lwd = 5)

# should we stick to the tetrahedral MSTs?


