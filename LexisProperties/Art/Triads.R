
# Author: tim
###############################################################################
setwd("/home/tim/git/LexisProperties/LexisProperties/Art")

source("/home/tim/git/LexisProperties/LexisProperties/OxfordR/Timelines.R")
source("/home/tim/git/LexisProperties/LexisProperties/OxfordR/Functions.R")

# n5
n5 <- lapply(generateSpanningTrees(5), sort)
sqrt(length(n5))
plot.order <- matrix(1:1296,ncol = 36)
xc         <- (col(plot.order) - 1) * 2.5 + 1
yc         <- (row(plot.order)-1) * 2.5 + 1
yc         <- abs(yc-max(yc)) + 1

pdf("Figures/n5MST.pdf",height=10,width=10)
par(xpd=TRUE,bg="white", xaxs="i",yaxs="i",mai=c(.5,.5,.5,.5))
plot(NULL,type='n',xlim=range(xc)+c(-1,1),ylim=range(yc)+c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
for (i in 1:length(n5)){
	draw.tree(5,n5[[i]],label=FALSE, lprop=.3,add = TRUE,cex=.5,x=xc[i], y = yc[i], col = gray(.3), lwd =  1)
}
dev.off()

n <- 5
n1<- n+1
p <- rep(1,n)
durs <- DefaultDurationOrdering(p)
durs$name 	<- paste0("d", durs$d)
durs$from 	<- NULL
durs$to 	<- NULL
durs$d 		<- NULL
durs$dur 	<- NULL

# make same for period measures, colnames must match
pp 			<- data.frame(pfrom = 1:n, 
		pto = rep(n1,n), 
		name = paste0("p",1:n))
all.edges 	<- rbind(pp,durs)

combos2     <- combn(1:nrow(all.edges),2)
verts 	<- decide.verts(c(p,1))

# actually this is easier: can just to combinatorics on verts...
v3     <- combn(1:6,3) # duh

# say p3 is period? : red c9313eff
# say p1,2,4,5 purple b62dd2ff
# any d touching 4th vertex is orange (d2,3,6,9) ea9c27ff
# other d's yellow. (d1,4,5,7,8,10) eee540ff
cols <- rep(NA, 15)
names(cols) <- c("p1", "p2", "p3", "p4", "p5", "d1", "d2", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10")
cols["p3"] <- "#c9313eff"
cols[c("p1","p2","p4","p5")] <- "#b62dd2ff"
cols[c("d2","d3","d6","d9")] <- "#ea9c27ff"
cols[c("d1","d4","d5","d7","d8","d10")] <- "#eee540ff"
all.edges$col <- cols

# 4x5
plot.order <- matrix(1:20,ncol = 5)
xc         <- (col(plot.order) - 1) * 2.5 + 1
yc         <- (row(plot.order)-1) * 2.5 + 1
yc         <- abs(yc-max(yc)) + 1
verts$n <- 1:n1

pdf("Triadsn5.pdf",width=12,height=12)
par(xpd=TRUE,bg="white", xaxs="i",yaxs="i",mai=c(.5,.5,.5,.5))
plot(NULL,type='n',xlim=range(xc)+c(-1,1),ylim=range(yc)+c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
for (i in 1:ncol(v3)){
	draw.tree(5,all.edges$name,label=FALSE, col = gray(.8), lwd =  .3,add=TRUE,x=xc[i], y = yc[i])
	
	ni      <- v3[,i]
	edges.i <- all.edges[all.edges$pfrom %in% ni & all.edges$pto %in% ni,]
	names.i <- edges.i$name
	pcol <- ifelse("p3" %in% names.i, "#f9311b50",
			ifelse(any(c("p1","p2","p4","p5")%in%names.i),"#b62dd230",
					ifelse(any(c("d2","d3","d6","d9")%in%names.i),"#ea9c2750","#eee54050")))
	polygon(x=verts$x[ni]+xc[i],
			y=verts$y[ni]+yc[i],
			col = pcol,
			border = NA)
	draw.tree(5,edges.i$name,label=FALSE, col = all.edges$col, lwd =  2,add=TRUE,x=xc[i], y = yc[i])
	
}
dev.off()
# n5 labelled
draw.tree(5,all.edges$name,label=TRUE, lprop=.3,cex=.5, col = all.edges$col, lwd =  1)


all.edges