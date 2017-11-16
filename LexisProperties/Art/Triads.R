
# Author: tim
###############################################################################

this.comp <- system("hostname",intern=TRUE)
if (this.comp == "tim-ThinkPad-L440"){
	setwd("/home/tim/git/LexisProperties/LexisProperties")
	source("Oxford/R/Timelines.R")
	source("Oxford/R/Functions.R")
}
if (this.comp == "PC-403478"){
	setwd("U:\\git\\LexisProperties\\LexisProperties")
	source("Oxford\\R\\Timelines.R")
	source("Oxford\\R\\Functions.R")
}


library(gridBase)
library(grid)
library(devtools)
#install_github("pmur002/vwline/pkg")
library(vwline)




brushed.segment <- function(x0,y0,x1,y1,w,n,tremble=lwd/20,lwd=1,col="black",minlwd = lwd / 3,...){
	opar <- par()
	
	if (missing(w)){
		if (missing(n)){
			n <- 10
		}
		# how many points in x?
		ptsx <- 72.27 * opar$pin[1] 
		xr   <- diff(opar$xaxp[1:2])
		cex1 <- lwd * opar$ps / (ptsx / xr) 
		#w    <- (runif(n)-.5) * tremble + cex1
		noise <- (runif(n)-.5) * tremble
		w    <- sort(runif(n) * tremble)/2 + noise + cex1
	}
	
	w[w < minlwd] <- minlwd
	l    <- length(w)
	x    <- seq(from=x0, to=x1, length = l)
	y    <- seq(from=y0, to=y1, length = l)
	
	# prelims
	vps  <- baseViewports()
	pushViewport(vps$inner, vps$figure, vps$plot)
	pushViewport(viewport(x=unit(opar$xaxp[3], "native"),
					y=unit(opar$yaxp[3], "native"),
					xscale = opar$xaxp[1:2],yscale=opar$yaxp[1:2],
					just="centre"))
	
	# draw the segment
	grid.vwline(x, y, w, default.units = "native",gp=gpar(col="transparent",fill=col),open=TRUE)
	#grid.vwXspline(x, y, w,  default.units = "native",gp=gpar(col=col,fill=col),shape=1)
# return to base
	popViewport(3)
}
#pdf("test.pdf")
#par(mai=c(0,0,0,0),xaxs="i",yaxs="i")
#plot(NULL, xlim = c(0,10),ylim=c(0,10),type="n",asp=1)
#for (i in 1:5){
#	brushed.segment(1,1,9,9,n=255,lwd=.2,tremble=.05,col="#22222250") 
#}
#dev.off()

sketched.brushed.segment <- function(x0,y0,x1,y1,
		njiggle=100, 
		nstrokes = 5,
		placement.tremble = 1,
		length.tremble = .1,
		edge.tremble=lwd/20,
		lwd=1,
		lwd.scatter=1,
		col="#00000050",
		...){
	# actual number of strokes may vary
	nstrokes <- rpois(1,lambda = nstrokes)
	if (nstrokes == 0){
		nstrokes <- 2
	}
	left.in  <- rnorm(nstrokes,mean = 0,sd = length.tremble)
	right.in <- rnorm(nstrokes,mean = 0,sd = length.tremble)
	# get vectors
	x        <- jitter(c(rep(x0, nstrokes), rep(x1, nstrokes)), factor = placement.tremble) 
	y        <- jitter(c(rep(y0, nstrokes), rep(y1, nstrokes)), factor = placement.tremble) 
	x[x < 0] <- 0
	y[y < 0] <- 0
	
	# re-separete left and right
    x0.      <- x[1:nstrokes]
    x1.      <- x[(nstrokes+1):(nstrokes*2)]
	y0.      <- y[1:nstrokes]
	y1.      <- y[(nstrokes+1):(nstrokes*2)]

	# now do the in-out step
	dx <- x1. - x0.
	dy <- y1. - y0.
	dxl <- dx * (1 + left.in)
	dxr <- dx * (1 + right.in)
	dyl <- dy * (1 + left.in)
	dyr <- dy * (1 + right.in)
	# now readjust:
	x0 <- x1. - dxl
	x1 <- x0. + dxr
	y0 <- y1. - dyl
	y1 <- y0. + dyr
	
	lwd.     <- jitter(rep(lwd, nstrokes), factor = lwd.scatter)
	
	for (i in 1:nstrokes){
		brushed.segment(
				x0[i],
				y0[i],
				x1[i],
				y1[i],
				n=njiggle,
				lwd=lwd.[i],
				tremble=edge.tremble,
				col=col) 
	}
}

draw.tree2 <- function(n=4, edges, lprop = .5, x = 0, 
		y = 0, add = FALSE, label = TRUE, col = NULL,alpha=1,...){
	p 			<- rep(1,n)
	n1    		<- n + 1
	# get coords for the n+1 vertices
	verts 		<- decide.verts(c(p,1))
	# get d1...dm
	durs  		<- DefaultDurationOrdering(p)
	# need comparable name, for selecting later
	durs$name 	<- paste0("d", durs$d)
	# remove unneeded columns
	durs$from 	<- NULL
	durs$to 	<- NULL
	durs$d 		<- NULL
	durs$dur 	<- NULL
	# make same for period measures, colnames must match
	pp 			<- data.frame(pfrom = 1:n, 
			pto = rep(n1,n), 
			name = paste0("p",1:n))
	# join event and duration edges
	all.edges 	<- rbind(pp,durs)
	
	if (is.null(col)){
		col <- rep("black",nrow(all.edges))
	} 
	all.edges$col <- col
	# select those specified
	if (!missing(edges)){
		edges.draw 	<- all.edges[all.edges$name %in% edges, ]
	} else {
		edges.draw 	<- all.edges
	}
	
	if (!add){
		# create empty device
		par(xpd=TRUE)
		plot(NULL,type='n',xlim=c(-1,1),ylim=c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
	}
	for (i in 1:nrow(edges.draw)){
		fr <- edges.draw$pfrom[i]
		to <- edges.draw$pto[i]
		d  <- to - fr
		lpropi <- ifelse(d == 1 | d == n, .5, lprop)
		
		x1 <- verts$x[fr] + x
		x2 <- verts$x[to] + x
		y1 <- verts$y[fr] + y
		y2 <- verts$y[to] + y
		
		#segments(x1,y1,x2,y2,col=edges.draw$col[i],...)
		sketched.brushed.segment(x0=x1,y0=y1,x1=x2,y1=y2,
				col=adjustcolor(edges.draw$col[i],alpha.f=alpha),...) 
		if (label){
			lx <- x1*lpropi+x2*(1-lpropi)
			ly <- y1*lpropi+y2*(1-lpropi)
			points(lx,ly,pch=22,cex=3.5,col=par("bg"),bg=par("bg"))
			text(lx,ly,edges.draw$name[i],col=gray(.5))
		}
	}
	out <- list(edges.draw, verts)
	invisible(out)
}

pdf("test.pdf")
par(xpd=TRUE,bg="white", xaxs="i",yaxs="i",mai=c(0,0,0,0))
plot(NULL,type='n',xlim=c(-1,1),ylim=c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
draw.tree2(n=5,col = "#222222",label=FALSE, add = TRUE,
		lwd.scatter = 25,
		length.tremble = .05,
		lwd=1,
		edge.tremble=.1,
		nstrokes=3,
		njiggles=100,
		placement.tremble = 1
		)
dev.off()
draw.tree2(n=5,c("p1","p2","p3","p4","d1","d2"),col = "#222222",label=FALSE,
		placement.tremble = .04, 
		lwd.scatter = 25,
		length.tremble = .05)
#x0=1;y0=1;x1=9;y1=9;lwd=.2
#pdf("test.pdf")
#par(mai=c(0,0,0,0),xaxs="i",yaxs="i")
#plot(NULL, xlim = c(0,10),ylim=c(0,10),type="n",asp=1)
#sketched.brushed.segment(x0=1,y0=1,x1=9,y1=9,
#		nstrokes=3,
#		njiggle=200,
#		lwd=.15,
#		edge.tremble=.05,
#		col="#222222AA",
#		placement.tremble = .04, 
#		lwd.scatter = 25,
#		length.tremble = .05) 
#dev.off()



# n5
n5 <- lapply(generateSpanningTrees(5), sort)
sqrt(length(n5))
plot.order <- matrix(1:1296,ncol = 36)
xc         <- (col(plot.order) - 1) * 2.5 + 1
yc         <- (row(plot.order)-1) * 2.5 + 1
yc         <- abs(yc-max(yc)) + 1

pdf(file.path("Art","n5MST.pdf"),height=10,width=10)
par(xpd=TRUE,bg="white", xaxs="i",yaxs="i",mai=c(.5,.5,.5,.5))
plot(NULL,type='n',xlim=range(xc)+c(-1,1),ylim=range(yc)+c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
for (i in 1:length(n5)){
	draw.tree(5,n5[[i]],label=FALSE, lprop=.3,add = TRUE,cex=.5,x=xc[i], y = yc[i], col = gray(.3), lwd =  1)
	#draw.tree2(5,n5[[i]],label=FALSE, lprop=.3,add = TRUE,cex=.5,x=xc[i], y = yc[i], col = gray(.3), lwd =  1)
	
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