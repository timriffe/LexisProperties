
# Author: tim
###############################################################################
library(expm)
# function preamble. Graph creation is toggled off so
# I can source this with no other effects.

# utility functions snagged from YearsLost repo.
degrees2radians <- function(degrees){
	degrees * (pi / 180)
}

quarterArc <- function(x, y, radius = 1, fromDegrees = 180, ...){
	xx <- degrees2radians(seq(fromDegrees, fromDegrees + 90, by = .5))
	x <- cos(xx) * radius + x
	y <- sin(xx) * radius + y
	lines(x, y, ...)
}

curlyBrace1 <- function(xl, y, length = 5, radius1 = .5, radius2 = .25, top = TRUE, ...){  
	# i.e. the pointy part on top or on bottom?
	if (top){
		quarterArc(xl + radius1, y - radius1, radius = radius1, fromDegrees = 90, ...)
		quarterArc(xl + length - radius1, y - radius1 , radius = radius1, fromDegrees = 0, ...)
		quarterArc(xl + length / 2 - radius2, y + radius2, radius = radius2, fromDegrees = 270, ...)
		quarterArc(xl + length / 2 + radius2, y + radius2, radius = radius2, fromDegrees = 180, ...)
	} else {
		quarterArc(xl + radius1, y + radius1, radius = radius1, fromDegrees = 180, ...)
		quarterArc(xl + length - radius1, y + radius1 , radius = radius1, fromDegrees = 0 - 90, ...)
		quarterArc(xl + length / 2 - radius2, y - radius2, radius = radius2, fromDegrees = 270 + 90, ...)
		quarterArc(xl + length / 2 + radius2, y - radius2, radius = radius2, fromDegrees = 180 - 90, ...)        
	}
	segments(xl + radius1, y, xl + length / 2 - radius2, y, ...)
	segments(xl + length - radius1, y, xl + length / 2 + radius2, y, ...)   
}

# from JS, 27-3-2017
GenerateTransformationMatrix <- function (n) {
	stopifnot(n>=2)
	# BEGIN change Pancho 28.04.2017
	# n=n-1
	# GenSubmatrx <- function (i) cbind(-diag(i), 1, matrix(0, ncol = n-i, nrow = i))
	# do.call("rbind", lapply(1:n, "GenSubmatrx"))
	GenSubmatrx <- function (i) cbind(matrix(0, ncol = i-1, nrow = n-i), rep(-1, n-i), diag(n-i))
	do.call("rbind", lapply(1:(n-1), "GenSubmatrx"))
	# END change Pancho 28.04.2017
}
#n <- 4
#make.At(4)
DefaultDurationOrdering <- function(p){
	n       <- length(p)
	At      <- GenerateTransformationMatrix(n)
	# ugly but works
	tofromi <- t(apply(At, 1, function(x){
						c(which(x == -1),  which(x == 1))
					}))
	from    <- p[tofromi[,1]]
	to      <- p[tofromi[,2]]
	
	m 		<- length(to)
	#sum(cumsum(1:(n-1)))-n
	data.frame(d = 1:m,pfrom = tofromi[,1],pto=tofromi[,2],from = from, to = to, dur = to - from)
}


draw.timeline <- function(p,ylim=c(-3,0)){
	durs <- DefaultDurationOrdering(p)
	par(mai=c(.1,.1,.1,.1),xpd=TRUE)
	plot(NULL, xlim = range(p), ylim = ylim, axes = FALSE, xlab = "", ylab = "",asp=1)
	segments(min(p),0,max(p),0,lwd=2,col = gray(.5))
#points(p,rep(0,4),cex=2,pch=16)
	segments(p,-.08,p,.08,lwd=4,col = "red")
	for (i in 1:length(p)){
		text(p[i],.08,substitute(p[x], list(x = i)),pos=3)
	}
	for (i in 1:nrow(durs)){
		x <- durs$from[i]
		l <- durs$dur[i]
		y <- -i*.5
		curlyBrace1(xl = x, y = y, length = l, top = FALSE, radius1 = .1, radius2 = .08)
		# BEGIN Pancho 28.04.2017
		text(x+l/2,y-.05,substitute(d[x], list(x = paste0(durs$pfrom[i], ",", durs$pto[i]))), pos=1)
		# text(x+l/2,y-.05,substitute(d[x], list(x = i)),pos=1)
		# END Pancho 28.04.2017
	}
	
}

decide.verts <- function(p){
	n      <- length(p)
	radint <- seq((2*pi),0,length=n+1)[-(n+1)] + pi / 2
	list(x=cos(radint),y=sin(radint))
}

star.timeline <- function(p,lprop=.5){
	n     <- length(p)
	verts <- decide.verts(p)
	durs  <- DefaultDurationOrdering(p)
	par(xpd=TRUE)
	plot(NULL,type='n',xlim=c(-1,1),ylim=c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
	
	for (i in 1:nrow(durs)){
		fr <- durs$pfrom[i]
		to <- durs$pto[i]
		d  <- to - fr
		lpropi <- ifelse(d == 1 | d == (n - 1), .5, lprop)
		
		x1 <- verts$x[fr]
		x2 <- verts$x[to]
		y1 <- verts$y[fr]
		y2 <- verts$y[to]
		segments(x1,y1,x2,y2)
		
		lx <- x1*lpropi+x2*(1-lpropi)
		ly <- y1*lpropi+y2*(1-lpropi)
		# BEGIN Pancho 28.04.2017
		points(lx,ly,pch=22,cex=5,col="white",bg="white")
		text(lx,ly,substitute(d[x], list(x = paste0(fr, ",", to))))
		# points(lx,ly,pch=22,cex=3.5,col="white",bg="white")
		# text(lx,ly,substitute(d[x], list(x = i)))
		# END Pancho 28.04.2017
	}
	for (i in 1:length(p)){
		points(verts$x[i],verts$y[i],pch=21,cex=3.5,col="red",bg="white")
		text(verts$x[i],verts$y[i],substitute(p[x], list(x = i)))
	}
}

#p <- rep(1,4)

star.timeline.edges.only <- function(p,lprop=.5){
	n     <- length(p)
	n1    <- n + 1
	verts <- decide.verts(c(p,1))
	durs  <- DefaultDurationOrdering(p)
	par(xpd=TRUE)
	plot(NULL,type='n',xlim=c(-1,1),ylim=c(-1,1),asp=1,axes=FALSE, xlab="",ylab ="")
	
	for (i in 1:n){
		x1 <- verts$x[n1]
		x2 <- verts$x[i]
		y1 <- verts$y[n1]
		y2 <- verts$y[i]
		d  <- abs(i-n1)
		lpropi <- ifelse(d == 1 | d == n, .5, lprop)
		
		segments(x1,y1,x2,y2,col="red")
		
		lx <- x1*lpropi+x2*(1-lpropi)
		ly <- y1*lpropi+y2*(1-lpropi)
		points(lx,ly,pch=21,cex=3.5,col="red",bg="white",lwd=.5)
		text(lx,ly,substitute(p[x], list(x = i)))
	}
	
	for (i in 1:nrow(durs)){
		fr <- durs$pfrom[i]
		to <- durs$pto[i]
		d  <- to - fr
		lpropi <- ifelse(d == 1 | d == n, .5, lprop)
		
		
		x1 <- verts$x[fr]
		x2 <- verts$x[to]
		y1 <- verts$y[fr]
		y2 <- verts$y[to]
		segments(x1,y1,x2,y2)
		
		lx <- x1*lpropi+x2*(1-lpropi)
		ly <- y1*lpropi+y2*(1-lpropi)
		# BEGIN Pancho 28.04.2017
		points(lx, ly, pch=22, cex=5,col="white",bg="white")
		text(lx,ly,substitute(d[x], list(x = paste0(fr, ",", to))))
		# points(lx,ly,pch=22,cex=3.5,col="white",bg="white")
		# text(lx,ly,substitute(d[x], list(x = i)))
		# END Pancho 28.04.2017
	}
	
}


# make ugly generalized function
# A degree-n event-duration identity is identifiable from a set of n' events and m' durations
# if the graph formed by the set of n' and m' is a spanning tree of the edge-only graph of 
# said identity. That is, the graph is connected and includes each of the vertices. 

# This is the same as demanding that the minimal graph (durations as edges, events as nodes)
# is connected and includes at least one node explicitly (anchored).

# two dependencies copied from Timelines.R
GenerateTransformationMatrix <- function (n) {
	stopifnot(n>=2); n=n-1
	GenSubmatrx <- function (i) cbind(-diag(i), 1, matrix(0, ncol = n-i, nrow = i))
	do.call("rbind", lapply(1:n, "GenSubmatrx"))
}

# this produces all durations implied by a set of events, p
# including to-from vertices (not directed), as well as duration values.
# used as utility here and there.
DefaultDurationOrdering <- function(p){
	n       <- length(p)
	At      <- GenerateTransformationMatrix(n)
	# an inane way to get 
	tofromi <- t(apply(At, 1, function(x){
						c(which(x == -1),  which(x == 1))
					}))
	from    <- p[tofromi[,1]]
	to      <- p[tofromi[,2]]
	
	m 		<- length(to)
	#sum(cumsum(1:(n-1)))-n
	data.frame(d = 1:m,pfrom = tofromi[,1],pto=tofromi[,2],from = from, to = to, dur = to - from)
}

# this generates an adjacency matrix for the n+1 edge-only identity
np_adjacency <- function(n=4, edges=c("p1","p2","d1")){
	p       				<- rep(1, n)
	n1    					<- n + 1
	# get all durations
	durs  					<- DefaultDurationOrdering(p)
	dd 	  					<- paste0("d", durs$d)
	pp    					<- paste0("p", 1:n)
	# set of all durations and events, given with vertices in edge-only graph.
	edges.all  				<- data.frame(v1 = c(durs$pfrom, 1:n),
			v2 = c(durs$pto, rep(n1,n)))
	rownames(edges.all)   	<- c(dd,pp)
	
	edges.have            	<- edges.all[edges,]
	edges.have            	<- as.matrix(edges.have)
	# make an adjacency matrix
	adj 	              	<- matrix(0, ncol = n1, nrow = n1, dimnames = list(1:n1, 1:n1))   
	adj[edges.have] 		<- 1
	adj[edges.have[,c(2,1)]] <- 1
	
	# TR: 16-4-2017 OK, we only have 1s on diag 
	# for vertices that actually are touched...?
	vdiag <- rep(0,n1)
	vhave <- unique(c(edges.have))
	vdiag[vhave] <- 1
	adj                   	<- adj + diag(vdiag)
	
	# an extra object for orientation
	edges.all             	<- as.matrix(edges.all)
	A                     	<- adj * 0
	A[edges.all]          	<- rownames(edges.all)
	A[edges.all[,c(2,1)]] 	<- rownames(edges.all)
	diag(A) 				<- paste0("v",1:n1)
	
	list(adj=adj, edgeids = A)
}

# this function expands to the full set of identifiable edges
np_expand <- function(n=4, edges=c("p1","p2","d1")){
	require(expm)
	adjs 		<- np_adjacency(n, edges)
	adj 		<- adjs$adj
	ids 		<- adjs$edgeids
	
	# build out full coinnectivity by taking nth power
	# operator from expm package
	adji 		<- adj %^% n
	
	U 			<- upper.tri(adj)
	edges.identifiable <- adji > 0 & U
	
	list(edges.have = edges, 
			edges.expanded = ids[edges.identifiable],
			edges.all = ids[U])
} 

# this is the new function to determine if a set of events and durations (p_i and d_i) identifies an
# n-point identity. This works by defining the n+1-vertex graph consisting of all possible p_i and d_i,
# and then checking whether the specified set of edges connects the graph or not. This is done by 
# checking whether the symmetrix adjacency matrix is invertible or not. Returns TRUE or FALSE.

identifiable <- function(n=4,edges=c("p1","p2","d1")){
	adj <- np_adjacency(n, edges=edges)$adj
	# is this matrix invertible?
	determinant(adj,FALSE)$modulus != 0
}
identifiable2 <- function(n=4,edges=c("p1","p2","d1")){
	adj <- np_adjacency(n, edges=edges)$adj
	# is this matrix invertible?
	all(adj %^% n > 0)
}

#identifiable(n=4, edges = c("p1","p4","d1","d3"))
#identifiable2(n=4, edges = c("p1","p4","d1","d3"))

# --------------------
# test
#np_adjacency(n=3,  edges = c("p1","p2","d1"))

## APCTDL
#np_expand(n=3,  edges = c("p1","p2","d1"))   # APC
#np_expand( n = 3, edges = c("p1","p3","d1")) # APD
## a higher order identity, partial expansion
#np_expand( n = 4, edges = c("p1","p3","d1")) # APD
#
## APCTDL
#identifiable(3,  edges = c("p1","p2","d1"))  # APC
#identifiable(3, edges =  c("p1","p3","d1"))  # APD
#
#np_expand( n = 4, edges = c("p1","p3","d1")) # APD
#
#identifiable(n=5, edges =  c("p1","p3","d1"))  # APD

# would now like to generate the full set of possible spanning trees.
# got it.

# generate
generateSpanningTrees <- function(n=3){
	# generate all indices (all p will give it
	edges.fake  <- paste0("p",1:n)
	vs    		<- 1:(n+1)
	ids   		<- np_adjacency(n=n,  edges = edges.fake)$edgeids
	# n choose 2 combos of all edges yields all vertex pairs
	edges.all 	<- ids[t(combn(vs,2))]
	# then create all sets of n vertex pairs
	np1trees 	<- combn(edges.all,n)
	# given all sets of n vertex pairs (i.e. edges), which are identifiable?
	identified 	<- apply(np1trees,2,identifiable2,n=n)
	# select down
	as.list(as.data.frame(np1trees[, identified]))
}

# must be connected, use n edges, and touch all vertices.
#length(generateSpanningTrees(2)) # 3      # choose(ne(2),2)  3
#length(generateSpanningTrees(3)) # 16     # choose(ne(3),3)  20
#length(generateSpanningTrees(4)) # 125     # choose(ne(4),4)  210
#length(generateSpanningTrees(5)) # 1296    # choose(ne(5),5)  3003
#length(generateSpanningTrees(6)) # 15046  # choose(ne(6),6)  54264
#length(generateSpanningTrees(7)) # haven't run

#c(3,16,65,846,15046) / (2:6 + 1)

#ncombos <- function(n){
#	choose(ne(n),n)
#}
#n <-4
#(n+1)^(n-1)
# 2^1
# [1] 2
# > cat("Synch2377770871081\n");
#cumprod(1:6) - cumprod(0:5)
##           2, 3, 4,  5,  6
#n<- 2:6
#factorial(n)
#nspans <- c(3,16,65,846,15046)
#ncombos(2:6) # appears to approach 1/4
#plot(nspans / ncombos(2:6))
#ncombos(7) * .25 # my prediction
#ncombos(2:6) - nspans
#ne <- function(n) n+(n*(n-1)/2)
#
## it turns out Cayley's formula doesn't work for this...
#cayley <- function(n=3){
#	(n+1)^(n-1)
#}
#cayley(2:6) / nspans
#plot(c(3,16,65,846,15046),log='y')

# for n = 4, each edge appears 26 times in the solution set
# each edge pair appears 9 times
# each edge triad appears 4 times
# each edge quad appears once (n=4)
# in 65 solutions

# for n = 3, each dyad appears 3 times
# each edge appears 8 times
# in 16 solutions

#cayley(3)
#cayley(4)
#
## they're unique sets
#length(unique(unlist(lapply(n4,paste, collapse=""))))
# but are these connecting trees?

draw.tree <- function(n=4, edges, lprop = .5, x = 0, y = 0, add = FALSE, label = TRUE, col = NULL,...){
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
	edges.draw 	<- all.edges[all.edges$name %in% edges, ]
	
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
		segments(x1,y1,x2,y2,col=edges.draw$col[i],...)
		
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

get_verts <- function(n=4, edges){
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
	
	# select those specified
	edges.draw 	<- all.edges[all.edges$name %in% edges, ]
#	
#	x1 <- verts$x[edges.draw$pfrom]
#	x2 <- verts$x[edges.draw$pto]
#	y1 <- verts$y[edges.draw$pfrom] 
#	y2 <- verts$y[edges.draw$pto]
#	
#	coords1 <- cbind(x1,y1)
#	coords2 <- cbind(x2,y2)
	
	# actually to determine equality after rotation we only need
	# this ordering, we don't need to actually see the paths of segments
	edges.draw
}

is.rotated <- function(edges2,n=4, edges.ref ){
	
	nr.rotations(edges2,n=4, edges.ref) >= 0
}
nr.rotations <- function(edges2,n=4, edges.ref){
	
	if (all(sort(edges.ref) == sort(edges2))){
		return(0)
	}
	verts.ref <- as.matrix(get_verts(n, edges.ref)[,1:2])
	verts.ref <- verts.ref[order(verts.ref[,1],verts.ref[,2]), ]
	
	v.2       <- as.matrix(get_verts(n, edges2)[,1:2])
	v.2       <- v.2[order(v.2[,1],v.2[,2]), ]
	v.2.i     <- v.2
	same <- -1
	for (i in 1:n){
		v.2.i                  <- v.2.i + 1
		v.2.i[v.2.i > (n + 1)] <- 1
		v.2.i                  <- t(apply(v.2.i, 1, sort))
		v.2.i                  <- v.2.i[order( v.2.i[,1], v.2.i[,2]), ]
		if (all(v.2.i == verts.ref)){
			same <- i
			break
		}
	}
	return(same)
}
#rotate <- function(coords, deg = 60){
#	rad  <- deg / 180 * pi
#	rmat <- matrix(c(cos(rad),sin(rad),-sin(rad),cos(rad)),2)
#	
#	t(rmat %*% t(coords))
#}
#
## check to see if one graph is just a rotation of another
#is.rotated <- function(n=4, edges.ref, edges2){
#	
#	if (all(sort(edges.ref) == sort(edges2))){
#		return(TRUE)
#	}
#	verts.ref <- get_verts(n, edges.ref)
#	verts.2   <- get_verts(n, edges2)
#	
#	
#	# now we rotatate verts.2 by:
#	degs <- seq(72,72*4,72)
#	same <- FALSE
#	for (i in 1:length(degs)){
#		coords.2.r1        <- rotate(verts.2[[1]], degs[i])
#		coords.2.r2        <- rotate(verts.2[[2]], degs[i])
#		
#		res1 <- sum(abs(verts.ref[[1]] - coords.2.r1 )) +
#				sum(abs(verts.ref[[2]] - coords.2.r2 ))
#		res2 <- sum(abs(verts.ref[[1]] - coords.2.r2 )) +
#				sum(abs(verts.ref[[2]] - coords.2.r1 ))
#		if (res1 < .1 | res2 < .1){
#			same <- TRUE
#			break
#        }
#	}
#	return(same)
#}

#is.rotated(4, edges.ref=c("p1","p2","p3","p4"), edges2=c("p1","d4","d2","d1") )
#is.rotated(4, edges.ref=c("p1","p2","p3","p4"), edges2=c("p1","d4","d2","d3") )

make.graphs <- FALSE
if (make.graphs){
	from <- 0; to <- 6
	p2   <- seq(from=from,to=to,length=2)
	p3   <- seq(from=from,to=to,length=3)
	p4   <- seq(from=from,to=to,length=4)
	p5   <- seq(from=from,to=to,length=5)
	
# timelines
	draw.timeline(p2) 
	draw.timeline(p3) 
	draw.timeline(p4) 
	draw.timeline(p5) 
#draw.timeline(p5,c(-4,0)) 
	
# timeline graph
	star.timeline(p2,.5)
	star.timeline(p3,.5)
	star.timeline(p4,.4)
#star.timeline(p5,.5)
#star.timeline(runif(6),.4)
	
# temporal planes graph
	star.timeline.edges.only(p2)
	star.timeline.edges.only(p3,.4)
	star.timeline.edges.only(p4)
#star.timeline.edges.only(p5,.4)
	
# system command to crop pdf white space.
#pdfcrop filename.pdf filename.pdf
	
	outdir <-"/home/tim/git/APCT/APCT/Figures/TimeLinesAndGraphs"
	pdf(file.path(outdir,"linep2.pdf"),width=5,height=5)
	draw.timeline(p2) 
	dev.off()
	
	pdf(file.path(outdir,"linep3.pdf"),width=5,height=5)
	draw.timeline(p3) 
	dev.off()
	
	pdf(file.path(outdir,"linep4.pdf"),width=5,height=5)
	draw.timeline(p4) 
	dev.off()
	
	pdf(file.path(outdir,"linep5.pdf"),width=5,height=6)
	draw.timeline(p5,ylim=c(-4,0)) 
	dev.off()
	
	pdf(file.path(outdir,"starp2.pdf"),width=5,height=5)
	star.timeline(p2) 
	dev.off()
	
	pdf(file.path(outdir,"starp3.pdf"),width=5,height=5)
	star.timeline(p3) 
	dev.off()
	
	pdf(file.path(outdir,"starp4.pdf"),width=5,height=5)
	star.timeline(p4,.4) 
	dev.off()
	
	pdf(file.path(outdir,"edgep2.pdf"),width=5,height=5)
	star.timeline.edges.only(p2) 
	dev.off()
	
	pdf(file.path(outdir,"edgep3.pdf"),width=5,height=5)
	star.timeline.edges.only(p3,.4) 
	dev.off()
	
	pdf(file.path(outdir,"edgep4.pdf"),width=5,height=5)
	star.timeline.edges.only(p4) 
	dev.off()
	
	pdf(file.path(outdir,"edgep5.pdf"),width=7,height=7)
	star.timeline.edges.only(p5,.4) 
	dev.off()
#pdfcrop filename.pdf filename.pdf
	system(paste("pdfcrop",file.path(outdir,"linep2.pdf"),file.path(outdir,"linep2.pdf")))
	system(paste("pdfcrop",file.path(outdir,"linep3.pdf"),file.path(outdir,"linep3.pdf")))
	system(paste("pdfcrop",file.path(outdir,"linep4.pdf"),file.path(outdir,"linep4.pdf")))
	system(paste("pdfcrop",file.path(outdir,"linep5.pdf"),file.path(outdir,"linep5.pdf")))
	system(paste("pdfcrop",file.path(outdir,"starp2.pdf"),file.path(outdir,"starp2.pdf")))
	system(paste("pdfcrop",file.path(outdir,"starp3.pdf"),file.path(outdir,"starp3.pdf")))
	system(paste("pdfcrop",file.path(outdir,"starp4.pdf"),file.path(outdir,"starp4.pdf")))
	system(paste("pdfcrop",file.path(outdir,"edgep2.pdf"),file.path(outdir,"edgep2.pdf")))
	system(paste("pdfcrop",file.path(outdir,"edgep3.pdf"),file.path(outdir,"edgep3.pdf")))
	system(paste("pdfcrop",file.path(outdir,"edgep4.pdf"),file.path(outdir,"edgep4.pdf")))
	system(paste("pdfcrop",file.path(outdir,"edgep5.pdf"),file.path(outdir,"edgep5.pdf")))
	
} # end graph creation 





#p8 <- seq(0,10,length=8)
#star.timeline.edges.only(p8,.43) 