
# Author: tim
################################################################5###############
n <- 12
set.seed(1)

pdf("/home/tim/git/LexisProperties/LexisProperties/Oxford/Figures/LexisSimplePlain.pdf")
par(mai = c(.3,.3,.3,.3),xaxs= "i", yaxs = "i")
plot(NULL,type = "n",xlim= c(0,10),ylim = c(0,10),asp = 1,axes=FALSE,xlab = "",ylab = "" )
segments(1,1,1,7)
segments(1,1,8,1)

l <- runif(n) * 6
b <- runif(n) * 2 + 1

segments(b,1,b+l,l+1)
points(b+l,l+1,pch=16)
dev.off()




