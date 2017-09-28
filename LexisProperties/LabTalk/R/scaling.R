

n <- 2:11

ids   <- choose(n+1,3)

# two events
a2f1  <- choose(n-1,2)
a1ps1 <- n-1

# 3 durations
s2f1 <- choose(n-1,3)
f3   <- choose(n-1,2)

# decompose
a2f1 + a1ps1 + s2f1 + f3

idcomp <- cbind(a1ps1, a2f1, f3, s2f1)
rownames(idcomp) <- n
plot(n,ids,type='S')

pdf("/home/tim/git/LexisProperties/LexisProperties/LabTalk/Figures/id3comp.pdf")
barplot(t(idcomp),space=0,width=1, col = c("#800080","#7f2aff","#d4aa00","#d45500"),
		xlab = "n events", ylab = "nr triad ids",las=1)
dev.off()







