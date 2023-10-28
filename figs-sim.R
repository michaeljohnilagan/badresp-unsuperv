# load workspace
load("badrespunsuperv-20230408.RData")

# define style
thecols = c("blue","dodgerblue","orchid","coral","red")
thepchs = c("1","3","9")
thecex = 1.5

# rename
colnames(simtab) = replace(colnames(simtab),match(c("acc","sens","spec",
"killrate"),colnames(simtab)),c("accuracy","sensitivity","specificity","flag rate"))
simtab$columnsused = as.factor(match(simtab$columnsused,simfactors$columnsused))
levels(simtab$columnsused) = c("32","16")
simtab$botdist = as.factor(match(simtab$botdist,simfactors$botdist))
levels(simtab$botdist) = c("unif","binom")

# function: visualize table
vis = function(tab,outcome,factor1,factor2,factor3,factor4,lims=NULL,
legendpos=NULL) {
	# make factors
	f1 = factor(tab[,factor1])
	f2 = factor(tab[,factor2])
	f3 = factor(tab[,factor3])
	f4 = factor(tab[,factor4])
	f1f2 = interaction(f1,f2)
	f3f4 = interaction(f3,f4)
	# compute ranges
	heights = setNames(as.integer(f1f2),f1f2) # heights
	jit = 0.1*scale(runif(levels(f3f4),-1,+1))[as.integer(f3f4)] # random jitter
	jit = 0.05*sapply(1:length(levels(f3f4)),function(i) {
		if(i%%2==0) {
			x = i
		} else {
			x = -(i+1)
		}
		x/2
	})[as.integer(f3f4)] # deterministic jitter
	if(is.null(lims)) {
		lims = range(tab[,outcome])
	} # plot range of outcome
	# draw
	plot(tab[,outcome],heights+jit,xlim=lims,
	xlab=outcome,ylab=NA,yaxt="n",
	pch=thepchs[as.integer(f3)],col=thecols[as.integer(f4)],
	cex=thecex,lwd=2) # main plotting
	axis(2,at=heights,labels=with(tab,paste(f1,"\n",f2,sep="")),gap.axis=0.5,
	padj=0) # vertical axis labels
	abline(h=0.5+unique(heights),lty=1,col="black") # guide lines per group
	# make legend
	f3f4tab = t(sapply(unique(f3f4),function(v) {
		c(as.integer(f3)[f3f4==v][1],as.integer(f4)[f3f4==v][1])
	}))
	f3f4name = unique(paste(factor3,": ",f3,", ",factor4,": ",f4,sep=""))
	if(!is.null(legendpos)) {
		legend(legendpos,legend=f3f4name,pch=thepchs[f3f4tab[,1]],
		col=thecols[f3f4tab[,2]],lty=0,lwd=2,cex=thecex*0.75)
	}
	# return object
	tab[,c(factor3,factor4,factor1,factor2,outcome)]
}

# plot accuracy
pdf("vis-acc.pdf")
par(cex.lab=1.5, cex.axis=1.5, cex=1.5)
	#par(mfrow=c(2,2))
vis(subset(simtab,method=="11"),"accuracy","botdist","columnsused","n","contam",lims=0:1) # both
abline(v=c(0.9,0.95),lty=3)
legend("topleft",legend=simfactors$n,pch=thepchs,title="test set size",cex=1,bg="white")
legend("bottomleft",legend=simfactors$contam,col=thecols,pch=15,title="contamination rate",cex=1,bg="white")
vis(subset(simtab,method=="10"),"accuracy","botdist","columnsused","n","contam",lims=0:1) # LOO only
abline(v=c(0.9,0.95),lty=3)
vis(subset(simtab,method=="01"),"accuracy","botdist","columnsused","n","contam",lims=0:1) # perms only
abline(v=c(0.9,0.95),lty=3)
vis(subset(simtab,method=="00"),"accuracy","botdist","columnsused","n","contam",lims=0:1) # neither
abline(v=c(0.9,0.95),lty=3)
dev.off()

# plot sensitivity
pdf("vis-sens.pdf")
par(cex.lab=1.5, cex.axis=1.5, cex=1.5)
	#par(mfrow=c(2,2))
vis(subset(simtab,method=="11"),"sensitivity","botdist","columnsused","n","contam",lims=0:1) # both
abline(v=0.95,lty=3)
legend("topleft",legend=simfactors$n,pch=thepchs,title="test set size",cex=1,bg="white")
legend("bottomleft",legend=simfactors$contam,col=thecols,pch=15,title="contamination rate",cex=1,bg="white")
vis(subset(simtab,method=="10"),"sensitivity","botdist","columnsused","n","contam",lims=0:1) # LOO only
abline(v=0.95,lty=3)
vis(subset(simtab,method=="01"),"sensitivity","botdist","columnsused","n","contam",lims=0:1) # perms only
abline(v=0.95,lty=3)
vis(subset(simtab,method=="00"),"sensitivity","botdist","columnsused","n","contam",lims=0:1) # neither
abline(v=0.95,lty=3)
dev.off()

# plot specificity
pdf("vis-spec.pdf")
par(cex.lab=1.5, cex.axis=1.5, cex=1.5)
	#par(mfrow=c(2,2))
vis(subset(simtab,method=="11"),"specificity","botdist","columnsused","n","contam",lims=0:1) # both
vis(subset(simtab,method=="10"),"specificity","botdist","columnsused","n","contam",lims=0:1) # LOO only
vis(subset(simtab,method=="01"),"specificity","botdist","columnsused","n","contam",lims=0:1) # perms only
vis(subset(simtab,method=="00"),"specificity","botdist","columnsused","n","contam",lims=0:1) # neither
legend("topleft",legend=simfactors$n,pch=thepchs,title="test set size",cex=1,bg="white")
legend("bottomleft",legend=simfactors$contam,col=thecols,pch=15,title="contamination rate",cex=1,bg="white")
dev.off()

# plot kill rate
pdf("vis-killrate.pdf")
par(cex.lab=1.5, cex.axis=1.5, cex=1.5)
	#par(mfrow=c(2,2))
vis(subset(simtab,method=="11"),"flag rate","botdist","columnsused","n","contam",lims=0:1) # both
abline(v=c(0.05,0.25,0.5,0.75,0.95),lty=3,col=thecols)
vis(subset(simtab,method=="10"),"flag rate","botdist","columnsused","n","contam",lims=0:1) # LOO only
abline(v=c(0.05,0.25,0.5,0.75,0.95),lty=3,col=thecols)
vis(subset(simtab,method=="01"),"flag rate","botdist","columnsused","n","contam",lims=0:1) # perms only
abline(v=c(0.05,0.25,0.5,0.75,0.95),lty=3,col=thecols)
vis(subset(simtab,method=="00"),"flag rate","botdist","columnsused","n","contam",lims=0:1) # neither
abline(v=c(0.05,0.25,0.5,0.75,0.95),lty=3,col=thecols)
dev.off()
