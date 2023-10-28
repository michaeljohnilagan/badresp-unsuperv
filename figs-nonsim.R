# load workspace
load("badrespunsuperv-20230408.RData")

# setup feature space (a)
set.seed(1704)
env_featspace_a = new.env()
with(env_featspace_a,{
	# params
	n = 50
	contam = 0.5
	botdist = "unif"
	columnsused = "all"
	numbotseach = 200
	# spell out botdist and columnsused
	botdistinfo = list(unif=c(1,1,1,1,1),binom=dbinom(0:4,size=4,prob=0.5))
	columnsusedinfo = list(all=rep(TRUE,ncol(humandata)),
	even=(1:ncol(humandata)%%2)==0)
	# sample sizes
	n0 = round(n*(1-contam))
	n1 = n-n0
	# generate test data likert
	humandataused = humandata[,columnsusedinfo[[columnsused]]]
	zte0 = samplerows(n0,data=humandataused) # human part
	zte1 = rbot(n1,pointscale=pointscale,numitems=ncol(humandataused),
	prob=botdistinfo[[botdist]]) # bot part
	zte = rbind(zte0,zte1) # all respondents
	zte = ifelse(is.na(zte),3,zte) # impute over missing
	yte = c(rep("human",n0),rep("bot",n1)) # true labels
	# compute NRIs
	detection = detectbots(zte,L1=TRUE,P1=TRUE,nri=nri,
	numbotseach=numbotseach,
	pointscale=pointscale,prob=NULL) # uniform synthetic for P0
	nri_obs = as.data.frame(detection$nri_obs)
	# plot
	with(nri_obs,{
		plot(mahal,ptc,xlab="feature 1",ylab="feature 2",pch="?")
	})
})

# setup feature space (b)
set.seed(1705)
env_featspace_b = new.env()
with(env_featspace_b,{
	# params
	n = 50
	contam = 0.5
	botdist = "unif"
	columnsused = "all"
	numbotseach = 200
	# spell out botdist and columnsused
	botdistinfo = list(unif=c(1,1,1,1,1),binom=dbinom(0:4,size=4,prob=0.5))
	columnsusedinfo = list(all=rep(TRUE,ncol(humandata)),
	even=(1:ncol(humandata)%%2)==0)
	# sample sizes
	n0 = round(n*(1-contam))
	n1 = n-n0
	# generate test data likert
	humandataused = humandata[,columnsusedinfo[[columnsused]]]
	zte0 = samplerows(n0,data=humandataused) # human part
	zte1 = rbot(n1,pointscale=pointscale,numitems=ncol(humandataused),
	prob=botdistinfo[[botdist]]) # bot part
	zte = rbind(zte0,zte1) # all respondents
	zte = ifelse(is.na(zte),3,zte) # impute over missing
	yte = c(rep("human",n0),rep("bot",n1)) # true labels
	# compute NRIs
	detection = detectbots(zte,L1=TRUE,P1=TRUE,nri=nri,
	numbotseach=numbotseach,
	pointscale=pointscale,prob=NULL) # uniform synthetic for P0
	nri_obs = as.data.frame(detection$nri_obs)
	# fit logistic regression
	linmod = glm(factor(yte)~mahal+ptc,data=nri_obs,
	family=binomial(link="logit"))
	# plot
	with(nri_obs,{
		plot(mahal,ptc,pch=ifelse(yte=="human","0","1"),
		xlab="feature 1",ylab="feature 2")
	})
	abline(-coef(linmod)[1:2]/coef(linmod)[3],lty=1)
	legend("topright",legend=c("human","bot"),pch=c("0","1"),bg="white")
})

# plot feature space
pdf("featspace.pdf")
par(cex.lab=1.5, cex.axis=1.5, cex=1.25)
    #par(mfrow=c(1,2))
with(env_featspace_a,{
	with(nri_obs,{
		plot(mahal,ptc,xlab="mahalanobis distance",
		ylab="person total correlation",pch="?",
		xlim=range(c(mahal,env_featspace_b$nri_obs$mahal)),
		ylim=range(c(ptc,env_featspace_b$nri_obs$ptc)))
	})
}) # test set
with(env_featspace_b,{
	with(nri_obs,{
		plot(mahal,ptc,pch=ifelse(yte=="human","0","1"),
		xlab="mahalanobis distance",ylab="person total correlation",
		xlim=range(c(mahal,env_featspace_a$nri_obs$mahal)),
		ylim=range(c(ptc,env_featspace_a$nri_obs$ptc)))
	})
	abline(-coef(linmod)[1:2]/coef(linmod)[3],lty=1)
	legend("topright",legend=c("human","bot"),pch=c("0","1"),bg="white")
}) # train set
dev.off()

# setup discriminability (a)
set.seed(1746)
env_discrim_a = new.env()
with(env_discrim_a,{
	# params
	n = 100
	contam = 0.5
	botdist = "unif"
	columnsused = "all"
	# spell out botdist and columnsused
	botdistinfo = list(unif=c(1,1,1,1,1),binom=dbinom(0:4,size=4,prob=0.5))
	columnsusedinfo = list(all=rep(TRUE,ncol(humandata)),
	even=(1:ncol(humandata)%%2)==0)
	# sample sizes
	n0 = round(n*(1-contam))
	n1 = n-n0
	# generate test data likert
	humandataused = humandata[,columnsusedinfo[[columnsused]]]
	zte0 = samplerows(n0,data=humandataused) # human part
	zte1 = rbot(n1,pointscale=pointscale,numitems=ncol(humandataused),
	prob=botdistinfo[[botdist]]) # bot part
	zte = rbind(zte0,zte1) # all respondents
	zte = ifelse(is.na(zte),3,zte) # impute over missing
	yte = c(rep("human",n0),rep("bot",n1)) # true labels
	# compute NRIs
	anchor = rbot(1e3,pointscale=pointscale,numitems=ncol(humandataused),
	prob=botdistinfo[[botdist]]) # bot part
	nri_obs = data.frame(mahal=mahal(zte,anchor),ptc=ptc(zte,anchor))
	# plot
	with(nri_obs,{
		plot(mahal,ptc,pch=ifelse(yte=="human","0","1"),
		xlab="mahalanobis distance",ylab="person total correlation")
	})
	legend("topright",legend=c("human","bot"),pch=c("0","1"),bg="white")
})

# setup discriminability (b)
set.seed(1746)
env_discrim_b = new.env()
with(env_discrim_b,{
	# params
	n = 50
	contam = 0.9
	ntr = 50
	contamtr = 0.1
	botdist = "unif"
	columnsused = "all"
	# spell out botdist and columnsused
	botdistinfo = list(unif=c(1,1,1,1,1),binom=dbinom(0:4,size=4,prob=0.5))
	columnsusedinfo = list(all=rep(TRUE,ncol(humandata)),
	even=(1:ncol(humandata)%%2)==0)
	# sample sizes
	n0 = round(n*(1-contam))
	n1 = n-n0
	n0tr = round(ntr*(1-contamtr))
	n1tr = ntr-n0tr
	# generate test data likert
	humandataused = humandata[,columnsusedinfo[[columnsused]]]
	zte0 = samplerows(n0,data=humandataused) # human part
	zte1 = rbot(n1,pointscale=pointscale,numitems=ncol(humandataused),
	prob=botdistinfo[[botdist]]) # bot part
	zte = rbind(zte0,zte1) # all respondents
	zte = ifelse(is.na(zte),3,zte) # impute over missing
	yte = c(rep("human",n0),rep("bot",n1)) # true labels
	# generate train data likert
	ztr0 = samplerows(n0tr,data=humandataused) # human part
	ztr1 = rbot(n1tr,pointscale=pointscale,numitems=ncol(humandataused),
	prob=botdistinfo[[botdist]]) # bot part
	ztr = rbind(ztr0,ztr1) # all respondents
	ztr = ifelse(is.na(ztr),3,ztr) # impute over missing
	ytr = c(rep("human",n0tr),rep("bot",n1tr)) # true labels
	# compute NRIs
	xte = data.frame(mahal=mahal(zte,zte),ptc=ptc(zte,zte))
	xtr = data.frame(mahal=mahal(ztr,ztr),ptc=ptc(ztr,ztr))
	# plot
	with(xte,{
		plot(mahal,ptc,pch=ifelse(yte=="human","0","1"),
		xlab="mahalanobis distance",ylab="person total correlation",
		col="purple",xlim=range(c(xte$mahal,xtr$mahal)),
		ylim=range(c(xte$ptc,xtr$ptc)))
	})
	with(xtr,{
		points(mahal,ptc,pch=ifelse(ytr=="human","0","1"),col="green2")
	})
	legend("topright",legend=c("human","bot"),pch=c("0","1"),bg="white")
})

# setup discriminability (c)
set.seed(1746)
env_discrim_c = new.env()
with(env_discrim_c,{
	# params
	n = 50
	contam = 0.5
	ntr = 50
	contamtr = 0.5
	botdist = "unif"
	columnsused = "all"
	# spell out botdist and columnsused
	botdistinfo = list(unif=c(1,1,1,1,1),binom=dbinom(0:4,size=4,prob=0.5))
	columnsusedinfo = list(all=rep(TRUE,ncol(humandata)),
	even=(1:ncol(humandata)%%2)==0)
	# sample sizes
	n0 = round(n*(1-contam))
	n1 = n-n0
	n0tr = round(ntr*(1-contamtr))
	n1tr = ntr-n0tr
	# generate test data likert
	humandataused = humandata[,columnsusedinfo[[columnsused]]]
	zte0 = samplerows(n0,data=humandataused) # human part
	zte1 = rbot(n1,pointscale=pointscale,numitems=ncol(humandataused),
	prob=botdistinfo[[botdist]]) # bot part
	zte = rbind(zte0,zte1) # all respondents
	zte = ifelse(is.na(zte),3,zte) # impute over missing
	yte = c(rep("human",n0),rep("bot",n1)) # true labels
	# generate train data likert
	ztr0 = samplerows(n0tr,data=humandataused) # human part
	ztr1 = rbot(n1tr,pointscale=pointscale,numitems=ncol(humandataused),
	prob=botdistinfo[[botdist]]) # bot part
	ztr = rbind(ztr0,ztr1) # all respondents
	ztr = ifelse(is.na(ztr),3,ztr) # impute over missing
	ytr = c(rep("human",n0tr),rep("bot",n1tr)) # true labels
	# compute NRIs
	xte = data.frame(mahal=mahal(zte,ztr),ptc=ptc(zte,ztr))
	xtr = data.frame(mahal=mahal(ztr,ztr),ptc=ptc(ztr,ztr))
	# plot
	with(xte,{
		plot(mahal,ptc,pch=ifelse(yte=="human","0","1"),
		xlab="mahalanobis distance",ylab="person total correlation",
		col="purple",xlim=range(c(xte$mahal,xtr$mahal)),
		ylim=range(c(xte$ptc,xtr$ptc)))
	})
	with(xtr,{
		points(mahal,ptc,pch=ifelse(ytr=="human","0","1"),col="green2")
	})
	legend("topright",legend=c("human","bot"),pch=c("0","1"),bg="white")
})

# plot discriminability
pdf("discrim.pdf")
par(cex.lab=1.5, cex.axis=1.5, cex=1.25)
    #par(mfrow=c(1,3))
with(env_discrim_a,{
	with(nri_obs,{
		plot(mahal,ptc,pch=ifelse(yte=="human","0","1"),
		xlab="mahalanobis distance",ylab="person total correlation")
	})
	legend("topleft",legend=c("human","bot"),pch=c("0","1"),bg="white")
}) # D1
with(env_discrim_b,{
	# plot
	with(xte,{
		plot(mahal,ptc,pch=ifelse(yte=="human","0","1"),
		xlab="mahalanobis distance",ylab="person total correlation",
		col="purple",xlim=range(c(xte$mahal,xtr$mahal)),
		ylim=range(c(xte$ptc,xtr$ptc)))
	})
	with(xtr,{
		points(mahal,ptc,pch=ifelse(ytr=="human","0","1"),col="green2")
	})
	legend("bottomleft",legend=c("human","bot"),pch=c("0","1"),bg="white")
}) # D2
with(env_discrim_c,{
	# plot
	with(xte,{
		plot(mahal,ptc,pch=ifelse(yte=="human","0","1"),
		xlab="mahalanobis distance",ylab="person total correlation",
		col="purple",xlim=range(c(xte$mahal,xtr$mahal)),
		ylim=range(c(xte$ptc,xtr$ptc)))
	})
	with(xtr,{
		points(mahal,ptc,pch=ifelse(ytr=="human","0","1"),col="green2")
	})
	legend("topright",legend=c("human","bot"),pch=c("0","1"),bg="white")
}) # D3
dev.off()

# setup bot distribution (a)
set.seed(1651)
env_botdist_a = new.env()
with(env_botdist_a,{
	# params
	n = 100
	contam = 0.5
	botdist = "unif"
	columnsused = "all"
	numbotseach = 200
	# spell out botdist and columnsused
	botdistinfo = list(unif=c(1,1,1,1,1),binom=dbinom(0:4,size=4,prob=0.5))
	columnsusedinfo = list(all=rep(TRUE,ncol(humandata)),
	even=(1:ncol(humandata)%%2)==0)
	# sample sizes
	n0 = round(n*(1-contam))
	n1 = n-n0
	# generate test data likert
	humandataused = humandata[,columnsusedinfo[[columnsused]]]
	zte0 = samplerows(n0,data=humandataused) # human part
	zte1 = rbot(n1,pointscale=pointscale,numitems=ncol(humandataused),
	prob=botdistinfo[[botdist]]) # bot part
	ztr = samplerows(n0,data=humandataused) # human part
	ztr = ifelse(is.na(ztr),3,ztr) # impute over missing
	zte = rbind(zte0,zte1) # all respondents
	zte = ifelse(is.na(zte),3,zte) # impute over missing
	yte = c(rep("human",n0),rep("bot",n1)) # true labels
	# compute NRIs
	xte = data.frame(mahal=mahal(zte,ztr),ptc=ptc(zte,ztr))
	# plot
	with(xte,{
		plot(mahal,ptc,pch=ifelse(yte=="human","0","1"),
		xlab="mahalanobis distance",ylab="person total correlation")
	})
	legend("topright",legend=c("human","bot"),pch=c("0","1"),bg="white")
})

# setup bot distribution (b)
set.seed(1651)
env_botdist_b = new.env()
with(env_botdist_b,{
	# params
	n = 100
	contam = 0.5
	botdist = "binom"
	columnsused = "all"
	numbotseach = 200
	# spell out botdist and columnsused
	botdistinfo = list(unif=c(1,1,1,1,1),binom=dbinom(0:4,size=4,prob=0.5))
	columnsusedinfo = list(all=rep(TRUE,ncol(humandata)),
	even=(1:ncol(humandata)%%2)==0)
	# sample sizes
	n0 = round(n*(1-contam))
	n1 = n-n0
	# generate test data likert
	humandataused = humandata[,columnsusedinfo[[columnsused]]]
	zte0 = samplerows(n0,data=humandataused) # human part
	zte1 = rbot(n1,pointscale=pointscale,numitems=ncol(humandataused),
	prob=botdistinfo[[botdist]]) # bot part
	ztr = samplerows(n0,data=humandataused) # human part
	ztr = ifelse(is.na(ztr),3,ztr) # impute over missing
	zte = rbind(zte0,zte1) # all respondents
	zte = ifelse(is.na(zte),3,zte) # impute over missing
	yte = c(rep("human",n0),rep("bot",n1)) # true labels
	# compute NRIs
	xte = data.frame(mahal=mahal(zte,ztr),ptc=ptc(zte,ztr))
	# plot
	with(xte,{
		plot(mahal,ptc,pch=ifelse(yte=="human","0","1"),
		xlab="mahalanobis distance",ylab="person total correlation",
		xlim=range(env_botdist_a$xte$mahal))
	})
	legend("topright",legend=c("human","bot"),pch=c("0","1"),bg="white")
})

# plot bot distribution
pdf("botdist.pdf")
par(cex.lab=1.5, cex.axis=1.5, cex=1.25)
    #par(mfrow=c(1,2))
with(env_botdist_a,{
	with(xte,{
		plot(mahal,ptc,pch=ifelse(yte=="human","0","1"),
		xlab="mahalanobis distance",ylab="person total correlation")
	})
	legend("topright",legend=c("human","bot"),pch=c("0","1"),bg="white")
}) # unif
with(env_botdist_b,{
	with(xte,{
		plot(mahal,ptc,pch=ifelse(yte=="human","0","1"),
		xlab="mahalanobis distance",ylab="person total correlation",
		xlim=range(env_botdist_a$xte$mahal),ylim=range(env_botdist_a$xte$ptc))
	})
	legend("topright",legend=c("human","bot"),pch=c("0","1"),bg="white")
}) # binom
dev.off()

# setup demo
set.seed(1932)
env_demo_a = list2env(runsimrepl(humandata=humandata,pointscale=5,nri,
numbotseach=numbotseach,n=100,contam=0.75,botdist="binom",method="11",
columnsused="all")) # execute
set.seed(1932)
env_demo_b = list2env(runsimrepl(humandata=humandata,pointscale=5,nri,
numbotseach=numbotseach,n=100,contam=0.75,botdist="binom",method="10",
columnsused="all")) # execute
set.seed(1932)
env_demo_c = list2env(runsimrepl(humandata=humandata,pointscale=5,nri,
numbotseach=numbotseach,n=100,contam=0.75,botdist="binom",method="01",
columnsused="all")) # execute
set.seed(1932)
env_demo_d = list2env(runsimrepl(humandata=humandata,pointscale=5,nri,
numbotseach=numbotseach,n=100,contam=0.75,botdist="binom",method="00",
columnsused="all")) # execute
env_demo_commonrange = with(new.env(),{
	tab = do.call(rbind,lapply(list(env_demo_a,env_demo_b,env_demo_c,
	env_demo_d),function(e) {
		rbind(e$nri_obs,e$nri_synth)
	}))
	list(mahal=range(tab$mahal),ptc=range(tab$ptc))
})

# plot demo
pdf("demo.pdf")
    #par(mfrow=c(2,2))
par(cex.lab=1.5, cex.axis=1.5, cex=1.25)
invisible(sapply(c(env_demo_a,env_demo_b,env_demo_c,env_demo_d),function(e) {
	with(e,{
		nriplot(xte=setNames(as.data.frame(nri_obs),
		c("mahalanobis distance","person total correlation")),
		xtr=nri_synth[sample(1:nrow(nri_synth),size=1e3),],
		yhat=yhat,yte=yte,trplotlim=TRUE)
		legend("bottomright",pch=c(1,4),legend=c("human","bot"),
		bg="white")
		legend("topright",pch=15,col=c("blue","red","green"),
		legend=c("spared","flagged","synthetic"),bg="white")
	})
}))
dev.off()
