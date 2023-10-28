# function: generate bots
rbot = function(n,pointscale,numitems,prob) {
	samp = t(replicate(n,{
		sample(1:pointscale,size=numitems,prob=prob,replace=TRUE)
	})) # each row is a participant
	return(samp)
}

# function: mahalanobis distance
mahal = function(x,ref) {
	# get anchor parameters
	mu = colMeans(ref)
	sig = cov(ref)
	# compute statistic
	sqmahal = mahalanobis(x,center=mu,cov=sig)
	mahal = sqrt(sqmahal) # take square root
	return(unname(mahal))
}

# function: person total correlation
ptc = function(x,ref,default=-1) {
	# get anchor parameters
	mu = colMeans(ref)
	# compute statistic
	ptc = apply(x,1,function(v) {
		cor(v,mu)
	})
	ptc = ifelse(is.na(ptc),default,ptc) # impute over undefined
	return(ptc)
}

# function: plot NRI space
nriplot = function(xte,xtr=NULL,yhat=NULL,yte=NULL,ytr=NULL,plottrain=TRUE,
getpc=FALSE,trplotlim=FALSE) {
	# principal component scores
	if(getpc) {
		if(!is.null(xtr)) {
			stdtr = scale(xtr)
			stdte = scale(xte,center=colMeans(xtr),
			scale=sapply(xtr,sd))
			eig = eigen(cor(stdtr))
			loctr = stdtr%*%eig$vectors[,1:2]
			locte = stdte%*%eig$vectors[,1:2]
		} else {
			eig = eigen(cor(xte))
			locte = scale(xte)%*%eig$vectors[,1:2]
		}
		thexlab = "PC1"
		theylab = "PC2"
	} else {
		if(!is.null(xtr)) {
			loctr = xtr
		}
		locte = xte
		thexlab = colnames(xte)[1]
		theylab = colnames(xte)[2]
	} # convert to PC scores or not
	# draw plot
	if(trplotlim&!is.null(xtr)) {
		thexlim = range(c(loctr[,1],locte[,1]))
		theylim = range(c(loctr[,2],locte[,2]))
	} else {
		thexlim = range(locte[,1])
		theylim = range(locte[,2])
	}
	plot(locte[,1],locte[,2],type="n",xlab=thexlab,ylab=theylab,
	xlim=thexlim,ylim=theylim)
	if(plottrain&!is.null(xtr)) {
		if(!is.null(ytr)) {
			points(loctr[,1],loctr[,2],pch=ifelse(ytr=="bot",4,1),
			col="green")
		} else {
			points(loctr[,1],loctr[,2],col="green",pch=15)
		} # training labels available or not
	} # plot training set
	if(!is.null(yte)&!is.null(yhat)) {
		points(locte[,1],locte[,2],pch=ifelse(yte=="bot",4,1),
		col=ifelse(yhat=="kill","red","blue"),lwd=2)
	} else if(is.null(yte)&!is.null(yhat)) {
		points(locte[,1],locte[,2],col=ifelse(yhat=="kill","red",
		"blue"),pch=15)
	} else if(!is.null(yte)&is.null(yhat)) {
		points(locte[,1],locte[,2],pch=ifelse(yte=="bot",4,1),lwd=2)
	} else if(is.null(yte)&is.null(yhat)) {
		points(locte[,1],locte[,2],pch=15)
	}
	# return test set coordinates
	return(locte[,1:2])
}

# function: performance metrics
perfmet = function(y,yhat) {
	# assert
	stopifnot(all(y%in%(0:1))|all(y%in%c("bot","human")))
	stopifnot(all(yhat%in%(0:1))|all(yhat%in%c("kill","spare")))
	# confusion table
	confusion = table(y,yhat)
	# class label format
	if(!is.numeric(y)) {
		y = ifelse(y=="bot",1,0)
	}
	if(!is.numeric(yhat)) {
		yhat = ifelse(yhat=="kill",1,0)
	}
	# compute metrics
	acc = mean(y==yhat) # accuracy
	spec = mean(yhat[y==0]==0) # specificity
	sens = mean(yhat[y==1]==1) # sensitivity
	ppv = mean(y[yhat==1]==1) # positive predictive value
	npv = mean(y[yhat==0]==0) # negative predictive value
	killrate = mean(yhat==1) # kill rate
	# put together
	outcomemeasures = list(acc=acc,spec=spec,sens=sens,ppv=ppv,npv=npv,
	killrate=killrate)
	return(list(confusion=confusion,
	outcomemeasures=unlist(outcomemeasures)))
}
