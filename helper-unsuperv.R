# function: reduce NRI to one dimension
redu1 = function(x,nulldist,idealpoint) {
	# null variance matrix
	sig = cov(nulldist)
	# distance from ideal point
	reduobs = mahalanobis(x,idealpoint,sig)
	redunull = mahalanobis(nulldist,idealpoint,sig)
	# p value
	pval = ecdf(redunull)(reduobs)
	return(pval)
}

# function: detect bots
detectbots = function(likert,L1,P1,nri,numbotseach,pointscale,prob) {
	# impute over missing
	likert = as.matrix(likert)
	imputevalue = median(1:pointscale)
	zte = ifelse(is.na(likert),imputevalue,likert)
	# dimensions of test set
	n = nrow(zte)
	numitems = ncol(zte)
	# NRI functions and ideal point
	nri = unique(nri)
	nrifuns = list(mahal=mahal,ptc=ptc)[nri]
	idealpoint = c(mahal=0,ptc=+1)[nri]
	# null distribution in likert space
	if(P1) {
		ztr = lapply(1:n,function(i) {
			obs = unname(zte[i,])
			synth = t(replicate(numbotseach,{
				sample(obs)
			}))
			rbind(obs,synth)
		}) # permutations as null
	} else {
		ztr = lapply(1:n,function(i) {
			rbot(numbotseach,pointscale,numitems,prob)
		}) # specified null
	}
	# keep track which respondent synthetic is for
	if(P1) {
		familyid = rep(1:n,each=1+numbotseach)
	} else if(!P1&L1) {
		familyid = rep(1:n,each=numbotseach)
	} else {
		familyid = rep(NA,times=n*numbotseach)
	}
	# from likert to NRI space
	if(L1) {
		ref = lapply(1:n,function(i) {
			zte[-i,,drop=FALSE]
		}) # each respondent has its own anchor
		xte = sapply(nrifuns,function(f) {
			sapply(1:n,function(i) {
				f(zte[i,,drop=FALSE],ref[[i]])
			})
		}) # observed NRI, done per anchor
		xtr = lapply(1:n,function(i) {
			sapply(nrifuns,function(f) {
				f(ztr[[i]],ref[[i]])
			})
		}) # null NRI, done per anchor
	} else {
		xte = sapply(nrifuns,function(f) {
			f(zte,zte)
		}) # observed NRI, done all at once
		ztr = do.call(rbind,ztr) # put together
		xtr = sapply(nrifuns,function(f) {
			f(ztr,zte)
		}) # null NRI, done all at once
		if(P1) {
			xtr = split(as.data.frame(xtr),familyid)
		} # separate null NRI by respondent
	}
	# computation of p value
	if(L1|P1) {
		pval = sapply(1:n,function(i) {
			uniquerowsinnull = nrow(unique(xtr[[i]]))
			if(uniquerowsinnull==1) {
				1
			} else {
				redu1(xte[i,,drop=FALSE],xtr[[i]],idealpoint)
			} # special handling when only one unique row
		}) # do not use all synthetic
	} else {
		pval = redu1(xte,xtr,idealpoint) # use all synthetic
	}
	# put together
	if(L1|P1) {
		xtr = do.call(rbind,xtr)
	}
	return(list(pval=pval,nri_obs=xte,nri_synth=xtr,familyid=familyid))
}

# function: detect bots, simplified args
detectbotseasy = function(likert,numbotseach,pointscale,threshold=0.05) {
	inner = detectbots(likert,L1=TRUE,P1=TRUE,nri=c("mahal","ptc"),
	numbotseach=numbotseach,pointscale=pointscale,
	prob=NULL) # compute p values
	yhat = ifelse(inner$pval<threshold,"spare","kill") # binary predictions
	return(c(inner,list(yhat=yhat))) # put together
}
