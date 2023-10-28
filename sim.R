# required packages and helper functions
library("furrr")
library("future")
source("helper-general.R")
source("helper-unsuperv.R")

# function: sample from data
samplerows = function(n,data) {
	sampindex = sample(1:nrow(data),size=n,replace=TRUE) # with replacement
	return(as.matrix(data[sampindex,,drop=FALSE]))
}

# simulation study constants
numrepl = 500 # number of replicates per cell
humandata = with(new.env(),{
	datoriginal = read.csv("./data.csv",header=TRUE)[,1:32] # load data
	humandata = replace(datoriginal,datoriginal==-1,NA) # code missing
	rowsallmissing = apply(humandata,1,function(v) {
		all(is.na(v))
	}) # rows that are all missing
	humandata[!rowsallmissing,]
}) # human data
pointscale = 5 # number of likert categories
nri = c("mahal","ptc") # NRIs
numbotseach = 200 # number of synthetic rows per observed

# simulation study factors
simfactors = list(n=c(100,300,900),contam=c(0.05,0.25,0.5,0.75,0.95),
botdist=c("unif","binom"),method=c("00","01","10","11"),
columnsused=c("all","even"))

# spell out botdist and columnsused
botdistinfo = list(unif=rep(1,times=pointscale),
binom=dbinom(0:(pointscale-1),size=pointscale-1,prob=0.5))
columnsusedinfo = list(all=rep(TRUE,ncol(humandata)),
even=(1:ncol(humandata)%%2)==0)

# function: run simulation replicate
runsimrepl = function(humandata,pointscale,nri,numbotseach,n,contam,
botdist,method,columnsused) {
	# sample sizes
	n0 = round(n*(1-contam))
	n1 = n-n0
	# generate test data likert
	humandataused = humandata[,columnsusedinfo[[columnsused]]]
	zte0 = samplerows(n0,data=humandataused) # human part
	zte1 = rbot(n1,pointscale=pointscale,numitems=ncol(humandataused),
	prob=botdistinfo[[botdist]]) # bot part
	zte = rbind(zte0,zte1) # all respondents
	yte = c(rep("human",n0),rep("bot",n1)) # true labels
	# choose approach
	if(method=="00") {
		L1 = FALSE
		P1 = FALSE
	} else if(method=="01") {
		L1 = FALSE
		P1 = TRUE
	} else if(method=="10") {
		L1 = TRUE
		P1 = FALSE
	} else if(method=="11") {
		L1 = TRUE
		P1 = TRUE
	} else {
		stop("method not implemented")
	}
	# detection
	detection = detectbots(zte,L1=L1,P1=P1,nri=nri,numbotseach=numbotseach,
	pointscale=pointscale,prob=NULL) # uniform synthetic for P0
	pval = detection$pval
	nri_obs = detection$nri_obs
	nri_synth = detection$nri_synth
	familyid = detection$familyid
	# get binary prediction
	yhat = ifelse(pval<0.05,"spare","kill")
	# evaluate
	met = perfmet(y=yte,yhat=yhat)
	# put together
	return(list(method=method,zte0=zte0,zte1=zte1,yte=yte,
	pval=pval,nri_obs=nri_obs,nri_synth=nri_synth,familyid=familyid,
	yhat=yhat,met=met))
}

# function: run simulation cell
runsimcell = function(numrepl,humandata,pointscale,nri,numbotseach,n,contam,
botdist,method,columnsused) {
	# scenario information
	scenario = data.frame(n=n,contam=contam,botdist=botdist,
	method=method,columnsused=columnsused)
	# do many replicates
	runsimreplthiscell = function(seed) {
		set.seed(seed)
		resu = runsimrepl(humandata=humandata,pointscale=pointscale,
		nri=nri,numbotseach=numbotseach,n=n,contam=contam,
		botdist=botdist,method=method,columnsused=columnsused)
		resu$met$outcomemeasures
	}
	manyreplicates = furrr::future_map(1908+1:numrepl,runsimreplthiscell,
	.progress=FALSE,.options=furrr::furrr_options(seed=NULL))
	# put together
	simplified = as.data.frame(do.call(rbind,manyreplicates))
	together = cbind(scenario,simplified)
	return(together)
}

# function: summarize cell
summarizecell = function(tab) {
	# get scenario
	paramnames = names(simfactors)
	scenario = tab[1,paramnames]
	# get outcome measures summary
	metricindices = setdiff(colnames(tab),paramnames)
	outcomemeasures = colMeans(tab[,metricindices])
	return(as.data.frame(c(scenario,outcomemeasures)))
}

# try single replicate
with(new.env(),{
	set.seed(1910)
	foo = runsimrepl(humandata=humandata,pointscale=5,nri=c("mahal","ptc"),
	numbotseach=200,n=100,contam=0.95,botdist="binom",method="11",
	columnsused="even") # execute
	if(!TRUE) {
		with(foo,{
			nriplot(xte=nri_obs,
			xtr=nri_synth[sample(1:nrow(nri_synth),size=1e3),],
			yhat=yhat,yte=yte,trplotlim=TRUE)
		}) # NRI vs NRI plot
	}
	foo$met # confusion table and outcome measures
})

# parallel processing
future::plan(future::multisession)

# simulation results
simresu = array(list(),dim=sapply(simfactors,length))
for(i1 in 1:length(simfactors$n)) for(i2 in 1:length(simfactors$contam)) 
for(i3 in 1:length(simfactors$botdist)) 
for(i4 in 1:length(simfactors$method)) 
for(i5 in 1:length(simfactors$columnsused)) {
	# report scenario
	with(simfactors,{
		message(paste(Sys.time(),"|",n[i1],contam[i2],botdist[i3],
		method[i4],columnsused[i5],sep=" "))
	})
	# work
	simresu[[i1,i2,i3,i4,i5]] = with(simfactors,{
		runsimcell(numrepl=numrepl,humandata=humandata,
		pointscale=pointscale,nri=nri,numbotseach=numbotseach,
		n=n[i1],contam=contam[i2],botdist=botdist[i3],
		method=method[i4],columnsused=columnsused[i5])
	})
}; Sys.time()
warns = warnings() # save warnings
print(warns)

# summarize cells
simtab = do.call(rbind,lapply(simresu,summarizecell))

# end session
save.image("badrespunsuperv-20230408.RData")
devtools::session_info()
