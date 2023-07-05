# function adds a set of tips to random positions in the tree
# written by Liam J. Revell 2013
# modified by Orlando Schwery 2017
library(ape)
library(phytools)
library(phangorn)
library(MonoPhy)

add.random.clade<-function(tree,ns=NULL,tips=NULL,clade=NULL, cladenames=NULL, edge.length=NULL,order=c("random","input")){
	if (is.null(clade)) {
		stop("Need to set at least one clade, use add.random otherwise")
	} else {
		cladelist <- list()  # open list to add extracted clades into
		for (cladenr in 1:length(clade)) {
			cladeextract <- extract.clade(tree, node=clade[cladenr])  # extract clade in question
			n <- ns[cladenr]
			# internal function
			randomPosn<-function(cladeextract){
				# sum edges cumulatively
				cum.edge<-cumsum(cladeextract$edge.length)
				index<-cladeextract$edge[,2]
				# pick random position
				pos<-runif(1)*sum(cladeextract$edge.length)
				edge<-1; while(pos>cum.edge[edge]) edge<-edge+1
				return(list(node=index[edge],posn=cum.edge[edge]-pos))
			}
			# check if cladeextract is ultrametric (required)
			if(is.ultrametric(cladeextract)) um<-TRUE
			else um<-FALSE
			# set n and/or name tips (if not provided)
			if(is.null(tips)){
				if(is.null(n)) n<-1
				if(is.null(cladenames)) {  # to add unique tip labels...
					tipsE<-paste("t",length(tree$tip)+((1+sum(ns[1:cladenr])-n):(1+sum(ns[1:cladenr]))),sep="")  # modified for unique labels
				} else {  # ...OR unique and labelled by clade they belong to
					tipsE<-paste(cladenames[cladenr],"_", "t",1:(sum(ns[1:cladenr])),sep="")  # modified for unique labels
				}
			} else {
        n<-length(tips)
        tipsE <- tips
      }
			if(is.null(edge.length)) if(!um) edge.length<-runif(n=n,min=min(cladeextract$edge.length),max=max(cladeextract$edge.length))
			# set order
			if(order[1]=="random"){
				o<-sample(1:n)
				tipsE<-tipsE[o]
				if(!is.null(edge.length)) edge.length<-edge.length[o]
			}
			# add tips
			for(i in 1:n) {
				where<-randomPosn(cladeextract)
				if(is.null(edge.length)) cladeextract<-bind.tip(cladeextract,tipsE[i],where=where$node,position=where$posn)
				else cladeextract<-bind.tip(cladeextract,tipsE[i],where=where$node,position=where$posn,edge.length=edge.length[i])
			}
			cladelist[[cladenr]] <- cladeextract
		}
		# prep lists for all descendants
		keeptips <- c()
		kicktips <- c()
		intedgelength <- c()
		for (cladetips in 1:length(clade)) {  # gor all clades...
			tippick <- Descendants(tree, node=clade[cladetips], type=c("tips"))  # ... list descendants...
			keeptips <- c(keeptips, tippick[[1]][1])  # ... keep adding first of them to keep list...
			kicktips <- c(kicktips, tippick[[1]][-1])  # ... keep adding remaining to kick list
			intedgelength <- c(intedgelength, tree$edge.length[which(tree$edge[, 2]==clade[cladetips])])  # ... keep adding lengths of branches subtending the clades
		}
		keeptiplabels <- tree$tip.label[keeptips]  # get tip names for single tips to be grafted upon
		tree <- collapse.singles(drop.tip(tree, kicktips))  # reduce tree to one tip per clade (keeplist in, kicklist out)
		for (graftstep in 1:length(clade)) {  # graft cladelist entries in...
			tree$edge.length[which(tree$edge[, 2]==which(tree$tip.label==keeptiplabels[graftstep]))] <- intedgelength[graftstep]
			tree <-bind.tree(tree, cladelist[[graftstep]], where=which(tree$tip.label==keeptiplabels[graftstep]))  # ... add each extracted clade onto it's grafting-stump-tip
		}
	}
	return(tree)
}
