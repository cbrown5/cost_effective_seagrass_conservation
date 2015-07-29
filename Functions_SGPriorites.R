#Impact model and prioritisation functions
## For Giakoumi et al. 2015 Marine Policy 61, 95-102
### 29 July 2015



#Function to get costs
getCosts <- function(x) x$cost
getnumreefs <- function(x) x$nreefsact
getnummoor <- function(x) x$nmoorsact


#
# MODEL OF IMPACTS  - HOW MUCH SEAGRASS REMAINS
#

ModelImpacts <- function(dat, N, reef.cost, moor.cost, trawl.pres, nreefsPU){

	totcover.2010 = sum(dat$sg)
	##Costs
	reef.act.cost = reef.cost * dat$sg
	reef.act.num <- nreefsPU * dat$sg
	sg.naive <- rep(0, N) #what threat scenario thinks is sg cover
	sg.naive[dat$sg>0] <- max(dat$sg)
	reef.cost.naive <- reef.cost * sg.naive
	yat.cat = sort(unique(dat$boats))
	moor.act.cost = dat$boats* moor.cost
	moor.act.num <- dat$boats
	both.act.cost <- reef.act.cost + moor.act.cost
	
	#Set to zero so we don't double up on actions. 
	both.act.cost[(reef.act.cost==0) | (moor.act.cost ==0)] <- NA

	###### YACHTS ############
	yachts.pres = dat$boats * yacht.maxpropswept
	yachts.pres[yachts.pres>1]=1

	##### COMBINED YACHTS AND TRAWL BOAT IMPACT #########
	total.pres = yachts.pres + trawl.pres
	total.pres[total.pres>1] = 1 #make sure it is not greater than 1

	#### SEAGRASS LOSS WITH NO YACHTS OR TRAWLING
	sgend.both = dat$sg #copy seagrass in present day
	iunstop <- dat$farms>=farm.thresh | dat$settle>=settle.thresh
	sgend.both[iunstop] = 0

	#####  SEAGRASS REMAINING WITH NO ACTION #######
	sgend.noman = sgend.both*(1 - total.pres)
	sgend.noman.noUS <- dat$sg*(1 - total.pres)

	#####  SEAGRASS REMAINING WITH NO YACHTS (JUST TRAWLING) #######
	sgend.moor = sgend.both *(1 - trawl.pres)

	#####  SEAGRASS REMAINING WITH NO TRAWLING (JUST YACHTS) #######
	sgend.reef = sgend.both *(1- yachts.pres)

	##### PROPORTION SEAGRASS LOSS WITH NO ACTION OR ACTIONS EVERYWHERE
	totcover.unstoppable = sum(sgend.both)
	sg.proplossUS = round((totcover.unstoppable - totcover.2010)/totcover.2010, digits = 3)

	totcover.end = sum(sgend.noman)
	sg.proploss = round(totcover.end/totcover.2010, digits = 3)

	totcover.end.both = sum(sgend.both)
	sg.proploss.both = round(totcover.end.both/totcover.2010, digits = 3)

	##### Cost effectiveness for all actions in all PUs
	#CE = avoided loss / cost = SG cover saved per euro
	both.roi = (sgend.both - sgend.noman) / (both.act.cost)
	moor.roi = (sgend.moor - sgend.noman) / moor.act.cost 
	reef.roi = (sgend.reef - sgend.noman) /reef.act.cost
	##### Loss rate for all actions in all PUs
	#effectiveness = avoided loss 
	both.loss = (sgend.both - sgend.noman) 
	moor.loss = (sgend.moor - sgend.noman)
	reef.loss = (sgend.reef - sgend.noman) 

	#
	# Create matrices of actions
	#
	#Action matrix. First row is base cover, can update methods in other rows
	iaction <- rep(1, N) #column number for action

	cover.mat <- cbind(sgend.noman, sgend.reef, sgend.moor, sgend.both)
	cost.mat <- cbind(rep(0, N), reef.act.cost, moor.act.cost, both.act.cost)
	cost.mat.naive <- cbind(rep(0, N), reef.cost.naive, moor.act.cost, both.act.cost)
	roi.mat <- cbind(rep(NA, N), reef.roi, moor.roi, both.roi)
	Aloss.mat <- cbind(rep(0, N), reef.loss, moor.loss, both.loss)
	Aloss.mat[is.na(both.act.cost),4] <- NA
	Numreefs.mat <- cbind(rep(0,N), reef.act.num, rep(0, N), reef.act.num)
	Nummoor.mat <- cbind(rep(0,N), rep(0, N), moor.act.num, moor.act.num)
	
	results <- list(sg.proploss = sg.proploss,totcover.2010 = totcover.2010, cover.mat = cover.mat, roi.mat = roi.mat, cost.mat = cost.mat, cost.mat.naive = cost.mat.naive, Aloss.mat = Aloss.mat, total.pres = total.pres, Numreefs.mat = Numreefs.mat, Nummoor.mat = Nummoor.mat)
	
	return(results)

}



#Set priorities for cost effectiveness
prioritiesCE <- function(propgoal, dat, N, nmethods, pmats){

iaction.mat <- matrix(0, nrow =N, ncol= nmethods+1)
iaction.mat[,1] <- 1
iaction.mat2 <- iaction.mat
iaction.save <- rep(1, N)

roi.mat2 <- pmats$roi.mat
matdims <- dim(pmats$roi.mat)


#If there is already enough seagrass, set all actions to zero
if (pmats$sg.proploss>propgoal){ 
		results = list(proscore = pmats$sg.proploss, nactions = 0, cost = 0, best.actions = cbind(dat$PU, iaction.save)) #### May need to edit
		
#If we need to protect some seagrass		
	} else{ 
			
	#compilation of cover in PUs with actions and cover in PUs without actions
	sgcov <- sum(pmats$cover.mat*iaction.mat)
	proscore <- sgcov/pmats$totcover.2010   #score for proportion protected
	
	nactions = 0 #number of actions taken in PUs
	while (proscore< propgoal){
		# print(nactions)
			nactions = nactions + 1 #add one action
			if (nactions==N) break		
			
			#Check if there is one cheap action that will get
			# us over the line	
			if(sum((pmats$Aloss.mat/pmats$totcover.2010) >= (propgoal-proscore), na.rm=T)>1){
				costlast <- pmats$cost.mat
				costlast[!((pmats$Aloss.mat/pmats$totcover.2010) >= (propgoal-proscore))] <- NA
				ibest <- arrayInd(which.min(costlast), matdims)
				} else{ #otherwise, choose most CE action
			ibest <- arrayInd(which.max(roi.mat2), matdims) #best action at moment
			}			
			
			iaction.save[ibest[1]] <- ibest[2] #set column no. for action
			iaction.mat2[ibest[1], ] <- 0
			iaction.mat2[ibest[1], ibest[2]] <- 1
			
			#Set best action to NA
			roi.mat2[ibest[1], ibest[2]] <- NA

			#Update protection status
			proscore <- sum(pmats$cover.mat * iaction.mat2)/pmats$totcover.2010
			
			#When we take an action, set non compatible actions to zero
			if (ibest[2] == 2) roi.mat2[ibest[1],3] <- NA #if it was a reef
			if (ibest[2] == 3) roi.mat2[ibest[1],2] <- NA #if it was a mooring
			if (ibest[2] == 4) roi.mat2[ibest[1],2:3] <- NA #if it was both
			
		}
		
		nactions <- sum(iaction.save>1, na.rm=T)
		best.actions <- cbind(dat$PU, iaction.save)
		results <- list(proscore = proscore, nactions = nactions, cost = sum(pmats$cost.mat * iaction.mat2, na.rm=T), best.actions = best.actions, nreefsact = sum(pmats$Numreefs.mat* iaction.mat2, na.rm=T), nmoorsact = sum(pmats$Nummoor.mat* iaction.mat2, na.rm=T))
	}

return(results)
}


#Set priorities for avoided loss
prioritiesAloss <- function(propgoal, dat, N, nmethods, pmats){

iaction.mat <- matrix(0, nrow =N, ncol= nmethods+1)
iaction.mat[,1] <- 1
iaction.mat2 <- iaction.mat
iaction.save <- rep(1, N)

roi.mat2 <- pmats$roi.mat
matdims <- dim(pmats$roi.mat)


#If there is already enough seagrass, set all actions to zero
if (pmats$sg.proploss>propgoal){ 
		results = list(proscore = pmats$sg.proploss, nactions = 0, cost = 0, best.actions = cbind(dat$PU, iaction.save)) #### May need to edit
		
#If we need to protect some seagrass		
	} else{ 
			
	#compilation of cover in PUs with actions and cover in PUs without actions
	sgcov <- sum(pmats$cover.mat*iaction.mat)
	proscore <- sgcov/pmats$totcover.2010   #score for proportion protected
	
	nactions = 0 #number of actions taken in PUs
	while (proscore< propgoal){
		# print(nactions)
			nactions = nactions + 1 #add one action
			if (nactions==N) break		
			
			ibest <- arrayInd(which.max(roi.mat2), matdims) #best action at moment
			iaction.save[ibest[1]] <- ibest[2] #set column no. for action
			iaction.mat2[ibest[1], ] <- 0
			iaction.mat2[ibest[1], ibest[2]] <- 1
			
			#Set best action to NA
			roi.mat2[ibest[1], ibest[2]] <- NA

			#Update protection status
			proscore <- sum(pmats$cover.mat * iaction.mat2)/pmats$totcover.2010
			
			#When we take an action, set non compatible actions to zero
			if (ibest[2] == 2) roi.mat2[ibest[1],3] <- NA #if it was a reef
			if (ibest[2] == 3) roi.mat2[ibest[1],2] <- NA #if it was a mooring
			if (ibest[2] == 4) roi.mat2[ibest[1],2:3] <- NA #if it was both
			
		}
		
		nactions <- sum(iaction.save>1, na.rm=T)
		best.actions <- cbind(dat$PU, iaction.save)
		results <- list(proscore = proscore, nactions = nactions, cost = sum(pmats$cost.mat * iaction.mat2, na.rm=T), best.actions = best.actions, nreefsact = sum(pmats$Numreefs.mat* iaction.mat2, na.rm=T), nmoorsact = sum(pmats$Nummoor.mat* iaction.mat2, na.rm=T))
	}

return(results)
}


#Set priorities for other schemes
prioritiesOther <- function(MyPriorityInd, propgoal, dat, N, nmethods, pmats, UseNaive=F){

#Use naive costs?
if (UseNaive){ 
	cost.mat <- pmats$cost.mat.naive	
	} else{
	cost.mat <- pmats$cost.mat	
		}
	
iaction.mat <- matrix(0, nrow =N, ncol= nmethods+1)
iaction.mat[,1] <- 1
iaction.mat2 <- iaction.mat
iaction.save <- rep(1, N)
roi.mat2 <- pmats$roi.mat
matdims <- dim(pmats$roi.mat)

#If there is already enough seagrass, set all actions to zero
if (pmats$sg.proploss>propgoal){ 
		results = list(proscore = pmats$sg.proploss, nactions = 0, cost = 0, best.actions = cbind(dat$PU, iaction.save)) #### May need to edit
		
#If we need to protect some seagrass		
	} else{ 
	
	#Create order and shuffle randomly
	iord2 <- sample(1:N, N, replace=F)
	iord.action <- order(MyPriorityInd,iord2, decreasing=T)
	# plot(ThreatVals[iord.action]) #checks
	# dat$PUID[iord.action]
	
	#compilation of cover in PUs with actions and cover in PUs without actions
	sgcov <- sum(pmats$cover.mat*iaction.mat)
	proscore <- sgcov/pmats$totcover.2010   #score for proportion protected
	
	nactions = 0 #number of actions taken in PUs
	while (proscore< propgoal){
		# print(nactions)
			nactions = nactions + 1 #add one action
			if (nactions==N) break		
			#find column with the greatest benefits
			# icol <- which.max(pmats$roi.mat[iord.action[nactions],])
			icol <- which.max(pmats$cover.mat[iord.action[nactions],])
			
			ibest <- c(iord.action[nactions],icol)
			iaction.save[ibest[1]] <- ibest[2] #set column no. for action
			iaction.mat2[ibest[1], ] <- 0
			iaction.mat2[ibest[1], ibest[2]] <- 1
			
			#Update protection status
			proscore <- sum(pmats$cover.mat * iaction.mat2)/pmats$totcover.2010
		}
		
		nactions <- sum(iaction.save>1, na.rm=T)
		best.actions <- cbind(dat$PU, iaction.save)
		results2 <- list(proscore = proscore, nactions = nactions, cost = sum(cost.mat * iaction.mat2, na.rm=T), best.actions = best.actions, nreefsact = sum(pmats$Numreefs.mat* iaction.mat2, na.rm=T), nmoorsact = sum(pmats$Nummoor.mat* iaction.mat2, na.rm=T))
	}
}
