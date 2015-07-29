##### Run seagrass optimisation functions
### To compare the different prioritisation methods
#For different trawl impacts
#Outputs tables of cost for each method, as well as locations for 
#actions under each prioritisation scheme. 
#
# For Giakoumi et al. 2015 Marine Policy 61, 95-102
### 29 July 2015


rm(list=ls())
library(RColorBrewer)
plottrawl <- T

#Folders
foldername <- c('maps/') #folder for saving maps
foldername2 <- c('tables/') #folder for saving tables

## Code for Table/ figure 1 and webfigure 3
ThreatsFile <- 'seagrass_threats005.csv'
settlenum <- '005'

setwd('/Mediterranean Seagrass Priorities')
source('Functions_SGPriorites.R')
source('Base_params.R')

nrand.choices2 <- 101 #make an odd number, so there is a median scenario

#New settlement data
dat$boats <- yachts$boats

# write.csv(dat, 'Check dataframe.csv', row.names=F)
### Set the trawl impact levels

nruns = 3 #3 different levels of trawl impact
scnrname <- c('Overlap','Intermediate','Avoid')

trawl.area = 0.01
Tcorr <- c(1.5, 1, 1/2) #0.4 and 3 times as likely to trawl same area
#This is area swept by each trawl. Middle value is the 'best guess'

tnames <- c('Overlapping','Intermediate','Avoiding') #names for timpact parameters
Cnames <- c('Cost Effectiveness','Seagrass Cover','Threat level', 'Threat level + cover', 'Avoided loss')

nschemes <- length(Cnames)

ActionNames <- c('Nothing','Reefs','Moorings','Both')
nmethods <- 3 #number of action types, excluding doing nothing

##### Make a graph, comparing trawl impacts at the three levels

trawlmin =0
# trawlmax = max(dat$trawlyears)
trawlmax =1000 #I suggest you use something less than the absolute maximum
nvals = 100
trawlvals = seq(trawlmin, trawlmax, length = nvals)
lvals=c(4,1,3)

histbreaks <- c(seq(0, trawlmax,by = 100), max(dat$trawlyears))
histvals <- hist(dat$trawlyears[dat$trawlyears>0], plot=F, breaks = histbreaks)
histdens <- histvals$counts/sum(histvals$counts)

if (plottrawl==T){

dev.new(width = 7, height = 6) #change to windows() on a windows machine
par(mar = c(5,6,4,2))
plot(0,0, type = 'n', xlim = c(trawlmin, trawlmax), ylim = c(0,1), xlab = 'No. trawl events over time frame',ylab = paste('Prop. of planning unit swept /','\n','Frequency of trawl events (proportions)'), las=1)

#Plots lines for each trawl event
for (i in 1:nruns){ 
	trawl.pres = pnbinom(trawlvals,size = Tcorr[i], prob = trawl.area)
	lines(trawlvals, trawl.pres, lwd = 3, lty =lvals[i])
	}
#### Add a line for the density
 lines(histvals$mids, histdens, lwd =3, col = 'grey60')

legend(trawlmax*0.5, 0.8, legend = c(tnames, paste('Frequency of','\n',' trawl events', sep='')), lty = c(lvals,1), lwd =3, cex = 1.2, col = c(rep('black',nruns), 'grey60'))

}

########################
###### Run prioritisation, varying trawling impact

#Preallocate dataframes
costs = data.frame(matrix(NA, nrow = nschemes, ncol = nruns))
row.names(costs) = Cnames
names(costs) = tnames

nactions = data.frame(matrix(NA, nrow = nschemes, ncol = nruns))
row.names(nactions) = Cnames
names(nactions) = tnames

#Number of actions by there type
nreefs = data.frame(matrix(NA, nrow = nschemes, ncol = nruns))
row.names(nreefs) = Cnames
names(nreefs) = tnames

nmoors = data.frame(matrix(NA, nrow = nschemes, ncol = nruns))
row.names(nmoors) = Cnames
names(nmoors) = tnames

nboth = data.frame(matrix(NA, nrow = nschemes, ncol = nruns))
row.names(nboth) = Cnames
names(nboth) = tnames



for (iruns in 1:nruns){
	# i = 2
	#Recalculate trawling impact
	trawl.pres = pnbinom(dat$trawlyears, size = Tcorr[iruns], prob = trawl.area)
	#Calculate trawling impacts, assuming geometric distribution (ie random locations of trawls)

	# boxplot(trawl.pres~dat$sg)
	# boxplot(dat$settle~dat$sg)
	# tapply(dat$settle, dat$sg, mean)

	#
	# NEW PRIORITISATION CODE
	#
	
	xres <- ModelImpacts(dat, N, reef.cost, moor.cost, trawl.pres, nreefsPU)
	
	##Other priority schemes
	ThreatVals <- xres$total.pres
	ThreatVals[dat$sg==0] <- 0
	SGvals <- dat$sg
	
	#
	#Prioritisation algorithm - CE and avoided loss
	#
	resultsCE <- prioritiesCE(propgoal, dat, N, nmethods,xres)
	resultsAloss <- prioritiesAloss(propgoal, dat, N, nmethods,xres)

	#Make csv
	 write.csv(ActionNames[resultsCE$best.actions[,2]], paste(foldername, 'Actions_CE_Trawl',tnames[iruns],'_settle',settlenum,'.csv', sep=''), row.names=F)
	 write.csv(ActionNames[resultsAloss$best.actions[,2]],  paste(foldername, 'Actions_ALoss_Trawl',tnames[iruns],'_settle',settlenum,'.csv', sep=''), row.names=F)
	
	#
	# Prioritisation algorithm - other schemes
	#
	
	DatSG <- NULL
	DatThreatNaive <- NULL
	DatThreat <- NULL
	
	for (irand in 1: nrand.choices2){
		Dnew <- prioritiesOther(ThreatVals,propgoal, dat, N, nmethods, xres, UseNaive=T)
		DatThreatNaive <- c(DatThreatNaive, list(Dnew))
		Dnew <- prioritiesOther(ThreatVals, propgoal, dat, N, nmethods, xres)
		DatThreat <- c(DatThreat, list(Dnew))
		Dnew <- prioritiesOther(SGvals, propgoal, dat, N, nmethods, xres)
		DatSG <- c(DatSG, list(Dnew))
	}
	
	sgcost <- unlist(lapply(DatSG, getCosts))
	threatcost.naive <-unlist(lapply(DatThreatNaive, getCosts))
	threatcost <- unlist(lapply(DatThreat, getCosts))
	
		#Save median scenarios
	iscnrSG <- which(sgcost == median(sgcost))[1]
	 write.csv(ActionNames[DatSG[[iscnrSG]]$best.actions[,2]],  paste(foldername, 'Actions_SG_Trawl',tnames[iruns],'_settle',settlenum,'.csv', sep=''), row.names=F)
		
	iscnrT <- which(threatcost == median(threatcost))[1]
	write.csv(ActionNames[DatThreat[[iscnrT]]$best.actions[,2]],  paste(foldername, 'Actions_Threat_Trawl',tnames[iruns],'_settle',settlenum,'.csv', sep=''), row.names=F)

	iscnrTN <- which(threatcost.naive == median(threatcost.naive))[1]
	write.csv(ActionNames[DatThreatNaive[[iscnrTN]]$best.actions[,2]],  paste(foldername, 'Actions_ThreatNaive_Trawl',tnames[iruns],'_settle',settlenum,'.csv', sep=''), row.names=F)

	#
	# STORE RESULTS FOR PLOTTING
	#

	costs[1, iruns] <- resultsCE$cost
	costs[2, iruns] <- median(sgcost)
	costs[3, iruns] <- median(threatcost.naive)
	costs[4, iruns] <- median(threatcost)
	costs[5, iruns] <- resultsAloss$cost
	print(unique(threatcost/resultsCE$cost))
	
	nreefs[1, iruns] <- resultsCE$nreefsact
	nreefs[2, iruns] <- DatSG[[iscnrSG]]$nreefsact
	nreefs[3, iruns] <- DatThreatNaive[[iscnrTN]]$nreefsact
	nreefs[4, iruns] <- DatThreat[[iscnrT]]$nreefsact
	nreefs[5, iruns] <- resultsAloss$nreefsact

	nmoors[1, iruns] <- resultsCE$nmoorsact
	nmoors[2, iruns] <- DatSG[[iscnrSG]]$nmoorsact
	nmoors[3, iruns] <- DatThreatNaive[[iscnrTN]]$nmoorsact
	nmoors[4, iruns] <- DatThreat[[iscnrT]]$nmoorsact
	nmoors[5, iruns] <- resultsAloss$nmoorsact	

	nactions[1, iruns] <- resultsCE$naction
	nactions[2, iruns] <- DatSG[[iscnrSG]]$naction
	nactions[3, iruns] <- DatThreatNaive[[iscnrTN]]$naction
	nactions[4, iruns] <- DatThreat[[iscnrT]]$naction
	nactions[5, iruns] <- resultsAloss$naction
	
}

#
# SAVE RESULTS AS TABLES
#

write.csv(costs,  paste(foldername2, 'Costs_settle',settlenum,'.csv', sep=''), row.names=F)
write.csv(nreefs,  paste(foldername2, 'Nreefs_settle',settlenum,'.csv', sep=''), row.names=F)
write.csv(nmoors,  paste(foldername2, 'Nmoors_settle',settlenum,'.csv', sep=''), row.names=F)
write.csv(nactions,  paste(foldername2, 'NPUs_with_actions_settle',settlenum,'.csv', sep=''), row.names=F)


#
# PLOT RELATIVE COSTS
#
myscnrs <- c(1,2,3,5)

results = 100*as.matrix(costs[myscnrs,] / costs[1,2])
# results = 100*as.matrix(costs / costs[1,2])

mycols = brewer.pal(nschemes, 'Greys')
mycols <- mycols[c(4, 1, 2,3)]

dev.new(width = 7, height = 6)
par(mar = c(5,6,4,2))
barplot(results, beside=T, ylab = 'Cost (% of base scenario)', names.arg = tnames, cex.lab=1.5, las=1, cex.names=1.4, cex.axis = 1.3, col = mycols, xlab = 'Scenarios for trawling overlap', ylim = c(0,700), main = '')

legend('topleft', legend = Cnames[myscnrs], col = mycols, pch =15, cex = 1.2, bty='n')
points(0.943, 622.68, pch = 0, cex = 1.1)


#
# PLOT COSTS
#

results.abs = as.matrix(costs[myscnrs,]/1E6)

dev.new(width = 7, height = 6)
par(mar = c(5,6,4,2))
barplot(results.abs, beside=T, ylab = 'Cost (000,000\'s \u20ac)', names.arg = tnames, cex.lab=1.5, las=1, cex.names=1.4, cex.axis = 1.3, col = mycols, xlab = 'Scenarios for trawling overlap', ylim = c(0,30), main = '')

legend('topleft', legend = Cnames[myscnrs], col = mycols, pch =15, cex = 1.2, bty='n')








