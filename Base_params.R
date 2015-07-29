#######################################
###### PARAMETERS AND DATA
#######################################
# For Giakoumi et al. 2015 Marine Policy 61, 95-102
### 29 July 2015
####

#### Read in data
dat = read.csv(ThreatsFile, header = T)
yachts = read.csv('boat_estimation.csv', header = T)
head(dat)
N = nrow(dat) #number of PUs

###Costs of reefs and moorings per PU
moor.cost = 3300 #euros, cost per category
reef.cost = 475200 #euros
nreefsPU <- 66 #number reefs to cover a PU

##### Goal for seagrass protection
propgoal = 0.65 #proportion of current seagrass cover
years = 20 #management time-frame

##### Budget goal for seagrass protection
budget = 100000 #euros

#### Unstoppable threat thresholds
#Seagrass in areas with these levels or greater is lost
farm.thresh = 1
settle.thresh = 2132

##### Trawl impact
dat$trawlyears = dat$trawl*years
trawl.area <- 0.01
trawl.pres = pgeom(dat$trawlyears, trawl.area)
#Calculate trawling impacts, assuming geometric distribution (ie random locations of trawls)

######## Area swept by highest yatchs category
yacht.daysperseason = 4 * 30 #days of trawling per season
yacht.maxpropswept =  0.0000000011* yacht.daysperseason* years #% of  area in the time period (ie maximum proportion of a PU) that one boat can impact
max.yacht.impact=1.0