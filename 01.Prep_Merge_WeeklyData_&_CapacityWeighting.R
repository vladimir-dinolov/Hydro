############################################################

packages <- c("plyr", "dplyr", "tidyr", "lubridate", "zoo")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# remove 'packages', 'package.check' after loading the required packages.
rm(packages)
rm(package.check)

############################################################
# Merge the Outage Data, Natural Flow, Load, and Market Variables

outage1125 <- read.csv(file="outage_Results_20191125.csv", header=TRUE, sep=",", na=c("","NA"))
outage.uo <- read.csv(file="forced_combined_20191130.csv", header=TRUE, sep=",", na=c("","NA"))

outage1130 <- cbind(outage1125, outage.uo[,-1]) 

str(outage1130)
str(market.hr)

market.hr <- read.csv(file="market_hourly_data.csv", header=TRUE, sep=",", na=c("","NA"))
str(market.hr)

outage1130$dateTime <- gsub("T", " ", outage1130$dateTime)
outage1130$dateTime <- gsub("Z", "", outage1130$dateTime)

outage1130$dateTime <- as.POSIXct(outage1130$dateTime, format = "%Y-%m-%d %H:%M")
market.hr$Time <- as.POSIXct(market.hr$Time, format = "%Y-%m-%d %H:%M")

outage <- left_join(outage1130, market.hr, by=c("dateTime"="Time"))
str(outage)

outage <- outage[ ,-which(names(outage)=="X")]
outage <- outage[-which(outage$year==1999), ]
outage <- outage[-which(outage$year==2019), ]

# convert R_25hrAvg & R_24hrVariability to numeric
outage$R_24hrAvg  <- as.numeric(outage$R_24hrAvg)
outage$R_24hrVariability  <- as.numeric(outage$R_24hrVariability)

# write out the combined outage data from 2000-2019
write.csv(outage, file = "outage20191130.csv")

rm(market.hr)
rm(outage.uo)
rm(outage1125)
rm(outage1130)

############################################################
# Convert the data into weekly
# strftime(outage$dateTime, format = "%Y-%V")
# strftime(outage$dateTime, format = "%V")

# outage$Week <- paste(strftime(outage$dateTime, format = "%Y-%m"), strftime(outage$dateTime, format = "%V"), sep="/")
# outage <- outage[ ,c(1,5,3,4,2,67,6:66)]

outage$Week <- paste(strftime(outage$dateTime, format = "%Y"), strftime(outage$dateTime, format = "%U"), sep="/")
outage <- outage[ ,c(1,5,3,4,2,67,6:66)]

str(outage)

which(outage$Week)

strftime("2000-01-08", format = "%U")
strftime("2000-12-31", format = "%U")

strftime("2000-01-01", format = "%V")
strftime("2000-12-31", format = "%V")

# split planned outage and forced outage to separate data sets to compute impacts
outage.po <- outage[ ,which(names(outage)=="R41_Status_Planned"):which(names(outage)=="B56_Status_Planned")]
outage.uo <- outage[ ,which(names(outage)=="R41_Status_Forced"):which(names(outage)=="B56_Status_Forced")]

str(outage.po)
str(outage.uo)


########################################################################################
#### weekly data of nat-flow

# combine R & B NatFlow by adding up
NatFlow <- data.frame(outage$dateTime, outage$Week, outage$R_NatFlow + outage$B_NatFlow, outage$R_24hrAvg + outage$B_24hrAvg)
names(NatFlow) <- c("dateTime", "Week", "NatFlow", "Avg24hr")

# compute weekly average nat flow
avgNatFlow.wk <- data.frame(tapply(NatFlow$Avg24hr, NatFlow$Week, mean))
avgNatFlow.wk$week <- row.names(avgNatFlow.wk)
names(avgNatFlow.wk) <- c("avgNatFlow.wk", "week")

# compute average weekly R_NatFlow per year
avgNatFlow.wk$week2 <- avgNatFlow.wk$week
avgNatFlow.wk <- separate(avgNatFlow.wk, week2, sep="-", into=c("year", "mo/wk"))

avg_wkNatFlow <- data.frame(tapply(avgNatFlow.wk$avgNatFlow.wk, avgNatFlow.wk$year, mean, na.rm=TRUE))
avg_wkNatFlow$year <- rownames(avg_wkNatFlow)
names(avg_wkNatFlow) <- c("avg_wkNatFlow_yr","year")

avgNatFlow.wk <- left_join(avgNatFlow.wk, avg_wkNatFlow, by=c("year"="year"))
avgNatFlow.wk$NatFlowVari.wk <- (avgNatFlow.wk$avgNatFlow.wk - avgNatFlow.wk$avg_wkNatFlow_yr) / avgNatFlow.wk$avg_wkNatFlow_yr

# remove last NaN record
avgNatFlow.wk <- avgNatFlow.wk[-1190, ]

summary(avgNatFlow.wk)


########################################################################################
#### weekly data of load

# take the load data only
Load <- outage[ ,c(1,6,13:15)]
str(Load)

# compute weekly average load
avgLoad.wk <- data.frame(tapply(Load$Load_24hrAvg, Load$Week, mean, na.rm=TRUE))
avgLoad.wk$week <- row.names(avgLoad.wk)
names(avgLoad.wk) <- c("avgLoad.wk", "week")

# compute average weekly load per year
avgLoad.wk$week2 <- avgLoad.wk$week
avgLoad.wk <- separate(avgLoad.wk, week2, sep="-", into=c("year", "mo/wk"))

avg_wkLoad <- data.frame(tapply(avgLoad.wk$avgLoad.wk, avgLoad.wk$year, mean, na.rm=TRUE))
avg_wkLoad$year <- rownames(avg_wkLoad)
names(avg_wkLoad) <- c("avg_wkLoad_yr","year")

avgLoad.wk <- left_join(avgLoad.wk, avg_wkLoad, by=c("year"="year"))
avgLoad.wk$LoadVari.wk <- (avgLoad.wk$avgLoad.wk - avgLoad.wk$avg_wkLoad_yr) / avgLoad.wk$avg_wkLoad_yr

# remove last NaN record
avgLoad.wk <- avgLoad.wk[-1190, ]

summary(avgLoad.wk)


########################################################################################
#### weekly market data

# read in the hourly market data
market.hr <- read.csv(file="market_hourly_data.csv", header=TRUE, sep=",", na=c("","NA"))

# get the week variable
market.hr$Week <- paste(strftime(market.hr$Time, format = "%Y-%m"), strftime(market.hr$Time, format = "%V"), sep="/")
market.hr$Week2 <- market.hr$Week
market.hr <- separate(market.hr, Week2, sep="-", into=c("year", "mo/wk"))

# remove records in 1999 and 2019
market.hr <- market.hr[c(-which(market.hr$year==1999),-which(market.hr$year==2019)),]

str(market.hr)

# get the get average Net per week
avgNet.wk <- data.frame(tapply(market.hr$netTotal.hr, market.hr$Week, mean))
avgNet.wk$week <- rownames(avgNet.wk)
names(avgNet.wk) <- c("avgNet.wk", "week")

avgNet.wk$week2 <- avgNet.wk$week
avgNet.wk <- separate(avgNet.wk, week2, sep="-", into=c("year", "mo/wk"))

# get average weekly Net per year
avg_wkNet <- data.frame(tapply(avgNet.wk$avgNet.wk, avgNet.wk$year, mean))
avg_wkNet$year <- rownames(avg_wkNet)
names(avg_wkNet) <- c("avg_wkNet", "year") 

# left_join weekly Net per year to average Net per week for variability
avgNet.wk <- left_join(avgNet.wk, avg_wkNet, by=c("year"="year"))
avgNet.wk$NetVari.wk <- (avgNet.wk$avgNet.wk - avgNet.wk$avg_wkNet) / avgNet.wk$avg_wkNet



#######################################################################################
# unit capacity weighting & normalization
# read in unit capacity data
unitCapacity <- read.csv(file="Unit Capacity.csv", header=T, sep=",", na=c("","NA"))

# take out D33, D34, G20 from unitCapacity, since these units are not included in the outage data set
unitCapacity <- unitCapacity[-7:-9, ]
str(unitCapacity)

unitCapacity[, 1:4]
sum(unitCapacity$W1)

# create weight percenatge per each unit's capacity, with 2 methods
# 1) proportion to the average capacity of the 16 units
# 2) proportion to total capacity of 16 units
unitCapacity$W1 <- unitCapacity$Capacity/mean(unitCapacity$Capacity)
unitCapacity$W2 <- unitCapacity$Capacity/sum(unitCapacity$Capacity)

outIndex.w1 <- sum(unitCapacity$W1)
outIndex.w2 <- sum(unitCapacity$W2)

unitImpacts.po <- data.frame(t(t(outage.po) * unitCapacity$W1))
unitImpacts.po$poImpact <- rowSums(unitImpacts.po)

unitImpacts.uo<- data.frame(t(t(outage.uo) * unitCapacity$W1))
unitImpacts.uo$uoImpact <- rowSums(unitImpacts.uo)

str(unitImpacts.po)
summary(unitImpacts.po)
str(unitImpacts.uo)
summary(unitImpacts.uo)

# lapply(outage.uo, function(x) length(which(is.na(x))))

#### create weight percentages for units in Ross group and Boundary group respectively
unitCapacity.r <- unitCapacity[1:10, 1:3]
unitCapacity.b <- unitCapacity[11:16, 1:3]

unitCapacity.r$W1 <- unitCapacity.r$Capacity/mean(unitCapacity.r$Capacity)
unitCapacity.b$W1 <- unitCapacity.b$Capacity/mean(unitCapacity.b$Capacity)

outIndex.rw1 <- sum(unitCapacity.r$W1)
outIndex.bw1 <- sum(unitCapacity.b$W1)

# unit impact of Ross group
unitImpacts.rpo<- data.frame(t(t(outage.po[,1:10]) * unitCapacity.r$W1))
unitImpacts.rpo$poImpact.r <- rowSums(unitImpacts.rpo)

unitImpacts.ruo<- data.frame(t(t(outage.uo[,1:10]) * unitCapacity.r$W1))
unitImpacts.ruo$uoImpact.r <- rowSums(unitImpacts.ruo)

# unit impact of Boundary group
unitImpacts.bpo<- data.frame(t(t(outage.po[,11:16]) * unitCapacity.b$W1))
unitImpacts.bpo$poImpact.b <- rowSums(unitImpacts.bpo)

unitImpacts.buo<- data.frame(t(t(outage.uo[,11:16]) * unitCapacity.b$W1))
unitImpacts.buo$uoImpact.b <- rowSums(unitImpacts.buo)

########################################################################################
#### hourly data of outage impact
impact.bndry <- cbind(unitImpacts.bpo, unitImpacts.buo)
impact.bndry <- impact.bndry[ ,c(-8,-16)]
impact.bndry$dateTime <- outage$dateTime
impact.bndry <- impact.bndry[ ,c(15,1:14)]

impact.ross <- cbind(unitImpacts.rpo, unitImpacts.ruo)
impact.ross <- impact.ross[ ,c(-12,-24)]
impact.ross$dateTime <- outage$dateTime
impact.ross <- impact.ross[ ,c(23,1:22)]

str(impact.bndry)
str(impact.ross)

# write out the weekly data
write.csv(impact.bndry, file = "outage_imapct_bndry_hr.csv")
write.csv(impact.ross, file = "outage_imapct_ross_hr.csv")

########################################################################################
#### weekly data of outage impact

# compute weekly overall impact of planned outage from all units 
unitImpacts.po$week <- outage$Week
wkImpact.po <- data.frame(tapply(unitImpacts.po$poImpact, unitImpacts.po$week, mean))
wkImpact.po$week <- rownames(wkImpact.po)
names(wkImpact.po) <- c("wkImpact.po", "week")

# compute weekly overall impact of forced outage from all units 
unitImpacts.uo$week <- outage$Week
wkImpact.uo <- data.frame(tapply(unitImpacts.uo$uoImpact, unitImpacts.uo$week, mean))
wkImpact.uo$week <- rownames(wkImpact.uo)
names(wkImpact.uo) <- c("wkImpact.uo", "week")

# remove last NaN record
wkImpact.po <- wkImpact.po[-1190, ]
wkImpact.uo <- wkImpact.uo[-1190, ]

str(wkImpact.po)
str(wkImpact.uo)


#### weekly data of outage impact, in ross and boundary groups

# compute weekly overall impact of planned & forced outage for boundary group 
unitImpacts.bpo$week <- outage$Week
wkImpact.bpo <- data.frame(tapply(unitImpacts.bpo$poImpact.b, unitImpacts.bpo$week, mean))
wkImpact.bpo$week <- rownames(wkImpact.bpo)
names(wkImpact.bpo) <- c("wkImpact.bpo", "week")

unitImpacts.buo$week <- outage$Week
wkImpact.buo <- data.frame(tapply(unitImpacts.buo$uoImpact.b, unitImpacts.buo$week, mean))
wkImpact.buo$week <- rownames(wkImpact.buo)
names(wkImpact.buo) <- c("wkImpact.buo", "week")

# compute weekly overall impact of planned & forced outage for ross group 
unitImpacts.rpo$week <- outage$Week
wkImpact.rpo <- data.frame(tapply(unitImpacts.rpo$poImpact.r, unitImpacts.rpo$week, mean))
wkImpact.rpo$week <- rownames(wkImpact.rpo)
names(wkImpact.rpo) <- c("wkImpact.rpo", "week")

unitImpacts.ruo$week <- outage$Week
wkImpact.ruo <- data.frame(tapply(unitImpacts.ruo$uoImpact.r, unitImpacts.ruo$week, mean))
wkImpact.ruo$week <- rownames(wkImpact.ruo)
names(wkImpact.ruo) <- c("wkImpact.ruo", "week")

# remove last NaN record
wkImpact.bpo <- wkImpact.bpo[-1190, ]
wkImpact.rpo <- wkImpact.rpo[-1190, ]
wkImpact.buo <- wkImpact.buo[-1190, ]
wkImpact.ruo <- wkImpact.ruo[-1190, ]

# cbind planned outage impact for ross and boundary groups respectively
wkImpact <- data.frame(wkImpact.po[ ,c(2,1)], wkImpact.rpo[ ,1], wkImpact.bpo[ ,1], wkImpact.uo[ ,1], wkImpact.ruo[ ,1], wkImpact.buo[ ,1])
names(wkImpact) <- c("week", "wkImpact.po", "wkImpact.po.r", "wkImpact.po.b","wkImpact.uo", "wkImpact.uo.r", "wkImpact.uo.b")

# write out the weekly outage impact
write.csv(wkImpact, file = "outage_impact_wk.csv")

########################################################################################
#### merge to create the weekly data

outage_wk <- data.frame(avgNatFlow.wk[ ,c(3,4,2,1,6)], avgLoad.wk[ ,c(1,6)], avgNet.wk$avgNet.wk, avgNet.wk$NetVari.wk, wkImpact.po[,1], wkImpact.uo[,1])
str(outage_wk)

# re-order the rows
outage_wk <- outage_wk[c(-6,-1184), ]


# write out the weekly data
write.csv(outage_wk, file = "outage_weekly.csv")
