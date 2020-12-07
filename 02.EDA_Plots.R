############################################################
packages <- c("readr","tidyr","lubridate","ggplot2","scales","zoo")
packages <- c("tm","stringr", "wordcloud", "caret", "readr", "dplyr", "tidyr", "datasets", "ggplot2", "scales", "arules","arulesViz","lubridate")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# remove 'packages', 'package.check' after loading the required packages.
rm(packages)
rm(package.check)


########################################################

# create time serie plot for average values
plot.avg <- outage_wk[ ,c(-2,-5,-7,-9)]
plot.avg$impactx <- plot.avg$wkImpact.po...1. * plot.avg$wkImpact.uo...1. * 100
plot.avg$avgLoad.wkx <- plot.avg$avgLoad.wk * 10
plot.avg <- plot.avg[ ,c(-4,-6:-7)]
names(plot.avg) <- c("year","week","natrual.flow", "net.transactions", "scaled.impact", "scaled.load" )

########################################################
# convert week to date format
plot.avg$week <- gsub("/", "-", plot.avg$week)
as.Date(plot.avg$week, format="%Y-%m-%v")
as.POSIXct(plot.avg$week, format="%Y-%m-%v")

plot.avg$week2 <- plot.avg$week
plot.avg <- separate(plot.avg, week2, sep="-", into=c("year", "month", "wk"))

plot.avg$year.wk <- paste(plot.avg$year, plot.avg$wk, sep="-")
as.POSIXct(plot.avg$year.wk, format="%Y-%v")

as.POSIXct(x="2000-01-01", format="%Y-%m-%v")
as.POSIXct(x="2000-01", format="%Y-%v")


########################################################
# # # # # code to run after data prep of outage_clustering.R

# use the data prepared - outageVal_mo
str(outageVal_mo)

# put all values into one column
plot.avg <- gather(outageVal_mo,"yearMonth","Flow.Monthly","Load_Scaled.Monthly","NetTrans.Monthly","HydroCapacity_Scaled.Monthly", key="Value.Type", value="Value",-yearMonth)

ggplot(plot.avg, aes(x=yearMonth, group=Value.Type, color=Value.Type)) + geom_line(aes(y=Value), alpha=0.8) + 
  labs(x="Year", y="Values") + scale_x_date(date_breaks="1 year", date_labels="%Y") +
  ggtitle("Monthly Average Values of Flow, Load, Transaction, & Hydro Capacity (2000-2018)") 



# use the data prepared - outageVari_mo
str(outageVari_mo)

# put all values into one column
plot.vari <- gather(outageVari_mo,"FlowVari.Monthly","LoadVari.Monthly","NetTransVari.Monthly","HydroCapacity.Monthly",key="Varibility.Type",value="Value",-yearMonth)
str(plot.vari)

ggplot(plot.vari, aes(x=yearMonth, group=Varibility.Type, color=Varibility.Type)) + geom_line(aes(y=Value), alpha=0.8) + 
  labs(x="Year", y="Values") + scale_x_date(date_breaks="1 year", date_labels="%Y") +
  ggtitle("Monthly Average Varibilities of Flow, Load, Transaction, & Hydro Capacity (2000-2018)") 


########################################################
# # # # # averages values of variables by month

########################################################
#### Natural Flow - monthly average line chart
names(NatFlow.mo) <- "value"
NatFlow.mo$date <- rownames(NatFlow.mo)

# convert year.mo to Date format
NatFlow.mo$date <- as.Date(as.yearmon(NatFlow.mo$date))

str(NatFlow.mo)

ggplot(NatFlow.mo, aes(x=date)) + geom_line(aes(y=value), size=0.75, color="steelblue") + 
  labs(x="timeline", y="cubit foot per second") + scale_x_date(date_breaks="1 year", date_labels="%Y") +
  ggtitle("Monthly Average of Natural Flow (2000-2018)") 

#### Natural Flow - yearly average bar chart

NatFlow.mo$date2 <- NatFlow.mo$date
NatFlow.mo <- separate(NatFlow.mo, date2, sep="-", into=c("year", "mo", "wk"))

NatFlow.yr <- data.frame(tapply(NatFlow.mo$value, NatFlow.mo$year, mean))
names(NatFlow.yr) <- "value"

NatFlow.yr$year <- rownames(NatFlow.yr)

ggplot(NatFlow.yr, aes(x=year, y=value)) + geom_bar(stat="identity", size=0.75, fill="steelblue") + 
  labs(x="timeline", y="cubit foot per second") + ggtitle("Annual Average of Natural Flow (2000-2018)")




########################################################
#### Load - monthly average line chart
names(Load.mo) <- "value"
Load.mo$date <- rownames(Load.mo)

# convert year.mo to Date format
Load.mo$date <- as.Date(as.yearmon(Load.mo$date))

str(Load.mo)

ggplot(Load.mo, aes(x=date)) + geom_line(aes(y=value), size=0.75, color="lightcoral") + 
  labs(x="timeline", y="mega watts per hour") + scale_x_date(date_breaks="1 year", date_labels="%Y") +
  ggtitle("Monthly Average of Load (2000-2018)") 


#### Load - yearly average bar chart

Load.mo$date2 <- Load.mo$date
Load.mo <- separate(Load.mo, date2, sep="-", into=c("year", "mo", "wk"))

Load.yr <- data.frame(tapply(Load.mo$value, Load.mo$year, mean))
names(Load.yr) <- "value"

Load.yr$year <- rownames(Load.yr)

ggplot(Load.yr, aes(x=year, y=value)) + geom_bar(stat="identity", size=0.75, fill="lightcoral") + 
  labs(x="timeline", y="mega watts per hour") + ggtitle("Annual Average of Load (2000-2018)")





########################################################
#### NetTrans - monthly average line chart
names(NetTrans.mo) <- "value"
NetTrans.mo$date <- rownames(NetTrans.mo)

# convert year.mo to Date format
NetTrans.mo$date <- as.Date(as.yearmon(NetTrans.mo$date))

str(Load.mo)

ggplot(NetTrans.mo, aes(x=date)) + geom_line(aes(y=value), size=0.75, color="tan") + 
  labs(x="timeline", y="dollars") + scale_x_date(date_breaks="1 year", date_labels="%Y") +
  ggtitle("Monthly Average of Net Transactions (2000-2018)") 


#### Load - yearly average bar chart

NetTrans.mo$date2 <- NetTrans.mo$date
NetTrans.mo <- separate(NetTrans.mo, date2, sep="-", into=c("year", "mo", "wk"))

NetTrans.yr <- data.frame(tapply(NetTrans.mo$value, Load.mo$year, mean))
names(NetTrans.yr) <- "value"

NetTrans.yr$year <- rownames(NetTrans.yr)

ggplot(NetTrans.yr, aes(x=year, y=value)) + geom_bar(stat="identity", size=0.75, fill="tan") + 
  labs(x="timeline", y="dollars") + ggtitle("Annual Average of Net Transactions (2000-2018)")




########################################################
#### Unit Capacities charts

#### Capacity comparison
Capacity <- read.csv(file="Unit Capacity.csv", header=T, sep=",", na=c("","NA"))

Capacity$CodeName <- gsub("R","Ross-",Capacity$CodeName)
Capacity$CodeName <- gsub("D","Diablo-",Capacity$CodeName)
Capacity$CodeName <- gsub("G","Gorge-",Capacity$CodeName)
Capacity$CodeName <- gsub("B","Boundary-",Capacity$CodeName)

Capacity <- separate(Capacity, CodeName, sep="-", into=c("Plant", "Unit"))

ggplot(data = Capacity, aes(x=Plant, y=Capacity, fill=Unit)) + geom_bar(stat="identity") +
  geom_text(aes(label = Capacity), size=3, color="gray10", position = position_stack(vjust=0.5)) +
  labs(x="Plant & Unit", y="Capacity") + ggtitle("Comparison of Capacities") +
  theme(plot.title = element_text(hjust = 0.5))




#### counts and duration of outages
po <- read.csv(file="PO_all.csv", header=T, sep=",", na=c("","NA"))
uo <- read.csv(file="UO_all.csv", header=T, sep=",", na=c("","NA"))


length(po$Event..)
length(uo$Event..)
po.days <- round(sum(po$Days, na.rm = T) + (sum(po$Hours, na.rm=T) + sum(po$Minutes, na.rm=T)/60)/12)
uo.days <- round(sum(uo$Days, na.rm = T) + (sum(uo$Hours, na.rm=T) + sum(uo$Minutes, na.rm=T)/60)/12)

unitCt <- length(unique(po$Unit))

alldays <- as.numeric(difftime(as.Date("2018-12-31",'%Y-%m-%d'),as.Date("1999-01-01",'%Y-%m-%d'),units = c("days")))

po.ratio <- po.days/unitCt/alldays
uo.ratio <- uo.days/unitCt/alldays

outStats <- data.frame(length(po$Event..), length(uo$Event..), po.days, uo.days, po.ratio, uo.ratio)
names(outStats) <- c("planned.counts", "forced.counts", "planned.days", "forced.days", "planned.ratio", "forced.ratio")
outStats <- data.frame(t(outStats))

outStats$type <- rownames(outStats)
names(outStats) <- c("value", "type")

outStats
str(outStats)

#### plot outage count comparison
ggplot(outStats[1:2,], aes(x=type, y=value)) + geom_bar(stat="identity", fill="tan") + 
  labs(x="outage type", y="counts") +  geom_text(aes(label = value), size=3.5, color="black", vjust=2) +
  ggtitle("Total Outage Counts (1999-2018)")

#### plot outage days comparison
ggplot(outStats[3:4,], aes(x=type, y=value)) + geom_bar(stat="identity", fill="tan") + 
  labs(x="outage type", y="days") +  geom_text(aes(label = value), size=3.5, color="black", vjust=2) +
  ggtitle("Total Outage Days (1999-2018)")

#### plot outage ratio comparison
ggplot(outStats[5:6,], aes(x=type, y=value)) + geom_bar(stat="identity", fill="tan") + 
  labs(x="outage type", y="ratio") +  geom_text(aes(label = paste0(round(value*100),"%")), size=3.5, color="black", vjust=2) +
  scale_y_continuous(labels = percent) +  ggtitle("Outage Ratios per Unit (1999-2018)")
