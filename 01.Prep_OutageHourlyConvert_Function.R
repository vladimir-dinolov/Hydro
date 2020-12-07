# outage cleaning algorithm from start to end date to hourly data

# read outage data
library(lubridate)
library(data.table)
library(tidyverse)

setwd("/Users/svillarreal/Desktop/Data Analytics/Project/")
dfMain <- read.csv("UO - Boundary.csv")

# change chr to datetime

dfMain$Start <- as.POSIXct(dfMain$Start, format = "%m/%d/%Y %H:%M")
dfMain$End <- as.POSIXct(dfMain$End, format = "%m/%d/%Y %H:%M")
str(dfMain)

str(dfMain$Start)

#check if the end of one event starts at the same start of the next event
# if so add one hour to the start event

for (i in 1:(nrow(dfMain)-1)){
  if (dfMain$End[i] - dfMain$Start[i+1]<= 2){ #dfMain$End[i] == dfMain$Start[i+1]
    dfMain$Start[i+1] <- floor_date(dfMain$Start[i+1],"hour") + (1*(60*60))
  }
}



#df1 <- subset(df, rownames(Unit) %in% "Ross, Unit 41")
df <- dfMain[which(dfMain$Unit == 'Boundary, Unit 55' & dfMain$Description != "RESERVE SHUTDOWN"
                   & dfMain$Hours >=3),]


v2 <- as.POSIXct("2019-01-01 00:00:00 PDT")
str(v2)

if(df$End[nrow(df)] == v2){
  df$End[nrow(df)] <- floor_date(df$End[nrow(df)],"hour") +(1*(60*60))
}

#seq(as.POSIXct("1912-02-24 23:00:00"), as.POSIXct("1912-02-25 08:32:00"), by="hour")
#as.POSIXct(df$Start[1], format = "%m/%d/%Y %H:%M")

v1 <- as.POSIXct("1999-01-01 01:00:00 PDT")
str(v1)

df$StartAvail <- floor_date(df$End,"hour") + (60*60) #set when unit is available
df$EndAvail <- floor_date(df$Start,"hour") - (60*60) #set when unit stops being available



# make sure unit is avaiable on 01/01/1999 hour 1 if the unit is actually available
if (floor_date(df$Start[1], "hour") == v1) {
  #v1 <- 1
  new_df <- data.frame(floor_date(seq(df$Start[1], df$End[1], by="hour"), "hour"))
  colnames(new_df) <- "dateTime"
  new_df$unitName <- df$Unit[1]
  new_df$Start <- df$Start[1]
  new_df$End <- df$End[1]
  new_df$outageNum <- df$Event..[1]
  new_df$outageFam <- df$Failure.Family[1]
  new_df$outageDesc <- df$Description[1]
  new_df$unitStatus <- 0 # this means the unit is not avaialable
  
} else {
  new_df <- data.frame(floor_date(seq(v1, df$EndAvail[1], by="hour"), "hour"))
  colnames(new_df) <- "dateTime"
  new_df$unitName <- df$Unit[1]
  new_df$Start <- df$Start[1]
  new_df$End <- df$End[1]
  new_df$outageNum <- df$Event..[1]
  new_df$outageFam <- df$Failure.Family[1]
  new_df$outageDesc <- df$Description[1]
  new_df$unitStatus <- 1 # this means the unit is available
  
  new_df1 <- data.frame(floor_date(seq(df$Start[1], df$StartAvail[1], by="hour"), "hour"))
  colnames(new_df1) <- "dateTime"
  new_df1$unitName <- df$Unit[1]
  new_df1$Start <- df$Start[1]
  new_df1$End <- df$End[1]
  new_df1$outageNum <- df$Event..[1]
  new_df1$outageFam <- df$Failure.Family[1]
  new_df1$outageDesc <- df$Description[1]
  new_df1$unitStatus <- 0 #this means the unit is not available
  new_df <- rbind(new_df, new_df1)
}

floor_date(df$StartAvail[1], "hour") # this is unit on or equal to 1
floor_date(df$EndAvail[2], "hour")

floor_date(df$Start[2], "hour") # this is unit off or equal to 0 
floor_date(df$End[2], "hour")


str(new_df)
colnames(df)


for(i in 1:(nrow(df)-1))
{
  df_Hourly <- data.frame(floor_date(seq(df$StartAvail[i], df$EndAvail[i+1], by="hour"), "hour"))
  colnames(df_Hourly) <- "dateTime"
  df_Hourly$unitName <- df$Unit[i]
  df_Hourly$Start <- df$StartAvail[i]
  df_Hourly$End <- df$EndAvail[i+1]
  df_Hourly$outageNum <- 0
  df_Hourly$outageFam <- "aa"
  df_Hourly$outageDesc <- "unitAvailable"
  df_Hourly$unitStatus <- 1 # this means the unit is available
  
  df_Hourly1 <- data.frame(floor_date(seq(df$Start[i+1], df$End[i+1], by="hour"), "hour"))
  colnames(df_Hourly1) <- "dateTime"
  df_Hourly1$unitName <- df$Unit[i+1]
  df_Hourly1$Start <- df$Start[i+1]
  df_Hourly1$End <- df$End[i+1]
  df_Hourly1$outageNum <- df$Event..[i+1]
  df_Hourly1$outageFam <- df$Failure.Family[i+1]
  df_Hourly1$outageDesc <- df$Description[i+1]
  df_Hourly1$unitStatus <- 0 #this means the unit is not available
  #df_Hourly1 <- rbind(new_df, new_df1)
  new_df <- rbind(new_df, df_Hourly, df_Hourly1)
}

# finish 2018 year if needed


v3 <- as.POSIXct("2019-01-01 00:00:00 PDT")
str(v3)

if (floor_date(df$End[nrow(df)], "hour") == v2) {
  
} else {
  df_Hourly2 <- data.frame(floor_date(seq(df$StartAvail[nrow(df)], v3, by="hour"), "hour"))
  colnames(df_Hourly2) <- "dateTime"
  df_Hourly2$unitName <- df$Unit[1]
  df_Hourly2$Start <- df$StartAvail[nrow(df)]
  df_Hourly2$End <- v3
  df_Hourly2$outageNum <- 0
  df_Hourly2$outageFam <- "aa"
  df_Hourly2$outageDesc <- "unitAvailable"
  df_Hourly2$unitStatus <- 1 # this means the unit is available
  
  new_df <- rbind(new_df, df_Hourly2)
}

df_check <- new_df
df_check <- df_check[-1,]
new_df$check <-(new_df$dateTime - df_check$dateTime)/(60*60)



check <- table(new_df$check)
check <- check[names(check)==-0]
check

for (i in 1:check){
  if (nrow(new_df) != 175320){
    indexNum <- match(0, new_df$check)
    newrow <- new_df[indexNum,]
    newrow$dateTime <-floor_date(newrow$dateTime,"hour") + (60*60)
    newrow$check <- -1
    new_df$check[indexNum]
    new_df<-rbind(new_df[1:indexNum,], newrow, new_df[-(1:indexNum),])
    new_df[indexNum,9] <- -1
  }
}







new_df <- new_df[!duplicated(new_df$dateTime),]
head(new_df, 10)
tail(new_df,10)
str(new_df$dateTime)
#fix dfMain
#test_dfMain <- dfMain

fwrite(new_df, "forced_boundaryUnit55.csv")
fwrite(df_genResults, "outage_summaryResults_11242019.csv")

