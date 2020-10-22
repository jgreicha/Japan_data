#Japan4
#clear workspace
rm(list = ls())

#packages
library(tidyverse)
library(readxl)
library(gtools)
library(votingpower)
#Reads the XLS File
Japan2<-read_excel("JapanClean2.xlsx")


# Determination of LDP Cannidates
Japan2$LDPCombined<-0
Japan2$JSPCombined<-0
Japan2$KOM<-0
Japan2$DSPCom<-0
Japan2$JCPCom<-0

Japan2$LDPCombined[which(Japan2$party_id==1 | Japan2$party_id==1.5)]<-1
Japan2$JSPCombined[which(Japan2$party_id==2 | Japan2$party_id==2.5)]<-1
Japan2$KOM[which(Japan2$party_id==3 | Japan2$party_id==3.5)]<-1
Japan2$DSPCom[which(Japan2$party_id==4 | Japan2$party_id==4.5)]<-1
Japan2$JCPCom[which(Japan2$party_id==4 | Japan2$party_id==4.5)]<-1

# Creates Unique Variables for loops
party<-unique(Japan2$party_id)

dist<- unique(Japan2$kucode)

year <- unique(Japan2$year)

#Calculates the total LDP VoteShare for each Ku Per Year
Japan2$totalVS <- 0
  for (j in 1:length(year)){
    for(i in 1:length(dist)){
      for(l in:length(party)){
        
      Japan2$LDPVS[which(Japan2$kucode == dist[i] & Japan2$year == year[j])] <- sum(Japan2$ku_voteshare[which(Japan2$LDPCombined==1 |Japan2$DSPCom==1 | Japan2$KOM==1 | Japan2$JCPCom & Japan2$year == year[j] & Japan2$kucode == dist[i])])
      }
    }
  }
write.csv(Japan2,"C:\\Users\\jgrei\\Dropbox\\Dissertation materials\\JapanClean7.csv")