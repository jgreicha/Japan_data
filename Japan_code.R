# Japan 2 -------------------------------------------------

#clear workspace
rm(list = ls())

#packages
library(tidyverse)
library(readxl)
library(gtools)
library(votingpower)

Japan2<-read_excel("C:\\Users\\jgrei\\Dropbox\\Dissertation materials\\JapanClean2.xlsx")

Japan2$LDPCombined<-0

Japan2$LDPCombined[which(Japan2$party_id==1 | Japan2$party_id==1.5)]<-1
summary(Japan2$LDPCombined)

sum(Japan2$ku_voteshare[which(Japan2$LDPCombined==1)])


#Vote Share Loops
Japan2$LDPVS83<-0

Japan2$LDPVS83<-if()
  
  
  
  