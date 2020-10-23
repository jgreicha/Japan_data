# Japan 2 Cleaning and Combining Data-------------------------------------------------

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

Japan2$LDPCombined[which(Japan2$party_id==1 | Japan2$party_id==1.5)]<-1

# Creates Unique Variables for loops

dist<- unique(Japan2$kucode)

year <- unique(Japan2$year)

#Calculates the total LDP VoteShare for each Ku Per Year
Japan2$LDPVS <- 0

for (j in 1:length(year)) {
  for(i in 1:length(dist))
  {
    Japan2$LDPVS[which(Japan2$kucode == dist[i] & Japan2$year == year[j])] <- sum(Japan2$ku_voteshare[which(Japan2$LDPCombined==1 & Japan2$year == year[j] & Japan2$kucode == dist[i])])
  }
}
# Calculates JSP Vote Share  for each Ku Per Year
Japan2$JSPCombined<-0

Japan2$JSPCombined[which(Japan2$party_id==2 | Japan2$party_id==2.5)]<-1

Japan2$JSPVS <- 0

for (l in 1:length(year)) {
  for(m in 1:length(dist))
  {
    Japan2$JSPVS[which(Japan2$kucode == dist[m] & Japan2$year == year[l])] <- sum(Japan2$ku_voteshare[which(Japan2$JSPCombined==1 & Japan2$year == year[l] & Japan2$kucode == dist[m])])
  }
}
view(Japan2$JSPVS)

# Calculates the Komeito Vote Share for each Ku Per Year
Japan2$KOM<-0

Japan2$KOM[which(Japan2$party_id==3 | Japan2$party_id==3.5)]<-1

Japan2$KomVS <- 0

for (k in 1:length(year)) {
  for(p in 1:length(dist))
  {
    Japan2$KomVS[which(Japan2$kucode == dist[p] & Japan2$year == year[k])] <- sum(Japan2$ku_voteshare[which(Japan2$KOM==1 & Japan2$year == year[k] & Japan2$kucode == dist[p])])
  }
}
view(Japan2$KomVS)

#Calculates the DSP Vote Share for each Ku Per Year
Japan2$DSPCom<-0

Japan2$DSPCom[which(Japan2$party_id==4 | Japan2$party_id==4.5)]<-1

Japan2$DSPVS <- 0

for (t in 1:length(year)) {
  for(u in 1:length(dist))
  {
    Japan2$DSPVS[which(Japan2$kucode == dist[u] & Japan2$year == year[t])] <- sum(Japan2$ku_voteshare[which(Japan2$DSPCom==1 & Japan2$year == year[t] & Japan2$kucode == dist[u])])
  }
}
view(Japan2$DSPVS) 

# calculates Reform Party Vote Share for each Ku Per Year
Japan2$RefCom<-0

Japan2$RefCom[which(Japan2$party_id==6)]<-1

Japan2$RefVS <- 0

for (f in 1:length(year)) {
  for(o in 1:length(dist))
  {
    Japan2$RefVS[which(Japan2$kucode == dist[o] & Japan2$year == year[f])] <- sum(Japan2$ku_voteshare[which(Japan2$JCPCom==1 & Japan2$year == year[f] & Japan2$kucode == dist[o])])
  }
}
view(Japan2$RefVS) 

# calculates JCP Vote Share for each Ku Per Year
Japan2$JCPCom<-0

Japan2$JCPCom[which(Japan2$party_id==4 | Japan2$party_id==4.5)]<-1

Japan2$JCPVS <- 0

for (u in 1:length(year)) {
  for(v in 1:length(dist))
  {
    Japan2$JCPVS[which(Japan2$kucode == dist[v] & Japan2$year == year[u])] <- sum(Japan2$ku_voteshare[which(Japan2$JCPCom==1 & Japan2$year == year[u] & Japan2$kucode == dist[v])])
  }
}
view(Japan2$JCPVS) 

# calculates Japan New Party Vote Share for each Ku Per Year
Japan2$JNPCom<-0

Japan2$JNPCom[which(Japan2$party_id==13 | Japan2$party_id==13.5)]<-1

Japan2$JNPVS <- 0

for (w in 1:length(year)) {
  for(s in 1:length(dist))
  {
    Japan2$JNPVS[which(Japan2$kucode == dist[s] & Japan2$year == year[w])] <- sum(Japan2$ku_voteshare[which(Japan2$JNPCom==1 & Japan2$year == year[w] & Japan2$kucode == dist[s])])
  }
}
view(Japan2$JNPVS) 

# calculates Renewl Vote Share for each Ku Per Year
Japan2$RenCom<-0

Japan2$RenCom[which(Japan2$party_id==14)]<-1

Japan2$RenVS <- 0

for (c in 1:length(year)) {
  for(x in 1:length(dist))
  {
    Japan2$RenVS[which(Japan2$kucode == dist[x] & Japan2$year == year[c])] <- sum(Japan2$ku_voteshare[which(Japan2$RenCom==1 & Japan2$year == year[c] & Japan2$kucode == dist[x])])
  }
}
view(Japan2$RenVS) 

# calculates Social Democratic Leauge Vote Share for each Ku Per Year
Japan2$SDLCom<-0

Japan2$SDLCom[which(Japan2$party_id==19 | Japan2$party_id==19.5)]<-1

Japan2$SDLVS <- 0

for (e in 1:length(year)) {
  for(y in 1:length(dist))
  {
    Japan2$SDLVS[which(Japan2$kucode == dist[y] & Japan2$year == year[e])] <- sum(Japan2$ku_voteshare[which(Japan2$SDLCom==1 & Japan2$year == year[y] & Japan2$kucode == dist[e])])
  }
}
view(Japan2$SDLVS) 

# calculates Social New Liberal Club Share for each Ku Per Year
Japan2$NLCCom<-0

Japan2$NLCCom[which(Japan2$party_id==11 | Japan2$party_id==11.5)]<-1

Japan2$NLCVS <- 0

for (e in 1:length(year)) {
  for(y in 1:length(dist))
  {
    Japan2$NLCVS[which(Japan2$kucode == dist[y] & Japan2$year == year[e])] <- sum(Japan2$ku_voteshare[which(Japan2$NLCCom==1 & Japan2$year == year[y] & Japan2$kucode == dist[e])])
  }
}
view(Japan2$NLCVS) 



# ENP Calculator ------------------------------------------------------------

P_sqLDP<-Japan2$LDPVS^2
P_sqJSP<-Japan2$JSPVS^2
P_sqKom<-Japan2$KomVS^2
P_sqJCP<-Japan2$JCPVS^2
P_sqJNP<-Japan2$JNPVS^2
P_sqRef<-Japan2$RefVS^2
P_sqRen<-Japan2$RenVS^2
P_sqSDL<-Japan2$SDLVS^2
P_sqNCL<-Japan2$NLCVS^2

Japan2$ENP2<-0
g<-length(Japan2$year)

for (i in 1:g) {
  Japan2$ENP2[i]<-1/(P_sqLDP[i]+P_sqJSP[i]+P_sqKom[i]+P_sqJCP[i]+P_sqJNP[i]+P_sqRen[i]+P_sqRef[i]+P_sqSDL[i]+P_sqNCL[i])
}
view(Japan2$ENP2)

# ENRP Calculator

Japan2$ENRP3<-0

for (i in 1:length(Japan2$year)) {
  const <- c(Japan2$LDPVS[i]*100, Japan2$JSPVS[i]*100,Japan2$JNPVS[i]*100, Japan2$KomVS[i]*100,Japan2$JCPVS[i]*100,Japan2$SDLVS[i]*100,Japan2$RefVS[i]*100,Japan2$RenVS[i]*100,Japan2$NLCVS[i]*100)
  
  eec <- create_weighted_voting_game(49, const)
  
  # the Banzhaf score counts the coalitions where a voter is critical.
  score <- compute_bz_score(eec)
  
  t<-(score/(sum(score)))^2
  Japan2$ENRP3[i] <-1/sum(t)
}

#LDP Victory
Japan2$LDPVictory<-0
Japan2$LDPVictory[which(Japan2$LDPVS>Japan2$JSPVS&Japan2$LDPVS>Japan2$JNPVS&Japan2$LDPVS>Japan2$KomVS&Japan2$LDPVS>Japan2$JCPVS&Japan2$LDPVS>Japan2$SDLVS&Japan2$LDPVS>Japan2$RefVS&Japan2$LDPVS>Japan2$RenVS&Japan2$LDPVS>Japan2$NLCVS)]<-1


write.csv(Japan2,"C:\\Users\\jgrei\\Dropbox\\Dissertation materials\\JapanClean5.csv")
# All that needs to be done now is turn the code in line 28 into a for loop. dist[1] would be replaced with dist[i] and year [1] replaced with year[j].

  
  
  
  