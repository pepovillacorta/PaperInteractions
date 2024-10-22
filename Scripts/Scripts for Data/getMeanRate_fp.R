rm(list = ls())
gc()

setwd("C:/Users/user/OneDrive/DavidComportamiento/trainning")

#function to open data from a rat and get probability of press the lever at each stimulus
#rats to analyze
Rats <- c(39, 41, 42, 43, 44, 45, 46, 31, 32, 33, 36, 37, 38)

#experimental groups
Groups <- c(1, 1, 1, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1)

expGroups <- c("D", "D", "D", "D", "D", "D", "D", "S", "S", "S", "S", "S", "S")
names(expGroups) <- Rats

# #paths for each rat
# rat.path <- paste0("rat", Rats)
# 
# #names of the variables
# varNames <- c("FrameCount", "ElapsedTime", "LeverState", "Scene", "RewardState", 
#               "RedX", "RedY", "BlueX", "BlueY", "Pic1X", "Pic1Y", "Pic2X", "Pic2Y")
# 
# #open data
# ratList <- readRDS(paste0(rat.path[ir],"//DataFormatedRat", Rats[ir],".rds"))


#function to get rates from each rat session
getDataSes <- function(nses, ratList, experiment) {


# nses <- 1
indat <- ratList[[nses]]$data

prot <- ratList[[nses]]$protocol[["summary"]]

#number of total stimuli
totalStim <- sum(prot$repetitions, na.rm = TRUE)

#stimuli names
stimNames <- rownames(prot)

#remove -1
indat <- indat[indat$Scene != -1,]

#remove blanks
indat <- indat[!(indat$stimuli %in% c("Blank5", "Blank3")),]


#aggregate
outdata <- aggregate(beginPress ~ ID+Rat+session+stimuli+type+stID.total+stID.within+protocol, data = indat, sum )
#get rate
outdata$rate <- outdata$beginPress
outdata$offset <- 0

#add experiment information
outdata$experiment <- experiment 

for(i in 1:length(stimNames)){
  x <- stimNames[i]
  outdata$rate[outdata$stimuli == x] <- 
  outdata$rate[outdata$stimuli == x] / prot$duration[rownames(prot) == x]
  outdata$offset[outdata$stimuli == x] <- prot$duration[rownames(prot) == x]
}

#normalize time to max stimuli
maxStim <- max(indat$stID.total)
outdata$relTimeTotal <- (outdata$stID.total-1) / (maxStim-1)

outdata$timeTotal <- (outdata$stID.total - 1) + totalStim*(nses-1) 

#relative to max of each stimuli
outdata$relTimePartial <- 0
outdata$timePartial <- 0
for(i in 1:length(stimNames)){
  x <- stimNames[i]
  if(!is.na(prot$repetitions[rownames(prot) == x])){
    if(!(x %in% c("Blank5", "Blank3"))) {
  maxStim <- max(outdata$stID.within[outdata$stimuli == x])
  outdata$relTimePartial[outdata$stimuli == x] <- 
    (outdata$stID.within[outdata$stimuli == x] -1) / (maxStim-1)
  
  
  outdata$timePartial[outdata$stimuli == x] <- 
    (outdata$stID.within[outdata$stimuli == x] - 1) + maxStim * (nses-1)
  }
  }
}

#for the session
outdata$relTimeSes <- (nses-1) / (length(ratList)-1)

return(outdata)
}

getDatRat <- function(rat, expgroup) {
  experiment <- expgroup[names(expgroup) == rat]
  print(paste0("Formating Rat ", rat))
  #paths for each rat
  rat.path <- paste0("rat", rat)
  
  #open data
  ratList <- readRDS(paste0(rat.path,"//DataFormatedRat", rat,".rds"))
  ratDat.List <- lapply(1:length(ratList), getDataSes, ratList, experiment)
  ratDat <- do.call(rbind, ratDat.List)
  ratDat$timeTotal <- ratDat$timeTotal / max(ratDat$timeTotal)
  maxStim <- aggregate(timePartial ~ stimuli , data = ratDat, max)
  for(i in 1:dim(maxStim)[1]){
    ratDat$timePartial[ratDat$stimuli == maxStim$stimuli[i]] <-
      ratDat$timePartial[ratDat$stimuli == maxStim$stimuli[i]] / maxStim$timePartial[i]
  }
  return(ratDat)
}



fL <- lapply(Rats, getDatRat, expGroups)

overallRate <- do.call(rbind, fL)

saveRDS(overallRate, "overallRate.rds")
