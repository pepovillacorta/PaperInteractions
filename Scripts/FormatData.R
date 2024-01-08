## TRAINING DEVICE SAVE DATA AS .dat FILES WITH RECORDING BEHAVIOURAL INFORMATION WITHIN 40 ms BINS
## THIS SCRIPT GET INFORMATION FROM .dat FILES OF THE GENERALIZATION SESSIONS AND APPLY A CORRECT FORMAT TO WORK WITH THEM



######################
# // Lever state 0: lever released, Lever state 1: lever pressed
# // Scene indicates number of scene currently shown on the screen: Scene 0: no scene, Scene 1: scene1, ...
# // RewardState 0: reward was not given, RewardState1: reward was given

## CLEAR WORKSPACE -----
rm(list = ls())
gc()

## LOAD LIBRARIES ------
library(stringr)

## SELECT PROTOCOL -----
protocol <- "DG" #dynamic protocol

# protocol <- "TG" #static protocol

## VARIABLES TO STORE ----
#names of the variables
varNames <- c("FrameCount", "ElapsedTime", "LeverState", "Scene", "RewardState", 
              "RedX", "RedY", "BlueX", "BlueY", "Pic1X", "Pic1Y", "Pic2X", "Pic2Y")

## FILES ----

##PATHS

#.dat FILES DON'T INCLUDE INFORMATION ABOUT COUNTERBALANCE GROUPS SO EACH FILE WAS 
#STORED IN THE FOLDER  "group 1" or "group 2" 

if(protocol == "DG") {
#path for group 1:
path1 <- "group1"

#path for group 2:
path2 <- "group2"
} else if(protocol == "TG"){
  #path for group 1:
  path1 <- "TGProtocol/group1"
  
  #path for group 2:
  path2 <- "TGProtocol/group2"
} else {
  print("Wrong protocol selected")
}

##FILES

#for group 1
files.names1 <- list.files(paste0(path1,"//"), pattern = ".dat")
#for group 2
files.names2 <- list.files(paste0(path2,"//"), pattern = ".dat")

## function to open the files and format data ----
readFiles <- function(fileName, path, group, protocoltype) {
  
  if(protocoltype == "DG"){
  #open file data
  finalName <- paste0(path, "//", fileName)
  #and store in a data.frame
  workfile <- read.table(finalName, header = FALSE, skip = 34, fill = TRUE)
  
  
  #last three lines are summaries: store appart and delete from data
  Nframes <- dim(workfile)[1] - 3
  
  #number of total presses
  press.total <- as.numeric(workfile[Nframes + 1,4])
  #numeber of pressess by scene
  press.by.scene <- as.numeric(workfile[Nframes + 2,4:dim(workfile)[2]])
  #number of total rewards
  rewards.total <- as.numeric(workfile[Nframes + 3, 2])
  
  #delete 3 last lines
  workfile <- workfile[1:Nframes,]
  
  #name columns
  names(workfile) <- varNames
  
  #all columns as number
  for(i in 1:ncol(workfile)){
    workfile[,i] <- as.numeric(workfile[,i])
  }
  
  
  # #remove -1
  # workfile <- workfile[workfile$Scene > 0,]
  # workfile <- workfile[2:nrow(workfile),]
  
  #add rat ID
  workfile$ID <- str_remove(fileName, ".dat")
  
  #add rat
  workfile$Rat <- gsub("\\_..*","",fileName)
  
  #add session
  ses <- gsub(".dat","", fileName)
  ses <- gsub(".*\\_G_","",ses)
  
  workfile$session <- as.numeric(ses)
  
  #While rat is pressing the lever appears 1 in "LeverState". 
  #find the begining of each lever press
  #create variable in the data.frame
  
  workfile$beginPress <- 0
  
  #work variable
  press <- array(0, dim = (nrow(workfile)-1))
  
  #number of frames to look for a previous pressing. 
  #if the new press is within this interval is considered spurious
  atLeast <- 6
  for(i in 2:(length(workfile$LeverState))) {
    if(workfile$LeverState[i] == 0) { #when lever is pressed
      
      if(workfile$LeverState[i-1] != 0) { #look if in previous frame was released
        if(i <= (atLeast+1)){ #if frame is less than atLeast
          if(sum(press[1:i-1]) == 0){ #if there's not a nearly previous press
            
            press[i-1] <- 1 #check as press
          }
        } else {#for higher frame numbers
          if(sum(press[(i-atLeast-1):(i-2)]) == 0){ #if there's not a nearly previous press
            press[i-1] <- 1 #add
            
          }
        }
        
      }
    }
  }
  workfile$beginPress[2:(nrow(workfile))] <- press
  
  
  
  #rename scenes
  new.names <- data.frame(group1=c("Reward_Static", "NonRew_Static", 
                                   "Reward_Dynamic", "NonRew_Dynamic",
                                   "Blank3", "Blank5",
                                   "Generalize_Reward", "Generalize_NonRew",
                                   "Novelty_Control", "Familiar"),
                          group2=c("NonRew_Static", "Reward_Static", 
                                   "NonRew_Dynamic", "Reward_Dynamic",
                                   "Blank3", "Blank5",
                                   "Generalize_NonRew", "Generalize_Reward",
                                   "Novelty_Control", "Familiar"))
  
  workfile$stimuli <- "NA"
  #-1 as NA
  workfile$stimuli[workfile$Scene == -1] <- NA
  
  for(i in 1:10) {
    workfile$stimuli[workfile$Scene == i] <- new.names[i,group]
  }
  
  #add design group
  workfile$designGroup <- group
  
  #add stimuly type
  learned <- c("Reward_Static", "NonRew_Static", 
               "Reward_Dynamic", "NonRew_Dynamic")
  blank <- c("Blank3", "Blank5")
  novel <- c("Generalize_Reward", "Generalize_NonRew",
             "Novelty_Control", "Familiar")
  
  workfile$type <- "NA"
  workfile$type[workfile$stimuli %in%learned ] <- "L"
  workfile$type[workfile$stimuli %in% blank ] <- "B"
  workfile$type[workfile$stimuli %in% novel ] <- "N"
  
  
  #add stimuli number
  #total
  current.st <- 1
  workfile$stID.total <- 0
  workfile$stID.total[1] <- current.st
  for(i in 2:nrow(workfile)){
    if(workfile$Scene[i] != -1) {
      if(workfile$Scene[i] != workfile$Scene[i-1]) {
        if(workfile$Scene[i-1] != -1) {
          current.st<-current.st + 1
        }
      }
    }
    workfile$stID.total[i] <- current.st
  }
  
  #within stimuli type
  #set current stimuli to zero
  current.st <- 0
  #create variable with all values zero
  workfile$stID.within <- 0
  
  #find the type of the first stimulus in the sequence
  firstStim <- workfile$Scene[1]
  #loop over all stimuli
  for(i in 1:10) {
    if(i != firstStim){ #if stimuli is not the first
      for(j in 2:nrow(workfile)) { #loop over all frames
        if(workfile$Scene[j] == i) { #if frames belongs to stimuli i
          if(workfile$Scene[j-1] != -1){ #and previous frame is not -1
            if(workfile$Scene[j-1] != i) { #and previous frame is different
              
              current.st <- current.st + 1 #new id 
              
            }
          }
          workfile$stID.within[j] <- current.st #set id
          
        }
      }
      current.st <- 0
    } else { #repeat for first stimulus type
      current.st <- 1
      for(j in 2:nrow(workfile)) {
        if(workfile$Scene[j] == i) {
          if(workfile$Scene[j-1] != -1){
            if(workfile$Scene[j-1] != i) {
              
              current.st <- current.st + 1
              
            }
          }
          workfile$stID.within[j] <- current.st
          
        }
      }
      current.st <- 0
    }
    
  }
  workfile$stID.within[1] <- 1
  

  
  #total id
  #set current stimuli to zero
  current.st <- 1
  #create variable with all values zero
  workfile$stID.total <- 0
  for(j in 2:nrow(workfile)) { #loop over all frames
    if(workfile$Scene[j] != -1) { #if current scene is not -1
      if(workfile$Scene[j] != workfile$Scene[j-1]) { #if current type is different and previous 
        if(workfile$Scene[j-1] != -1){ #and previous is not -1
          
          
          current.st <- current.st + 1 #increase id by 1
          
        }
      }
      workfile$stID.total[j] <- current.st #add stimuli id to data.frame
      
    }
  }
  workfile$stID.total[1] <- 1 #first frame id = 1
  } # end of DG protocol
  else if(protocoltype == "TG"){
    #open file data
    finalName <- paste0(path, "//", fileName)
    #and store in a data.frame
    workfile <- read.table(finalName, header = FALSE, skip = 34, fill = TRUE)
    
    
    #last three lines are summaries: store appart and delete from data
    Nframes <- dim(workfile)[1] - 3
    
    #number of total presses
    press.total <- as.numeric(workfile[Nframes + 1,4])
    #numeber of pressess by scene
    press.by.scene <- as.numeric(workfile[Nframes + 2,4:dim(workfile)[2]])
    #number of total rewards
    rewards.total <- as.numeric(workfile[Nframes + 3, 2])
    
    #delete 3 last lines
    workfile <- workfile[1:Nframes,]
    
    #name columns
    names(workfile) <- varNames
    
    #all columns as number
    for(i in 1:ncol(workfile)){
      workfile[,i] <- as.numeric(workfile[,i])
    }
    
    
    # #remove -1
    # workfile <- workfile[workfile$Scene > 0,]
    # workfile <- workfile[2:nrow(workfile),]
    
    #add rat ID
    workfile$ID <- str_remove(fileName, ".dat")
    
    #add rat
    workfile$Rat <- gsub("\\_..*","",fileName)
    
    #add session
    ses <- gsub(".dat","", fileName)
    ses <- gsub(".*\\_G_","",ses)
    
    workfile$session <- as.numeric(ses)
    
    #While rat is pressing the lever appears 1 in "LeverState". 
    #find the begining of each lever press
    #create variable in the data.frame
    workfile$beginPress <- 0
    #work variable
    press <- array(0, dim = (nrow(workfile)-1))
    
    #number of frames to look for a previous pressing. 
    #if the new press is within this interval is considered spurious
    atLeast <- 6
    for(i in 2:(length(workfile$LeverState))) {
      if(workfile$LeverState[i] == 0) { #when lever is pressed
        
        if(workfile$LeverState[i-1] != 0) { #look if in previous frame was released
          if(i <= (atLeast+1)){ #if frame is less than atLeast
            if(sum(press[1:i-1]) == 0){ #if there's not a nearly previous press
              
              press[i-1] <- 1 #check as press
            }
          } else {#for higher frame numbers
            if(sum(press[(i-atLeast-1):(i-2)]) == 0){ #if there's not a nearly previous press
              press[i-1] <- 1 #add
              
            }
          }
          
        }
      }
    }
    workfile$beginPress[2:(nrow(workfile))] <- press
    
    
    
    #rename scenes
    new.names <- data.frame(group1=c("Reward_up", "NonRew_up", 
                                     "Reward_down", "NonRew_down",
                                     "Blank3", "Blank5",
                                     "Generalize_Reward_up", "Generalize_NonRew_up",
                                     "Generalize_Reward_down", "Generalize_NonRew_down"),
                            group2=c("NonRew_up", "Reward_up", 
                                     "NonRew_down", "Reward_down",
                                     "Blank3", "Blank5",
                                     "Generalize_NonRew_up", "Generalize_Reward_up",
                                     "Generalize_NonRew_down", "Generalize_Reward_down"))
    
    workfile$stimuli <- "NA"
    #-1 as NA
    workfile$stimuli[workfile$Scene == -1] <- NA
    
    for(i in 1:10) {
      workfile$stimuli[workfile$Scene == i] <- new.names[i,group]
    }
    
    #add design group
    workfile$designGroup <- group
    
    #add stimuly type
    learned <- c("Reward_up", "NonRew_up", 
                 "Reward_down", "NonRew_down")
    blank <- c("Blank3", "Blank5")
    novel <- c("Generalize_Reward_up", "Generalize_NonRew_up",
               "Generalize_Reward_down", "Generalize_NonRew_down")
    
    workfile$type <- "NA"
    workfile$type[workfile$stimuli %in%learned ] <- "L"
    workfile$type[workfile$stimuli %in% blank ] <- "B"
    workfile$type[workfile$stimuli %in% novel ] <- "N"
    
    
    #add stimuli number
    #total
    current.st <- 1
    workfile$stID.total <- 0
    workfile$stID.total[1] <- current.st
    for(i in 2:nrow(workfile)){
      if(workfile$Scene[i] != -1) {
        if(workfile$Scene[i] != workfile$Scene[i-1]) {
          if(workfile$Scene[i-1] != -1) {
            current.st<-current.st + 1
          }
        }
      }
      workfile$stID.total[i] <- current.st
    }
    
    #within stimuli type
    #set current stimuli to zero
    current.st <- 0
    #create variable with all values zero
    workfile$stID.within <- 0
    
    #find the type of the first stimulus in the sequence
    firstStim <- workfile$Scene[1]
    #loop over all stimuli
    for(i in 1:10) {
      if(i != firstStim){ #if stimuli is not the first
        for(j in 2:nrow(workfile)) { #loop over all frames
          if(workfile$Scene[j] == i) { #if frames belongs to stimuli i
            if(workfile$Scene[j-1] != -1){ #and previous frame is not -1
              if(workfile$Scene[j-1] != i) { #and previous frame is different
                
                current.st <- current.st + 1 #new id 
                
              }
            }
            workfile$stID.within[j] <- current.st #set id
            
          }
        }
        current.st <- 0
      } else { #repeat for first stimulus type
        current.st <- 1
        for(j in 2:nrow(workfile)) {
          if(workfile$Scene[j] == i) {
            if(workfile$Scene[j-1] != -1){
              if(workfile$Scene[j-1] != i) {
                
                current.st <- current.st + 1
                
              }
            }
            workfile$stID.within[j] <- current.st
            
          }
        }
        current.st <- 0
      }
      
    }
    workfile$stID.within[1] <- 1
    
    #there are some differences between this calculus and dat file
    
    #total id
    #set current stimuli to zero
    current.st <- 1
    #create variable with all values zero
    workfile$stID.total <- 0
    for(j in 2:nrow(workfile)) { #loop over all frames
      if(workfile$Scene[j] != -1) { #if current scene is not -1
        if(workfile$Scene[j] != workfile$Scene[j-1]) { #if current type is different and previous 
          if(workfile$Scene[j-1] != -1){ #and previous is not -1
            
            
            current.st <- current.st + 1 #increase id by 1
            
          }
        }
        workfile$stID.total[j] <- current.st #add stimuli id to data.frame
        
      }
    }
    workfile$stID.total[1] <- 1 #first frame id = 1
  } #end of TG protocol
  
  
  
  return(list(workfile=workfile, press.total = press.total, press.by.scene = press.by.scene))
  
} #end of function

#apply over group 1
list1 <- lapply(files.names1, readFiles, path1, 1, protocol)
#apply over group 2
list2 <- lapply(files.names2, readFiles, path2, 2, protocol)

#merge data
N1 <- length(files.names1)
N2 <- length(files.names2)
#list1 first
final.data <- as.data.frame(matrix(NA,nrow=0,ncol = ncol(list1[[1]]$workfile)))
final.dat.file <- vector(mode = "list", length = (N1+N2))
for(i in 1:N1){
  workdat <- as.data.frame(list1[[i]]$workfile)
  final.data <- as.data.frame(rbind(final.data, workdat))
  workname <- workdat$ID[1]
  final.dat.file[[i]] <- list(press.total = list1[[i]]$press.total, 
                              press.by.scene = list1[[i]]$press.by.scene)
  names(final.dat.file)[i] <- workname
}
#list2
for(i in 1:N2){
  workdat <- as.data.frame(list2[[i]]$workfile)
  final.data <- as.data.frame(rbind(final.data, workdat))
  workname <- workdat$ID[1]
  final.dat.file[[N1+i]] <- list(press.total = list2[[i]]$press.total, 
                                 press.by.scene = list2[[i]]$press.by.scene)
  names(final.dat.file)[N1+i] <- workname
}




#save
if(protocol == "DG"){
saveRDS(final.data, "Behavioral_Data.RDS")
saveRDS(final.dat.file,"Behavioral_from_Dat_File.RDS")
} else if(protocol == "TG"){
  saveRDS(final.data, "Behavioral_Data_TGProtocol.RDS")
  saveRDS(final.dat.file,"Behavioral_from_Dat_File_TGProtocol.RDS")
}

##extended ----
rec_names <- unique(final.data$ID)

final.data$time <- 0

for(i in 1:length(rec_names)){
  wdat <- final.data[final.data$ID == rec_names[i],]
  max_st <- max(wdat$stID.total)
  for(j in 1:max_st){
    wwdat <- wdat[wdat$stID.total==j,]
    wtime <- wwdat$ElapsedTime - wwdat$ElapsedTime[1]
    final.data$time[final.data$ID == rec_names[i] & final.data$stID.total == j] <- wtime
  }
}

if(protocol == "DG"){
saveRDS(final.data, "Behavioral_Data_extended.RDS")
} else if(protocol == "TG"){
  saveRDS(final.data, "Behavioral_Data_extended_TGProtocol.RDS")
}


#check. Comment to pass trough all files
comment.probe <- TRUE #TRUE = comment, don't do
if(!comment.probe) {
  wrat <- 2
  wfile <- files.names2[wrat]
  wpath <- path2
  wgroup <- 2
  
  probe <- readFiles(wfile, wpath, wgroup)
  
  #check total press
  print(paste0("Difference in total lever press = ", sum(probe$workfile$beginPress)-probe$press.total))
  by.stim <- aggregate(beginPress ~ Scene, data = probe$workfile, sum)
  by.stim <- by.stim[by.stim$Scene != -1,]
  by.stim$beginPress <- by.stim$beginPress - probe$press.by.scene
  print(paste0("Difference by stimuli type:"))
  by.stim
  
  print(paste0("Total stimuli = ", max(probe$workfile$stID.total)))
  print(paste0("Stimuli by type: "))
  aggregate(stID.within~stimuli, data=probe$workfile, max)
}
