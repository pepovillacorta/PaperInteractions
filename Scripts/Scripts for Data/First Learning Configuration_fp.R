#performance over the first configuration of the training 

### setup -----
#clear workspace
rm(list = ls())
#clear ram
gc()
#clear console
cat("\014")

#set working dir
setwd("C:/Users/user/OneDrive/DavidComportamiento/trainning")


#libraries
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggeffects)
library(gridExtra)
library(emmeans)
library(DHARMa)
library(glmmTMB)
library(parallel)
library(car)
library(survival)
library(coxme)
library(survminer)

#colors
color <- c("red", "darkorange", "blue", "darkblue")

### DATA for Mean Rate ----
datos <- readRDS("overallRate.rds")
dsave <- datos

#subset first protocol
datos <- datos[datos$protocol == 1,]

#subset D setup
datos <- datos[datos$setup == "D",]

# Normalize time
IDs <- unique(datos$Rat)
Nids <- length(IDs)
datos$time <- 0
for(i in 1:Nids){
  wd <- datos[datos$Rat == IDs[i],]
  wd$session <- as.numeric(wd$session)
  maxTime <- max(wd$session)
  datos$time[datos$Rat == IDs[i]] <- (as.numeric(datos$session[datos$Rat == IDs[i]]) - 1 ) / (maxTime-1)
  
}


#select variables
varSel <- c("ID", "Rat", "session", "stimuli", "rewarded", "beginPress", "rate", "offset", "time")
datos <- datos[,varSel]

#split rewardwed and non-rewarded
datosR <- datos[datos$rewarded == "R",]
datosNR <- datos[datos$rewarded == "NR",]

#get alpha
zalpha = 1.0 #1.96 for 95% ci, 1.0 for se
alphaci <- 2*pnorm(zalpha, lower.tail = FALSE)


### RATE THROUGH SESSIONS ----
### MODEL FOR REWARDED STIMULI ----
fitR <- glmer(rate ~ stimuli*time + (1|ID), data = datosR, family = binomial("logit"), weights = offset)

#predictions
predR <- ggemmeans(fitR, terms = c("time [all]", "stimuli"), type = "fe", ci_level = 1-alphaci)

#p-values
pR <- emtrends(fitR, specs = "stimuli", var = "time")



### MODEL FOR NON-REWARDED STIMULI ----
fitNR <- glmer(rate ~ stimuli*time + (1|ID), data = datosNR, family = binomial("logit"), weights = offset)

#predictions
predNR <- ggemmeans(fitNR, terms = c("time [all]", "stimuli"), type = "fe", ci_level = 1-alphaci )

#p-values
pNR <- emtrends(fitNR, specs = "stimuli", var = "time")



### Plot rate ----
#merge REW and non-REW predictions
dataPlotTime <- rbind(predR, predNR)

#set group as factor
dataPlotTime$group <- factor(dataPlotTime$group, levels = c("Reward_Dynamic", "Reward_Static", "NonRew_Dynamic", "NonRew_Static"))

#change levels order for plotting
levels(dataPlotTime$group) <- c("Dynamic REW", "Static REW", "Dynamic non-REW", "Static non-REW")

#plot
plotTrain <- ggplot(dataPlotTime) +
  geom_line(aes(x=x, y=predicted, group = group, color = group), alpha = 1, linewidth = 1) +
  geom_ribbon(aes(x=x, ymax=conf.high, ymin=conf.low, fill = group, group = group), alpha = 0.15) +
  scale_color_manual(values = color, guide = guide_legend(byrow = TRUE, override.aes = list(linetype = 1, shape = NA, linewidth = 3))) +
  scale_fill_manual(values = color, guide = "none") +
  ylab("lever pressing prob") +
  xlab("Normalized time \n (0 = first session, 1=last session)") +
  geom_text(x=0.15, y=0.25, label = "*", color = "red", size = 12) +
  scale_x_continuous( limits = c(0, 1), n.breaks = 5, expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 0.34), n.breaks = 4, expand = c(0,0)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0, 0.34), clip = "off") +
  theme(legend.title = element_blank(),
        legend.spacing.y = unit(25, 'mm'),
        legend.justification = c(0,0),
        legend.key = element_rect(fill = NA)
  )

#save plot
setwd("C:/Users/user/OneDrive/DavidComportamiento")
saveRDS(plotTrain, "MeanRate_FirstConfiguration_familiar.rds")
write.csv2(dataPlotTime, "fig2_panel_A.csv")

### FIRST LEVER PRESS THROUGH SESSIONS ----

#function to get the first lever press of each stimulation period
firstLever.st <- function(wIDst, wwinput, durcens){
  # print(IDst)
  w3input <- wwinput[wwinput$stID.total == wIDst,]
  w3input$time <- w3input$ElapsedTime - w3input$ElapsedTime[1]
 
   #find first lever press
  wfirst <- w3input[w3input$beginPress == 1,][1,]
  
  #if exist
  if (!is.na(wfirst$time)){
    wfirst["censored"] <- 1
   
        #if not, add data, set time to max time and censored data (=1)
  } else if (is.na(wfirst$time)){
    
    #create array
    wfirst <- w3input[1,]
    
    #add time
    wfirst["time"] <- durcens
    
    #censoring data
    wfirst["censored"] <- 0
    
    
    
  } else {
    print("something is wrong")
  }
  
  return(wfirst)
}


##function to apply over all sessions of a protocol
overSessions <- function(ses, wr, durcens){
  #open data for session
  datS <- readRDS(paste0("./", wr, "/DataFormated_",wr,"ses",ses,".rds"))$data
  
  #remove blanks
  datS <- datS[!(datS$type == "NA"),]
  # sdata <- as.data.frame(matrix(NA, nrow = 0, ncol = (ncol(datS)+1)))
  IDst <- unique(datS$stID.total)
  # sapply(IDst, firstLever.st, input)
  sdata<- do.call(rbind, lapply(IDst, firstLever.st, datS, durcens))
  return(sdata)
  
  
}

### function to apply over all protocols
overProt <- function(sel_prot, protocols, wr, pInf){
  durcens <- protInf[[sel_prot]]$duration[1] * 1000
  limProt <- protocols[protocols$protocol == sel_prot,]
  #select sessions
  prot_sessions <- seq(limProt$start,limProt$end)
  byProt <- do.call(rbind, lapply(prot_sessions, overSessions, wr, durcens ))
  
  #normalize session
  byProt$session <- as.numeric(byProt$session)
  byProt$NormSes <- (byProt$session - (min(byProt$session))) / (max(byProt$session)-min(byProt$session))
  
  return(byProt)
}


### function to apply over all rats
overRat <- function(r,pInf){
  protR <- readRDS(paste0("./", r,"/",r,"protocol.rds"))
  byRat <- lapply(1, overProt, protR, r, pInf)
  names(byRat) <- paste0("protocol_",1)
  return(byRat)
  
}


#information about protocols
setwd("C:/Users/user/OneDrive/DavidComportamiento/trainning")
protInf <- readRDS("protocols.rds")$summary


#get rats id
RatsS <- unique(datos$Rat)

#get first lever press
survDat <- lapply(RatsS, overRat, protInf)

names(survDat) <- RatsS


## merge protocols
protocols <- paste0("protocol_", 1)

byProtocols <- do.call(rbind, lapply(RatsS, function(x) survDat[[x]][["protocol_1"]]))


## pass over all sessions of a protocol
coxBySes <- function(ses, sdata){
  survDat <- sdata[sdata$NormSes == ses,]
  
  if(length(unique(survDat$Rat)) > 1){
    survFit <- coxph(Surv(time=time, event = censored, type = "right") ~ stimuli, 
                     id = Rat, data = survDat)
  } else if (length(unique(survDat$Rat)) == 1){
    survFit <- coxph(Surv(time=time, event = censored, type = "right") ~ stimuli, 
                     data = survDat)
  }
  HR <- summary(emmeans(survFit,  ~stimuli), type = "response")[,1:3]
  HR$NormSes <- ses
  
  #median life by stimuli type
  stimType <- levels(survDat$stimuli)
  medianLife <- as.data.frame(
    do.call(rbind, lapply(stimType, function(x) 
      summary(
        survfit(Surv(time, censored) ~ 1, data = survDat[survDat$stimuli==x,]))$table[7:9]
    )
    )
  )
  medianLife$stimuli <- stimType
  medianLife$NormSes <- ses
  medianLife[is.na(medianLife)]<-max(survDat$time)
  return(list(HR=HR, medianLife = medianLife))
}

## pass over all protocols
coxByProt <- function(sdata){
  
  sdata$stimuli <- as.factor(sdata$stimuli)
  sdata$stimuli <- relevel(sdata$stimuli, ref = "NonRew_Static")
  sdata$NormSes <- round(sdata$NormSes, digits = 2)
  sesTimes <- unique(sdata$NormSes)
  #apply over all sessions
  bySes <- lapply(sesTimes, coxBySes, sdata)
  names(bySes) <- sesTimes
  HR <- do.call(rbind, lapply(sesTimes, function(x) bySes[[as.character(x)]]$HR))
  medianLife <- do.call(rbind, lapply(sesTimes, function(x) bySes[[as.character(x)]]$medianLife))
  return(list(HR=HR, medianLife=medianLife))
}

datPlotSurv <- coxByProt(byProtocols)

### survfit
sdata <- byProtocols
sdata$stimuli <- as.factor(sdata$stimuli)
sdata$stimuli <- relevel(sdata$stimuli, ref = "NonRew_Static")
sdata$NormSes <- as.numeric(round(sdata$NormSes, digits = 2))

#fit for inference
survFit <- coxph(Surv(time=time, event = censored, type = "right") ~ stimuli*NormSes, 
                 id = Rat, data = sdata)


# car::Anova(survFit, test.statistic = "Wald")

#comparisons
pair.surv.lea <- emtrends(survFit,  specs = "stimuli", var = "NormSes")

v <- summary(pair.surv.lea, type = "response")


#p-values
p.pair.surv.lea <-summary(contrast(pair.surv.lea, method = "tukey"))

#split
workdat <- sdata[sdata$rewarded == "R",]

#drop levels
workdat$stimuli <- droplevels(workdat$stimuli)

#fit rewarded stimuli
survFit.R <- coxph(Surv(time=time, event = censored, type = "right") ~ stimuli+NormSes, 
                   id = Rat, data = workdat)

#median time for rewarded stimuli
stimType_surv.r <- levels(workdat$stimuli)
Med.R <- as.data.frame(
  do.call(rbind, lapply(stimType_surv.r, function(x) 
    summary(
      survfit(Surv(time, censored) ~ 1, data = workdat))$table[7:9]
  )
  )
)
Med.R$stimuli <- stimType_surv.r

workdat <- sdata[sdata$rewarded == "NR",]
workdat$stimuli <- droplevels(workdat$stimuli)
survFit.NR <- coxph(Surv(time=time, event = censored, type = "right") ~ stimuli+NormSes, 
                    id = Rat, data = workdat)

### fit by stimuli for plotting
survFit_NRS <- coxph(Surv(time=time, event = censored, type = "right") ~ NormSes, 
                     data = sdata[sdata$stimuli == "NonRew_Static",])
HR_NRS <- ggemmeans(survFit_NRS, terms = c("NormSes [all]"), type = "fe", ci_level = 1-alphaci )
HR_NRS$group <- "NonRew_Static"
survFit_NRD <- coxph(Surv(time=time, event = censored, type = "right") ~ NormSes, 
                     id = Rat, data = sdata[sdata$stimuli == "NonRew_Dynamic",])
HR_NRD <- ggemmeans(survFit_NRD, terms = c("NormSes [all]"), type = "fe", ci_level = 1-alphaci )
HR_NRD$group <- "NonRew_Dynamic"
survFit_RS <- coxph(Surv(time=time, event = censored, type = "right") ~ NormSes, 
                    id = Rat, data = sdata[sdata$stimuli == "Reward_Static",])
HR_RS <- ggemmeans(survFit_RS, terms = c("NormSes [all]"), type = "fe", ci_level = 1-alphaci )
HR_RS$group <- "Reward_Static"
survFit_RD <- coxph(Surv(time=time, event = censored, type = "right") ~ NormSes, 
                    id = Rat, data = sdata[sdata$stimuli == "Reward_Dynamic",])
HR_RD <- ggemmeans(survFit_RD, terms = c("NormSes [all]"), type = "fe", ci_level = 1-alphaci )
HR_RD$group <- "Reward_Dynamic"

HR <- rbind(HR_NRS, HR_NRD, HR_RS, HR_RD)


#get HR

#group as factor
HR$group <- factor(HR$group, levels = c("Reward_Dynamic", "Reward_Static", "NonRew_Dynamic", "NonRew_Static"))

#reorder levels for plotting
levels(HR$group) <- c("Dynamic REW", "Static REW", "Dynamic non-REW", "Static non-REW")

##PLOT HR ---
plotHR <- ggplot(HR) +
  geom_line(aes(x=x, y=predicted, group = group, color = group), alpha = 1, linewidth = 1) +
  geom_ribbon(aes(x=x, ymax=conf.high, ymin=conf.low, fill = group, group = group), alpha = 0.15) +
  scale_color_manual(values = color, guide = guide_legend(byrow = TRUE, override.aes = list(linetype = 1, shape = NA, linewidth = 3))) +
  scale_fill_manual(values = color, guide = "none") +
  ylab("Hazard") +
  xlab("Normalized time \n (0 = first session, 1=last session)") +
  geom_text(x=0.15, y=0.65, label = "*", color = "red", size = 12) +
  scale_x_continuous(limits = c(0, 1), n.breaks = 5, expand = c(0,0)) +
  scale_y_continuous(limits = c(0.4, 0.79), n.breaks = 4, expand = c(0,0)) +
  coord_cartesian(xlim = c(0, 1), ylim = c(0.4, 0.79), clip = "off") +
  theme(legend.title = element_blank(),
        legend.spacing.y = unit(25, 'mm'),
        legend.justification = c(0,0),
        legend.key = element_rect(fill = NA)
  )


#save
setwd("C:/Users/user/OneDrive/DavidComportamiento")
saveRDS(plotHR, "Survival_FirstConfiguration_familiar.rds")
write.csv2(HR,"fig2_panel_B.csv")




### Final figure ----


plotTrain <- plotTrain + theme(legend.position = "bottom")
plotHR <- plotHR + theme(legend.position = "none")


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(plotTrain)

plotTraining <- grid.arrange(arrangeGrob(plotTrain+theme(legend.position = "none"), plotHR, nrow = 1, ncol = 2),
                             mylegend, nrow=2,heights=c(10, 1))

ggsave("Figure training performance.tiff", plot = plotTraining, units="mm", width=190, height=100, dpi=600, compression = 'lzw')

