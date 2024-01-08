#figures for the 2nd to 5th protocols


### setup -----
#clear workspace
rm(list = ls())
#clear ram
gc()
#clear console
cat("\014")
setwd("C:/Users/user/OneDrive/DavidComportamiento/trainning")


#libraries
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggeffects)
library(gridExtra)
library(gtable)
library(emmeans)
library(DHARMa)
library(glmmTMB)
library(parallel)
library(car)
library(survival)
library(coxme)
library(survminer)
library(gridtext)
library(grid)
library(tidyverse)

#get alpha
zalpha = 1.0 #1.96 for 95% ci, 1.0 for se
alphaci <- 2*pnorm(zalpha, lower.tail = FALSE)


# colors
color <- c("red", "darkorange", "blue", "darkblue")



### DATA ------
##Data for figure 1A
datos <- readRDS("overallRate.rds")

#remove rat 43
datos <- datos[datos$Rat != "rat43",]

#only D protocol
datos <- datos[datos$setup == "D",]

###MEAN RATE -----

#function to get data for each protocol
getData <- function(x){
  wd <- datos[datos$protocol== x,]
  #subset D setup
  wd <- wd[wd$setup == "D",]
  #session as numeric
  wd$session <- as.numeric(wd$session)
  
  # Normalize time
  IDs <- unique(wd$Rat)
  Nids <- length(IDs)
  wd$time <- 0
  for(i in 1:Nids){
    wwd <- wd[wd$Rat == IDs[i],]
    maxTime <- max(wwd$session)
    minTime <- min(wwd$session)
    wd$time[wd$Rat == IDs[i]] <- (wd$session[wd$Rat == IDs[i]] - minTime ) / (maxTime-minTime)
    
  }
  
  
  #select variables
  varSel <- c("ID", "Rat", "protocol", "session", "stimuli", "rewarded", "beginPress", "rate", "offset", "time")
  wd <- wd[,varSel]
  
  
  return(wd)
}

datAll <- lapply(2:5, getData )
names(datAll) <- paste0("protocol_", 2:5)


#function to get plots:
getPlot <- function(x, mY){
  wd <- datAll[[paste0("protocol_",x)]]
  #MEAN DATA
  adata <- aggregate(rate ~ stimuli + time, data = wd, mean)
  adata$se <- aggregate(rate ~ stimuli + time, data = wd, sd)$rate /
    aggregate(rate ~ stimuli + time, data = wd, length)$rate
  
  # ggplot(data = adata) +
  #   geom_point(aes(x=time, y=rate, color = stimuli, group = stimuli)) +
  #   geom_errorbar(aes(x=time, ymax=rate+se, ymin=rate-se, color = stimuli, group = stimuli))
  
  #MODEL
  fit <- glmer(rate ~ stimuli*time + (1|Rat), data=wd, family=binomial("logit"), weights = offset)
  
  if(x == 2){
    ordertitle <- "nd"
  } else if(x == 3){
    ordertitle <- "rd"
  } else {
    ordertitle <- "th"
  }
  
  mtitle <- paste0(x,ordertitle, " Configuration")
  
  #predictions
  pred <- ggemmeans(fit, terms = c("time [all]", "stimuli"), type = "fe", ci.lvl = 1-alphaci, back.transform = TRUE)
  adata$group <- factor(adata$stimuli, levels = c("Reward_Dynamic", "Reward_Static","NonRew_Dynamic", "NonRew_Static"))
  levels(adata$group) <- c("Dynamic REW", "Static REW", "Dynamic non-REW", "Static non-REW")
  pred$color <- factor(pred$group, levels = c("Reward_Dynamic", "Reward_Static","NonRew_Dynamic", "NonRew_Static"))
  levels(pred$color) <- c("Dynamic REW", "Static REW", "Dynamic non-REW", "Static non-REW")
  pred$group <- factor(pred$group, levels = c("Reward_Dynamic", "Reward_Static","NonRew_Dynamic", "NonRew_Static"))
  levels(pred$group) <- c("Dynamic REW", "Static REW", "Dynamic non-REW", "Static non-REW")
  
  plotProt <- ggplot() +
    geom_point(data=adata,aes(x=time, y=rate, color = group, group = group), size = 0.5) +
    geom_errorbar(data=adata,aes(x=time, ymax=rate+se, ymin=rate-se, color = group, group = group), width = 0.05) +
    geom_line(data=pred, aes(x=x, y=predicted, group = group, color = group)) +
    geom_ribbon(data=pred, aes(x=x, ymax=conf.high, ymin=conf.low, group=color, fill=color), alpha = 0.15) +
    scale_color_manual(values = color, guide = guide_legend(byrow = TRUE, override.aes = list(linetype = 1, shape = NA, linewidth = 3))) +
    scale_fill_manual(values = color, guide = "none") +
    ylab("lever pressing prob") +
    xlab("Normalized time \n (0 = first session, 1=last session)") +
    labs(title = mtitle) +
    scale_x_continuous( limits = c(0-0.05, 1+0.05), n.breaks = 5, expand = c(0,0)) +
    scale_y_continuous(limits = c(0, mY), n.breaks = 4, expand = c(0,0)) +
    theme(legend.title = element_blank(),
          axis.title.x = element_text(margin = margin(t=2, r=0, b=0, l=0, unit = "mm"), size = 16),
          axis.title.y = element_text(margin = margin(t=0, r=2, b=0, l=0, unit = "mm"), size = 16, vjust = 1),
          plot.margin = margin(t=10, r=10, b=10, l=10, unit = "mm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(colour = "black", linewidth =  1),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 14, hjust = 0.5, vjust = -0.05))
  
  
  
  
  return(plotProt)
  
}

#get max y range
maxY <- max(sapply(names(datAll), function(x) {
  wm <- aggregate(rate~stimuli+time, data = datAll[[x]], mean)$rate
  ws <- aggregate(rate~stimuli+time, data = datAll[[x]], sd)$rate /
    aggregate(rate~stimuli+time, data = datAll[[x]], length)$rate
  return(max(wm+ws))
}
)
)
### get plots
allPlots <- lapply(2:5, getPlot, maxY)

p2 <- allPlots[[1]] + theme(legend.position = "bottom", plot.margin=unit(c(0,7,0,0), "pt"), axis.line.y = element_line(colour = "black", linewidth = 1)) + labs(x=NULL)
p3 <- allPlots[[2]] + theme(legend.position = "none", plot.margin=unit(c(0,7,0,7), "pt")) + labs(x=NULL, y = NULL) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank() )
p4 <- allPlots[[3]] + theme(legend.position = "none", plot.margin=unit(c(0,7,0,7), "pt")) + labs(x=NULL, y = NULL) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank() )
p5 <- allPlots[[4]] + theme(legend.position = "none", plot.margin=unit(c(0,7,0,7), "pt")) + labs(x=NULL, y = NULL) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank() )

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p2)
bottom <- textGrob("Normalized time (0=first session, 1=last session)", gp = gpar(fontsize = 10))
top=textGrob("VSD Task - dynamic version: Mean Probability",
             gp = gpar(col = "black", fontsize = 20))

plotTime <- grid.arrange(arrangeGrob(p2+theme(legend.position = "none"),p3, p4, p5, top = top, bottom = bottom,
                                     nrow = 1, ncol = 4, widths = c(1.2,1,1,1)),
                         mylegend, nrow=2,heights=c(10, 2))

ggsave("Figure all protocols time.tiff", plot = plotTime, units="mm", width=190, height=100, dpi=600, compression = 'lzw')


#### HR AND MEDIAN SURVIVAL TIME ----

firstLever.st <- function(wIDst, wwinput, durcens){
  # print(IDst)
  w3input <- wwinput[wwinput$stID.total == wIDst,]
  w3input$time <- w3input$ElapsedTime - w3input$ElapsedTime[1]
  #find first lever press
  wfirst <- w3input[w3input$beginPress == 1,][1,]
  
  #if exist
  if (!is.na(wfirst$time)){
    wfirst["censored"] <- 1
    # sdata <<- rbind(sdata, wfirst)
    
    #if not, add data, set time to max time and censored data (=1)
  } else if (is.na(wfirst$time)){
    
    #create array
    wfirst <- w3input[1,]
    
    #add time
    wfirst["time"] <- durcens
    #censoring data
    wfirst["censored"] <- 0
    # sdata <<- rbind(sdata, wfirst)
    
    
  } else {
    print("something is wrong")
  }
  
  return(wfirst)
}


###function to apply over all sessions of a protocol
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
  byRat <- lapply(2:5, overProt, protR, r, pInf)
  names(byRat) <- paste0("protocol_",2:5)
  return(byRat)
  
}
#information about protocols
protInf <- readRDS("protocols.rds")$summary
#select rat
#get rats id
RatsS <- unique(datos$Rat)

survDat <- lapply(RatsS, overRat, protInf)
names(survDat) <- RatsS


## merge protocols
protocols <- paste0("protocol_", 2:5)

byProtocols <- vector(mode="list", length = 4)
names(byProtocols) <- protocols

for(i in 1:length(byProtocols)){
  byProtocols[[i]] <- do.call(rbind, lapply(RatsS, function(x) survDat[[x]][[protocols[i]]]))
  
}

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
coxByProt <- function(prot, sdata){
  sdata <- sdata[[prot]]
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

datPlotSurv <- lapply(names(byProtocols), coxByProt, byProtocols)

plotHR <- function(prot, sdat, mY){
  plotDat <- sdat[[prot]]$HR
  plotDat$stimuli <- as.character(plotDat$stimuli)
  plotDat$stimuli <- factor(plotDat$stimuli, levels = c("Reward_Dynamic", "Reward_Static", "NonRew_Dynamic", "NonRew_Static"))
  levels(plotDat$stimuli) <- c("Dynamic REW", "Static REW", "Dynamic non-REW", "Static non-REW")
  
  prot <- prot + 1
  
  if(prot == 2){
    ordertitle <- "nd"
  } else if(prot == 3){
    ordertitle <- "rd"
  } else {
    ordertitle <- "th"
  }
  
  mtitle <- paste0(prot,ordertitle, " Configuration")
  
  
  pHR <- ggplot(data = plotDat) +
    geom_point(aes(x=NormSes, y=response, group = stimuli, color = stimuli), size = 0.5) +
    geom_smooth(aes(x=NormSes, y=response, group = stimuli, color = stimuli), method=lm, se=FALSE ) +
    geom_errorbar(aes(x=NormSes, ymin = response - SE, ymax = response + SE, group = stimuli, color = stimuli), width = 0.05) +
    scale_color_manual(values = color, guide = guide_legend(byrow = TRUE, override.aes = list(linetype = 1, shape = NA, linewidth = 3))) +
    scale_fill_manual(values = color, guide = "none") +
    scale_x_continuous( limits = c(0-0.05, 1+0.05), n.breaks = 5, expand = c(0,0)) +
    scale_y_continuous(limits = c(0, mY), n.breaks = 4, expand = c(0,0)) +
    xlab("Normalized time \n (0 = first session, 1=last session)") +
    ylab("Hazard Ratio") +
    labs(title = mtitle) +
    theme(legend.title = element_blank(),
          axis.title.x = element_text(margin = margin(t=2, r=0, b=0, l=0, unit = "mm"), size = 16),
          axis.title.y = element_text(margin = margin(t=0, r=2, b=0, l=0, unit = "mm"), size = 16, vjust = 1),
          plot.margin = margin(t=10, r=10, b=10, l=10, unit = "mm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(colour = "black", linewidth =  1),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 14, hjust = 0.5, vjust = -0.05)) 
  
  
  
  
  
  return(pHR)
}

plotMedian <- function(prot, sdat, mY, MY){
  plotDat <- sdat[[prot]]$medianLife
  plotDat$stimuli <- as.character(plotDat$stimuli)
  plotDat$stimuli <- factor(plotDat$stimuli, levels = c("Reward_Dynamic", "Reward_Static", "NonRew_Dynamic", "NonRew_Static"))
  levels(plotDat$stimuli) <- c("Dynamic REW", "Static REW", "Dynamic non-REW", "Static non-REW")
  names(plotDat) <- c("median", "low", "high", "stimuli", "NormSes")
  
  prot <- prot + 1
  
  if(prot == 2){
    ordertitle <- "nd"
  } else if(prot == 3){
    ordertitle <- "rd"
  } else {
    ordertitle <- "th"
  }
  
  mtitle <- paste0(prot,ordertitle, " Configuration")
  
  pM <- ggplot(data = plotDat) +
    geom_point(aes(x=NormSes, y=median, group = stimuli, color = stimuli), size = 0.5) +
    geom_smooth(aes(x=NormSes, y=median, group = stimuli, color = stimuli), method=lm, se=FALSE ) +
    geom_errorbar(aes(x=NormSes, ymin = low, ymax = high, group = stimuli, color = stimuli), width = 0.05) +
    scale_color_manual(values = color, guide = guide_legend(byrow = TRUE, override.aes = list(linetype = 1, shape = NA, linewidth = 3))) +
    scale_fill_manual(values = color, guide = "none") +
    scale_x_continuous( limits = c(0-0.05, 1+0.05), n.breaks = 5, expand = c(0,0)) +
    scale_y_continuous(limits = c(mY, MY), n.breaks = 4, expand = c(0,0)) +
    xlab("Normalized time \n (0 = first session, 1=last session)") +
    ylab("Median Survival Time (ms)") +
    labs(title = mtitle) +
    theme(legend.title = element_blank(),
          axis.title.x = element_text(margin = margin(t=2, r=0, b=0, l=0, unit = "mm"), size = 16),
          axis.title.y = element_text(margin = margin(t=2, r=2, b=0, l=0, unit = "mm"), size = 16, vjust = 1),
          plot.margin = margin(t=10, r=10, b=10, l=10, unit = "mm"),
          panel.background = element_rect(fill = "white"),
          axis.line.x = element_line(colour = "black", linewidth =  1),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          plot.title = element_text(size = 14, hjust = 0.5, vjust = -0.05))
  
  return(pM)
}

#get max y range
maxY_hr <- max(sapply(1:4, function(x) {max(datPlotSurv[[x]]$HR$response + datPlotSurv[[x]]$HR$SE)} ) )
maxY_med <- max(sapply(1:4, function(x) {max(datPlotSurv[[x]]$medianLife[,3])} ) )
minY_med <- min(sapply(1:4, function(x) {min(datPlotSurv[[x]]$medianLife[,2])} ) )

pHR <- lapply(1:4, plotHR, datPlotSurv, maxY_hr)
pMed <- lapply(1:4, plotMedian, datPlotSurv, minY_med, maxY_med)


#join
### HR
p2 <- pHR[[1]] + theme(legend.position = "bottom", plot.margin=unit(c(0,7,0,0), "pt"), axis.line.y = element_line(colour = "black", linewidth = 1)) + labs(x=NULL)
p3 <- pHR[[2]] + theme(legend.position = "none", plot.margin=unit(c(0,7,0,7), "pt")) + labs(x=NULL, y = NULL) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank() )
p4 <- pHR[[3]] + theme(legend.position = "none", plot.margin=unit(c(0,7,0,7), "pt")) + labs(x=NULL, y = NULL) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank() )
p5 <- pHR[[4]] + theme(legend.position = "none", plot.margin=unit(c(0,7,0,7), "pt")) + labs(x=NULL, y = NULL) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank() )

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p2)

bottom <- textGrob("Normalized time (0=first session, 1=last session)", gp = gpar(fontsize = 10))
top=textGrob("VSD Task - dynamic version: Hazard Ratio",
             gp = gpar(col = "black", fontsize = 20))

plotHR <- grid.arrange(arrangeGrob(p2+theme(legend.position = "none"),p3, p4, p5, 
                                   top=top, bottom = bottom, nrow = 1, ncol = 4, widths = c(1.2,1,1,1)),
                       mylegend, nrow=2,heights=c(10, 2))

ggsave("Figure all protocols HR.tiff", plot = plotHR, units="mm", width=190, height=100, dpi=600, compression = 'lzw')


### Median

p2 <- pMed[[1]] + theme(legend.position = "bottom", plot.margin=unit(c(5,7,0,0), "pt"), axis.line.y = element_line(colour = "black", linewidth = 1)) + labs(x=NULL)
p3 <- pMed[[2]] + theme(legend.position = "none", plot.margin=unit(c(5,7,0,7), "pt")) + labs(x=NULL, y = NULL) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank() )
p4 <- pMed[[3]] + theme(legend.position = "none", plot.margin=unit(c(5,7,0,7), "pt")) + labs(x=NULL, y = NULL) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank() )
p5 <- pMed[[4]] + theme(legend.position = "none", plot.margin=unit(c(5,7,0,7), "pt")) + labs(x=NULL, y = NULL) + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank() )

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

mylegend<-g_legend(p2)

bottom <- textGrob("Normalized time (0=first session, 1=last session)", gp = gpar(fontsize = 10))
top=textGrob("VSD Task - dynamic version: Median Time",
             gp = gpar(col = "black", fontsize = 20))

plotMedian <- grid.arrange(arrangeGrob(p2+theme(legend.position = "none"),p3, p4, p5, 
                                       top=top, bottom = bottom, nrow = 1, ncol = 4, widths = c(1.2,1,1,1)),
                           mylegend, nrow=2,heights=c(10, 2))

ggsave("Figure all protocols Median Surv.tiff", plot = plotMedian, units="mm", width=190, height=100, dpi=600, compression = 'lzw')
