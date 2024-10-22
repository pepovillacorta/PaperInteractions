#Analyze last trainning session

### setup -----
#clear workspace
rm(list = ls())
#clear ram
gc()
#clear console
cat("\014")

#set path
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

#get alpha
zalpha = 1.0 #1.96 for 95% ci, 1.0 for se
alphaci <- 2*pnorm(zalpha, lower.tail = FALSE)

### DATA ------

#Mean Rate
datos <- readRDS("overallRate.rds")
dsave <- datos

#subset D setup
datos <- datos[datos$experiment == "D",]

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

##function to get last trainning session data
getLast <- function(id) {
  w <- dsave[dsave$Rat == id,]
  w <- w[w$session == max(w$session),]
  return(w)
  
}

dsave$session <- as.numeric(dsave$session)

dataLast <- as.data.frame(do.call(rbind, lapply(IDs, getLast)))


#subset D setup
dataLast <- dataLast[dataLast$experiment == "D",]

# id of Rats
IDs <- unique(dataLast$Rat)
Nids <- length(IDs)

ScenesCycle_5 <- c(1,5,2,5,3,5,4,5,2,5,3,5,4,5,2,5,4,5,1,5,4,5,3,5,2,5,4,5,1,5,2,5,4,5,2,5,
                   1,5,2,5,3,5,4,5,2,5,3,5,4,5,2,5,4,5,1,5,4,5,3,5,2,5,4,5,1,5,2,5,4,5,2,5,
                   1,5,2,5,3,5,4,5,2,5,3,5,4,5,2,5,4,5,1,5,4,5,3,5,2,5,4,5,1,5,2,5,4,5,2,5,
                   1,5,2,5,3,5,4,5,2,5,3,5,4,5,2,5,4,5,1,5,4,5,3,5,2,5,4,5,1,5,2,5,4,5,2,5,
                   1,5,2,5,3,5,4,5,2,5,3,5,4,5,2,5,4,5,1,5,4,5,3,5,2,5,4,5,1,5,2,5,4,5,2,5,
                   1,5,2,5,3,5,4,5,2,5,3,5,4,5,2,5,4,5,1,5,4,5,3,5,2,5,4,5,1,5,2,5,4,5,2,5)


# repetitions in dynamic stimuli are taken as the same stimulus so:
ScenesTime_5 <- c(15, 15, 15, 15)

SceneNames_5 <- c("Reward_Static", "NonRew_Static", 
                  "Reward_Dynamic", "NonRew_Dynamic")

names(ScenesTime_5) <- SceneNames_5

#check
Nrep_5 <- table(as.factor(ScenesCycle_5))

#get real repetitions of each stimuli type
Nrep2_5 <- aggregate(stID.within ~ stimuli, data = dataLast, max)

#and multiply by time of each stimuli
#"subsetting" to order:
Nrep2m <- Nrep2_5[,2]
names(Nrep2m) <- Nrep2_5[,1]
Nrep2m <- Nrep2m[SceneNames_5]
timem <- ScenesTime_5[SceneNames_5]
total.time <- Nrep2m * timem
names(total.time) <- SceneNames_5




#aggregate
adata.Lea <- aggregate(beginPress ~ ID+Rat+session+stimuli+type, data = dataLast, sum)
adata.Lea$offset <- 0
adata.Lea$offset <- apply(adata.Lea, 1, function(x) x["offset"] <- total.time[names(total.time) == x["stimuli"]] )
adata.Lea$rate <- adata.Lea$beginPress / adata.Lea$offset
adata.Lea$stimuli <- as.factor(adata.Lea$stimuli)


#First lever pressing


#function to get first lever
firstLever2 <- function(IDname, input){
  print(IDname)
  winput <- input[input$ID == IDname,]
  IDst <- unique(winput$stID.total)
  sapply(IDst, firstLever.st2, winput)
  return()
}
firstLever.st2 <- function(wIDst, wwinput){
  # print(IDst)
  w3input <- wwinput[wwinput$stID.total == wIDst,]
  w3input$time <- w3input$ElapsedTime - w3input$ElapsedTime[1]
  #find first lever press
  wfirst <- w3input[w3input$beginPress == 1,][1,]
  
  #if exist
  if (!is.na(wfirst$time)){
    wfirst["censored"] <- 1
    sdata2 <<- rbind(sdata2, wfirst)
    
    #if not, add data, set time to max time and censored data (=1)
  } else if (is.na(wfirst$time)){
    
    #create array
    wfirst <- array(0, dim = ncol(w3input))
    #assign names
    names(wfirst) <- names(w3input)
    #add data
    wfirst[c("ID", "Rat", "session")] <- w3input[1, c("ID", "Rat", "session") ]
    #add more data
    wfirst[c("stimuli", "type", "stID.total", "stID.within")] <- w3input[1, c("stimuli", "type", "stID.total", "stID.within")]
    #add time
    if(wfirst["stimuli"] == "Blank3") {
      wfirst["time"] <- 3000
    } else if (wfirst["stimuli"] == "Blank5") {
      wfirst["time"] <- 5000
    } else if (wfirst["type"] != "B"){
      wfirst["time"] <- 15000
    } else {
      print("Something is wrong with type")
    }
    #censoring data
    wfirst["censored"] <- 0
    sdata2 <<- rbind(sdata2, wfirst)
    
    
  } else {
    print("something is wrong")
  }
  
  return()
}


#Probability distribution
setwd("C:/Users/user/OneDrive/DavidComportamiento/trainning")
bdata2 <- readRDS("Behavioral_Data_extended_lastSession.RDS")

#name of recordings
rec_names2 <- unique(bdata2$ID)

#remove -1
bdata2 <- bdata2[bdata2$Scene != -1,]

#number of different recordings
Nid2 <- length(rec_names2)


for(i in 1:length(rec_names2)){
wdat <- bdata2[bdata2$ID == rec_names2[i],]
max_st <- max(wdat$stID.total)
for(j in 1:max_st){
  wwdat <- wdat[wdat$stID.total==j,]
  wtime <- wwdat$ElapsedTime - wwdat$ElapsedTime[1]
  bdata2$time[bdata2$ID == rec_names2[i] & bdata2$stID.total == j] <- wtime
}
}


#names of variables in data
var_names2 <- names(bdata2)
sdata2 <- as.data.frame(matrix(NA, nrow = 0, ncol = (ncol(bdata2)+1)))
lapply(rec_names2, firstLever2, bdata2)
sdata2 <- sdata2[sdata2$stimuli != "Blank3",]

data.surv.Lea <- sdata2


########## MODELS #######################


### Mean probability ----

#Familiar
#check random effects
baseFinal <- glm(rate ~ 1, data = adata.Lea, family = binomial("logit"), weights = offset)
finalI <- glmer(rate ~ 1 + (1|Rat), data = adata.Lea, family = binomial("logit"), weights = offset)
finalIS <- glmer(rate ~ 1 + (1|Rat/session), data = adata.Lea, family = binomial("logit"), weights = offset)


anova(finalI,baseFinal)
anova(finalIS, finalI)


#include Rat 
fit.Lea <- glmer(rate ~ stimuli + (1|Rat), data = adata.Lea, family = binomial("logit"), weights = offset )

pred.Lea <- ggemmeans(fit.Lea, terms = c("stimuli"), type = "fe", ci_level = 1-alphaci)

#effects
effects.Lea <- emmeans(fit.Lea,  "stimuli",  max.degree = 1, type = "response")
peffects.Lea <- summary(effects.Lea, infer = c(TRUE), null = 0)

#comparisons
#p-values
pcomp.Lea <-summary(contrast(effects.Lea, method = "tukey"))


### PLOT


#reorder levels for coloring

pred.Lea$x <- factor(pred.Lea$x, levels = c("Reward_Dynamic", "Reward_Static", "NonRew_Dynamic", "NonRew_Static"))

#reorder for plot
levels(pred.Lea$x) <- c("Dynamic REW", "Static REW", "Dynamic non-REW", "Static non-REW")


xmin <- 0.0
xMax <- 0.6

plot.Lea <- ggplot(pred.Lea) +
  geom_point(aes(x=x, y=predicted, group = x, color = x), alpha = 1, size = 3) +
  geom_errorbar(aes(x=x, ymax = conf.high, ymin=conf.low, group = x, color = x, width = 0.3), linewidth = 0.75) +
  scale_color_manual(values = color, guide = guide_legend(label.position = "top", byrow = FALSE, override.aes = list(linetype = 1, shape = NA, linewidth = 5)) ) +
  ylab("lever pressing prob") +
  scale_x_discrete(expand = c(0.1,0.1)) +
  scale_y_continuous(limits = c(xmin,xMax), expand = c(0,0)) +
  labs(x="stimulus type") +
  geom_text(x=2.5, y=0.55, label = "#", color = "black", size = 7) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(color = "white"),
        axis.ticks.x = element_line(color = "white"),
        legend.spacing.x = unit(15, 'mm'),
        legend.justification = c(0,0),
        legend.position="bottom",
        legend.key.width = unit(2, "cm"),
        legend.key = element_rect(fill = NA),
        legend.text = element_text(size = 6)
        
  ) 

setwd("C:/Users/user/OneDrive/DavidComportamiento") 
saveRDS(plot.Lea, "MeanRate_Trainning_familiar.rds")
write.csv2(pred.Lea, "fig2_panel_C.csv")



### SURVIVAL MODELS ---
data.surv.Lea$stimuli <- as.factor(data.surv.Lea$stimuli)
surv.mod.L <- coxph(Surv(time=time, event = censored, type = "right") ~ stimuli, 
                    id = ID, data = data.surv.Lea)
car::Anova(surv.mod.L, test.statistic = "Wald")

#comparisons
pair.surv.lea <- emmeans(surv.mod.L,  ~stimuli)
v <- summary(pair.surv.lea, type = "response")


#p-values
p.pair.surv.lea <-summary(contrast(pair.surv.lea, method = "tukey"))


#median survival time
stimType <- unique(data.surv.Lea$stimuli)
Med.Lea <- as.data.frame(
  do.call(rbind, lapply(stimType, function(x) 
    summary(
      survfit(Surv(time, censored) ~ 1, data = data.surv.Lea[data.surv.Lea$stimuli==x,]))$table[7:9]
  )
  )
)
Med.Lea$stimuli <- stimType


### PLOT Survival ----

surv.eff.L <- ggpredict(surv.mod.L, terms = c("stimuli"), type = "surv", 
                        ci_level = 1-alphaci)
surv.eff.L$predicted <- 1-surv.eff.L$predicted
surv.eff.L$conf.high <- 1-surv.eff.L$conf.high
surv.eff.L$conf.low <- 1-surv.eff.L$conf.low
surv.eff.L$group <- factor(surv.eff.L$group, levels = c("Reward_Dynamic", "Reward_Static", "NonRew_Dynamic", "NonRew_Static"))
levels(surv.eff.L$group) <- c("Dynamic REW", "Static REW", "Dynamic non-REW", "Static non-REW")
surv.eff.L$x <- surv.eff.L$x / 1000

xmin <- 0
xMax <- 1

surv.plot.L <- ggplot(surv.eff.L, aes(x=x, y=predicted, group = group, color = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high, group = group, fill = group), alpha = 0.15, linetype = 0) +
  scale_color_manual(values = color) +
  scale_fill_manual(values = color) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(xmin, xMax), expand=c(0,0)) +
  xlab("time from stimulus onset (s)") +
  ylab("1st lever press prob") +
  geom_text(x=7.5, y=0.65, label = "#", color = "black", size = 7)  +
  theme(axis.title.x = element_text(margin = margin(t=0, r=0, b=5, l=0, unit = "mm"),
                                    size = 9)
  )

setwd("C:/Users/user/OneDrive/DavidComportamiento")
saveRDS(surv.plot.L, "Survival_Trainning_familiar.rds")
write.csv2(surv.eff.L, "fig2_panel_E.csv")


### RATE distribution ----
#group times each N
tgdata <- aggregate(beginPress ~ ID + Rat + session + stimuli + time + type + stID.within, data = bdata2, sum)
tgdata$each <- 0
bin.g <- 5


for(j in 1:length(rec_names2)){
  for(i in 1:length(SceneNames_5)){
    for(h in 1:max(tgdata$stID.within[tgdata$stimuli == SceneNames_5[i] & tgdata$ID == rec_names2[j]]))
      tgdata$each[tgdata$stimuli == SceneNames_5[i] & tgdata$ID == rec_names2[j] & tgdata$stID.within == h] <- rep(
        seq(1,length(tgdata$each[tgdata$stimuli == SceneNames_5[i] & tgdata$ID == rec_names2[j] & tgdata$stID.within == h] ) / bin.g ), each = bin.g)
  }
}


#remove blanks
tgdata <- tgdata[tgdata$type != "NA",]

#check offset

t.group <- aggregate(beginPress ~ ID + Rat + session + stimuli + type + each, data = tgdata, sum)

t.offset <- aggregate(beginPress ~ ID + Rat + session + stimuli + type + each, data = tgdata, length)

t.group$time <- t.group$each * 40 * bin.g
t.group$offset <- t.offset$beginPress * 40 * bin.g  #each 1 ms
t.group$rate <- t.group$beginPress / t.group$offset


t.gea <- t.group
t.gea$time <- as.numeric(t.gea$time)

#model
baseT <- glm(rate ~ 1, family = binomial("logit"), data = t.gea, weights = offset)
fitI <- glmer(rate ~ 1 + (1|Rat), family = binomial("logit"), data = t.gea, weights = offset)
fitIS <- glmer(rate ~ 1 + (1|Rat/session), family = binomial("logit"), data = t.gea, weights = offset)
fitIST <- glmer(rate ~ 1 + (1+time|Rat/session), family = binomial("logit"), data = t.gea, weights = offset)

anova(fitI, baseT)
anova(fitIS, fitI)
anova(fitIST, fitIS)
# anova(fitISTT, fitIST)


t.gea$stimuli <- as.factor(t.gea$stimuli)
fitTime.G <- glmer(rate ~ stimuli * poly(time, degree = 2) + (1|Rat/session), 
                   family = binomial("logit"), data = t.gea, weights = offset)


#comparisons
#pairwise comparisons: Tukey test
by.time.gea <- emtrends(fitTime.G,  "stimuli", "time", max.degree = 2)
sum.by.time.gea <- summary(by.time.gea, type = "response")

#check effects of each estimuli
by.time.dyREW <- glmer(rate ~ poly(time, degree = 2) + (1|Rat/session), 
                       family = binomial("logit"), data = t.gea[t.gea$stimuli == "Reward_Dynamic",], weights = offset)
by.time.stREW <- glmer(rate ~ poly(time, degree = 2) + (1|Rat/session), 
                       family = binomial("logit"), data = t.gea[t.gea$stimuli == "Reward_Static",], weights = offset)

by.time.dyNon <- glmer(rate ~ poly(time, degree = 2) + (1|Rat/session), 
                       family = binomial("logit"), data = t.gea[t.gea$stimuli == "NonRew_Dynamic",], weights = offset)
by.time.stNon <- glmer(rate ~ poly(time, degree = 2) + (1|Rat/session), 
                       family = binomial("logit"), data = t.gea[t.gea$stimuli == "NonRew_Static",], weights = offset)

#por ms

#p-values
p.by.time.gea <-summary(contrast(by.time.gea, method = "tukey"))



time.effects.lea <- ggpredict(fitTime.G, terms = c("time[all]", "stimuli"), 
                              type = "fe")

data.grouped.L <- aggregate(rate ~ time + stimuli + type, data = t.group, mean)

names(data.grouped.L)[2] <- "group"
time.effects.lea$x <- time.effects.lea$x / 1000
data.grouped.L$time <- data.grouped.L$time / 1000

#labels
stimLab.L <- c('Dynamic REW', 'Static REW', 'Dynamic non-REW', 'Static non-REW')
groupLab.L <- c('Dynamic REW', 'Static REW', 'Dynamic non-REW', 'Static non-REW')

### Plot dist ----
time.effects.lea$group <- factor(time.effects.lea$group, 
                                 levels = c("Reward_Dynamic", "Reward_Static", "NonRew_Dynamic", "NonRew_Static"))
levels(time.effects.lea$group) <- c("Dynamic REW", "Static REW", "Dynamic non-REW", "Static non-REW")


data.grouped.L$group <- factor(data.grouped.L$group, 
                               levels = c("Reward_Dynamic", "Reward_Static", "NonRew_Dynamic", "NonRew_Static"))
levels(data.grouped.L$group) <- c("Dynamic REW", "Static REW", "Dynamic non-REW", "Static non-REW")

xmin <- 0
xMax <- 0.00019
maxExp <- floor(log10(max(xMax)))

#data.frame for asterisc
dat.as.L <- data.frame(label = "#", x=15, y=0, group="Dynamic REW")

dat.exp <- data.frame(group=c("Dynamic REW", "Dynamic non-REW"), lab = paste0("x10^{",maxExp,"}"), y = xMax ) 


plot.time.lea <-ggplot() +
  geom_line(data = time.effects.lea, aes(x=x, y=predicted, colour = group), linewidth = 1) +
  geom_col(data = data.grouped.L, aes(x=time, y = rate, group = group, fill = group), alpha = 0.5) +
  geom_text(data = dat.as.L, aes(x=x, y=y, label=label), size = 7, color = "black", hjust = -0.7) +
  ylab("lever pressing prob") +
  xlab("time from stimulus onset (s)") +
  geom_text(data=dat.exp, aes(x=0, y=y, group = group, label = lab), parse = "TRUE", size = 5, hjust = 0.5, vjust = -0.5) +
  scale_x_continuous( limits = c(0, 15), n.breaks = 3, expand = c(0,0)) +
  scale_y_continuous(limits = c(xmin, xMax), n.breaks = 6, labels = function(x) x/10^maxExp, expand = c(0,0)) +
  coord_cartesian(xlim = c(0, 15), ylim = c(xmin, xMax), clip = "off") +
  scale_fill_manual(values = color, guide = "none") +
  scale_color_manual(values = color, labels = stimLab.L) +
  facet_wrap(~factor(group, levels = c("Dynamic REW", "Static REW", "Dynamic non-REW", "Static non-REW")), nrow = 2) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(margin = margin(t=2, r=5, b=0, l=0, unit = "mm")),
    plot.margin = unit(c(0,5,5,0), "mm"),
    panel.spacing.y = unit(2, "lines"),
    panel.spacing.x = unit(2, "lines")
  )

setwd("C:/Users/user/OneDrive/DavidComportamiento")
saveRDS(plot.time.lea, "RateDistribution_Trainning_familiar.rds")
write.csv2(time.effects.lea, "fig2_panel_D_fit.csv")
write.csv2(data.grouped.L, "fig2_panel_D_histogram.csv")

