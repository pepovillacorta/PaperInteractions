#Analize Generalization phase of TG protocol

### setup -----
#clear workspace
rm(list = ls())
#clear ram
gc()
#clear console
cat("\014")



#libraries
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggeffects)
library(gridExtra)
library(grid)
library(emmeans)
library(DHARMa)
library(glmmTMB)
library(parallel)
library(car)
library(survival)
library(coxme)
library(survminer)
library(scales)
library(dplyr)

#colors
coloresL <- c("darkorange", "darkblue")
coloresN <- c("purple", "darkgreen")

#get alpha
zalpha = 1.0 #1.96 for 95% ci, 1.0 for se
alphaci <- 2*pnorm(zalpha, lower.tail = FALSE)

### Mean Rate ----
setwd("C:/Users/user/OneDrive/DavidComportamiento")

#load data
bdata <- readRDS("Behavioral_Data_extended_TGProtocol.RDS")


ScenesCycle <- c( 1, 6, 4, 5, 2, 5, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 5,
                  4, 6, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 6, 3, 5, 2, 6,
                  1, 5, 2, 6, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 6, 4, 5,
                  10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 6, 2, 5, 3,
                  6, 4, 6, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 5, 2, 5, 8,
                  8, 8, 8, 8, 8, 8, 8, 8, 8, 6, 3, 6, 4, 5, 4, 5, 7, 
                  7, 7, 7, 7, 7, 7, 7, 7, 7, 5, 1, 5, 2, 6, 10, 10, 
                  10, 10, 10, 10, 10, 10, 10, 10, 6 )

# ScenesTime <- c( 15, 15, 15, 15, 3, 5, 1, 1, 1, 1 )
# repetitions in dynamic stimuli are taken as the same stimulus so:
ScenesTime <- c( 15, 15, 15, 15, 3, 5, 10, 10, 10, 10 )

SceneNames <- c("Reward_up", "NonRew_up", 
                "Reward_down", "NonRew_down",
                "Blank3", "Blank5",
                "Generalize_Reward_up", "Generalize_NonRew_up",
                "Generalize_Reward_down", "Generalize_NonRew_down")
names(ScenesTime) <- SceneNames


#check
Nrep <- table(as.factor(ScenesCycle))

#get real repetitions of each stimuli type
Nrep2 <- aggregate(stID.within ~ stimuli, data = bdata, max)

#and multiply by time of each stimuli
#"subsetting" to order:
Nrep2m <- Nrep2[,2]
names(Nrep2m) <- Nrep2[,1]
Nrep2m <- Nrep2m[SceneNames]
timem <- ScenesTime[SceneNames]
total.time <- Nrep2m * timem
names(total.time) <- SceneNames
#name of recordings
rec_names <- unique(bdata$Rat)

#remove -1
bdata <- bdata[bdata$Scene != -1,]

#number of different rats
Nid <- length(unique(bdata$Rat))

#names of variables in data
var_names <- names(bdata)


##split familiar from novel
familiar_stimuli <- c("Reward_up", "Reward_down", "NonRew_up", "NonRew_down")
familiar_R <- c("Reward_up", "Reward_down")
familiar_NR <- c("NonRew_up", "NonRew_down")

novel_stimuli <- c("Generalize_Reward_up", "Generalize_Reward_down","Generalize_NonRew_up", "Generalize_NonRew_down")
novel_R <- c("Generalize_Reward_up", "Generalize_Reward_down")
novel_NR <- c("Generalize_NonRew_up", "Generalize_NonRew_down")

bdata$Reward <- "R"
bdata$Reward[bdata$stimuli %in% familiar_NR] <-"NR"
bdata$Reward[bdata$stimuli %in% novel_NR] <- "NR"

#up or down
up_stimuli <- c("Reward_up", "NonRew_up", "Generalize_Reward_up", "Generalize_NonRew_up")
down_stimuli <- c("Reward_down", "NonRew_down", "Generalize_Reward_down", "Generalize_NonRew_down")
bdata$possition <- "up"
bdata$possition[bdata$stimuli %in% down_stimuli] <- "down"


#remove blanks
bdata <- bdata[bdata$type != "B",]


#aggregate
adata <- aggregate(beginPress ~ ID + Rat + session + designGroup + possition + stID.within + type + Reward + stimuli, data = bdata, sum)

#offset
#for familiar stimuli

  Loffset <- 15

binwithin <- 2 #length of the rate bin (1=1 second, 2=500 ms, etc)
#with binwithin == 1 some epochs showed rates > 1. Set to 2 and then rescale predictions
#in ordet to get rates as number of lever pressing each second (to fit remainder figures)
adata$offset <- Loffset * binwithin #each 500 ms
adata$offset[adata$type == "N"] <- 10 * binwithin

#rate
adata$rate <- adata$beginPress / adata$offset



#fit
fitOverall <- glmer(rate ~ type*Reward+designGroup+possition + (1|Rat), data = adata, family = binomial("logit"), weights = offset )
summary(fitOverall)

#get effects
pred <- ggemmeans(fitOverall, terms = c("type", "Reward"), type = "fe", ci.lvl = 1-alphaci)

#effects
effects <- emmeans(fitOverall,  c("type", "Reward"),  max.degree = 1, type = "response")


#comparisons
#p-values
pcomp <-summary(contrast(effects, method = "tukey"))

### PLOT 
#reorder levels for coloring
pred$stimtype <- paste0(pred$x,pred$group)
pred$stimtype <- as.factor(pred$stimtype)
levels(pred$stimtype) <- c("Familiar non-REW", "Familiar REW", "Compl non-REW", "Compl REW")
pred$stimtype <- factor(pred$stimtype, 
                        levels = c("Familiar REW", "Familiar non-REW", "Compl REW",  "Compl non-REW"))
pred <- as.data.frame(pred)


#rescale predictions
if(binwithin != 1){
  pred$predicted <- pred$predicted * binwithin
  pred$conf.low <- pred$conf.low * binwithin
  pred$conf.high <- pred$conf.high * binwithin
}


#find maximum
xMax <- max(pred$conf.high)
xmin <- min(pred$conf.low)

wdat <- pred[pred$x=="L",]
wdat <- droplevels(wdat)

#familiar
plot.Overall.L <- ggplot(wdat) +
  geom_point(aes(x=stimtype, y=predicted, group = stimtype, color = stimtype), alpha = 1, size = 3) +
  geom_errorbar(aes(x=stimtype, ymax = conf.high, ymin=conf.low, group = stimtype, color = stimtype, width = 0.1), linewidth = 0.75) +
  scale_color_manual(values = coloresL, guide = guide_legend(label.position = "top", byrow = FALSE, override.aes = list(linetype = 1, shape = NA, linewidth = 5)) ) +
  ylab("lever pressing prob") +
  scale_x_discrete(expand = c(0.1,0.1)) +
  scale_y_continuous(limits = c(xmin,xMax), expand = c(0,0.1)) +
  labs(x="stimulus type") +
  geom_text(x=1.25, y=0.49, label = "*", color = coloresL[1], size = 12) +
  # guides(color = guide_legend(override.aes = list(linetype=1))) +
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

saveRDS(plot.Overall.L, "MeanRate_TG_familiar.rds")

wdat <- pred[pred$x=="N",]
wdat <- droplevels(wdat)

#Generalized
plot.Overall.N <- ggplot(wdat) +
  geom_point(aes(x=stimtype, y=predicted, group = stimtype, color = stimtype), alpha = 1, size = 3) +
  geom_errorbar(aes(x=stimtype, ymax = conf.high, ymin=conf.low, group = stimtype, color = stimtype, width = 0.1), linewidth = 0.75) +
  scale_color_manual(values = coloresN, guide = guide_legend(label.position = "top", byrow = FALSE, override.aes = list(linetype = 1, shape = NA, linewidth = 5))) +
  ylab("lever pressing prob") +
  scale_x_discrete(expand = c(0.1,0.1)) +
  # coord_fixed(ratio=2.5) +
  scale_y_continuous(limits = c(xmin,xMax), expand = c(0,0.1)) +
  labs(x="stimulus type") +
  geom_text(x=1.25, y=0.49, label = "*", color = coloresN[1], size = 12) +
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

saveRDS(plot.Overall.N, "MeanRate_TG_novel.rds")

## rate distribution over stim time ----
#group times each N
tgdata <- aggregate(beginPress ~ ID + designGroup + type + Reward + possition + Rat + session + stimuli + time + stID.within, data = bdata, sum)
tgdata$each <- 0
bin.g <- 5

sel_stim <- unique(tgdata$stimuli)
rec_names <- unique(bdata$ID)

for(j in 1:length(rec_names)){ #over rats
  for(i in 1:length(sel_stim)){ #over stimuli types
    MaxStim <- max(tgdata$stID.within[tgdata$ID == rec_names[j] & tgdata$stimuli==sel_stim[i]])
    for(h in 1:MaxStim){ #over each stimli
      tgdata$each[tgdata$stimuli == sel_stim[i] & tgdata$ID == rec_names[j] & tgdata$stID.within == h] <- rep(
        seq(1,(length(tgdata$each[tgdata$stimuli == sel_stim[i] & tgdata$ID == rec_names[j] & tgdata$stID.within == h] )) / bin.g ), each = bin.g)
    }
  }
}


#check offset

t.group <- aggregate(beginPress ~ ID + designGroup + type + Reward + possition + Rat + session + stimuli + each, data = tgdata, sum)

t.offset <- aggregate(beginPress ~ ID + designGroup + type + Reward + possition+ Rat + session + stimuli  + each, data = tgdata, length)

t.group$time <- t.group$each * 40 * bin.g
t.group$offset <- t.offset$beginPress * 40 * bin.g #each 1 ms
t.group$rate <- t.group$beginPress / t.group$offset

t.group <- t.group[t.group$time > 0,]
t.group$time <- as.numeric(t.group$time)
t.group$Reward <- as.factor(t.group$Reward)
#model
fitTime <- glmer(rate ~ Reward * poly(time, degree = 2)*type + (1|Rat), 
                 family = binomial("logit"), data = t.group, weights = offset)



#comparisons
#pairwise comparisons: Tukey test
by.time.gea <- emtrends(fitTime, ~ Reward + type + degree | time, "time",  max.degree = 2)
sum.by.time.gea <- summary(by.time.gea, type = "response")


#p-values
p.by.time.gea <-summary(contrast(by.time.gea, method = "tukey"))

##split to check slopes significance


time.effects.gea <- ggeffect(fitTime, terms = c("time[all]", "Reward", "type"), type = "fe")


data.grouped <- aggregate(rate ~ time + Reward + type, data = t.group, mean)
names(data.grouped)[2] <- "group"
time.effects.gea$x <- time.effects.gea$x / 1000
data.grouped$time <- data.grouped$time / 1000

#labels
stimLab <- c('Dynamic REW',"Compl. dyn.")
groupLab <- c("Familiar REW","Familiar non-REW", "Compl REW", "Compl non-REW")

### Plot dist ----
time.effects.gea$stimulus <- paste0(time.effects.gea$group,time.effects.gea$facet)
time.effects.gea$stimulus <- factor(time.effects.gea$stimulus, levels = c("NRL", "RL", "NRN", "RN"))
levels(time.effects.gea$stimulus) <- c("Familiar non-REW", "Familiar REW", "Compl non-REW", "Compl REW")
time.effects.gea$stimulus <- factor(time.effects.gea$stimulus,
                                    levels = c("Familiar REW","Familiar non-REW", "Compl REW", "Compl non-REW"))

#create variable for histograms
data.grouped$stimulus <- paste0(data.grouped$group, data.grouped$type)
data.grouped$stimulus <- factor(data.grouped$stimulus, levels = c("NRL", "RL", "NRN", "RN"))
levels(data.grouped$stimulus) <- c("Familiar non-REW", "Familiar REW", "Compl non-REW", "Compl REW")
data.grouped$stimulus <- factor(data.grouped$stimulus, 
                                levels = c("Familiar REW","Familiar non-REW", "Compl REW", "Compl non-REW"))



##plot separately
#find maximum and minimum
xmin <- 0
xMax <- max(max(time.effects.gea$conf.low), max(data.grouped$rate))

xmin <- 0
xMax <- 0.00019

#familiar
#subset data
wdat <- time.effects.gea[time.effects.gea$facet=="L",]
wdat <- droplevels(wdat)

wdat2 <- data.grouped[data.grouped$type == "L",]
wdat2 <- droplevels(wdat2)

#data.frame for asterisc
dat.as <- data.frame(label = "*", x=0.5, y=0.00015, stimulus="Familiar REW")

#find maximun exponent
maxExp <- floor(log10(max(xMax)))

stimLab_L <- c("Familiar REW", "Familiar non-REW")

plot.time.gea.L <- ggplot() +
  geom_line(data = wdat, aes(x=x, y=predicted, colour = stimulus), linewidth = 1) +
  geom_col(data = wdat2, aes(x=time, y = rate, group = stimulus, fill = stimulus), alpha = 0.5) +
  geom_text(data = dat.as, aes(x=x, y=y, label=label, color = stimulus), size = 12) +
  ylab("lever pressing prob") +
  xlab("time from stimulus onset (s)") +
  annotate("text", label = paste0("x10^{",maxExp,"}"), x=0, y=xMax, size = 5, hjust = 0.5, vjust = -0.5, parse = TRUE) +
  scale_x_continuous(limits = c(0, 15), n.breaks = 3, expand = c(0,0)) +
  scale_y_continuous(limits = c(xmin, xMax), n.breaks = 6, labels = function(x) x/10^maxExp, expand = c(0,0)) +
  coord_cartesian(xlim = c(0, 15), ylim = c(xmin, xMax), clip = "off") +
  scale_fill_manual(values = coloresL, guide = "none") +
  scale_color_manual(values = coloresL, labels = stimLab_L) +
  facet_wrap(~factor(stimulus, levels = c("Familiar REW", "Familiar non-REW")), nrow = 2) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(margin = margin(t=2, r=5, b=0, l=0, unit = "mm")),
    plot.margin = unit(c(0,5,5,0), "mm"),
    panel.spacing.y = unit(2, "lines")
  )

saveRDS(plot.time.gea.L, "RateDistribution_TG_familiar.rds")

#generalized
wdat <- time.effects.gea[time.effects.gea$facet=="N",]
wdat <- droplevels(wdat)

wdat2 <- data.grouped[data.grouped$type == "N",]
wdat2 <- droplevels(wdat2)

stimLab_N <- c("Compl REW", "Compl non-REW")

#data.frame for asterisc
dat.as <- data.frame(label = "*", x=0.5, y=0.00015, stimulus="Compl REW")

plot.time.gea.N <- ggplot() +
  geom_line(data = wdat[wdat$x<=10,], aes(x=x, y=predicted, colour = stimulus), linewidth = 1) +
  geom_col(data = wdat2, aes(x=time, y = rate, group = stimulus, fill = stimulus), alpha = 0.5) +
  geom_text(data = dat.as, aes(x=x, y=y, label=label, color = stimulus), size = 12) +
  ylab("lever pressing prob") +
  xlab("time from stimulus onset (s)") +
  scale_x_continuous(limits = c(0, 15), n.breaks = 3, expand = c(0,0)) +
  scale_fill_manual(values = coloresN, guide = "none") +
  scale_color_manual(values = coloresN, labels = stimLab_N) +
  annotate("text", label = paste0("x10^{",maxExp,"}"), x=-Inf, y=xMax, size = 5, hjust = 0.5, vjust = -0.5, parse = TRUE) +
  coord_cartesian(xlim = c(0,15), y=c(xmin, xMax), clip = "off") +
  scale_y_continuous(limits = c(xmin, xMax), n.breaks = 6, labels = function(x) x/10^maxExp, expand = c(0,0)) +
  facet_wrap(~factor(stimulus,levels = c("Compl REW", "Compl non-REW")), nrow = 2) +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank(),
    legend.title = element_blank(),
    axis.title.x = element_text(size = 9),
    axis.title.y = element_text(margin = margin(t=2, r=5, b=0, l=0, unit = "mm")),
    plot.margin = unit(c(0,5,5,0), "mm"),
    panel.spacing.y = unit(2, "lines")
  )

saveRDS(plot.time.gea.N, "RateDistribution_TG_novel.rds")

##fit separately
##familiar
workdat <- t.group[t.group$type == "L",]
fitTime.L <- glmer(rate ~ Reward * poly(time, degree = 2) + (1|Rat), 
                   family = binomial("logit"), data = workdat, weights = offset)
Anova(fitTime.L)

##generalized
workdat <- t.group[t.group$type == "N",]
fitTime.N <- glmer(rate ~ Reward * poly(time, degree = 2) + (1|Rat), 
                   family = binomial("logit"), data = workdat, weights = offset)
Anova(fitTime.N)



### first lever pressing ----

#find the first lever of each stimuli
#function to get first lever
firstLever <- function(IDname, input){
  print(IDname)
  winput <- input[input$ID == IDname,]
  IDst <- unique(winput$stID.total)
  sapply(IDst, firstLever.st, winput)
  return()
}
firstLever.st <- function(wIDst, wwinput){
  # print(IDst)
  w3input <- wwinput[wwinput$stID.total == wIDst,]
  #find first lever press
  wfirst <- w3input[w3input$beginPress == 1,][1,]
  
  #if exist
  if (!is.na(wfirst$time)){
    wfirst["censored"] <- 1
    sdata <<- rbind(sdata, wfirst)
    
    #if not, add data, set time to max time and censored data (=1)
  } else if (is.na(wfirst$time)){
    
    #create array
    wfirst <- array(0, dim = ncol(w3input))
    #assign names
    names(wfirst) <- names(w3input)
    #add data
    wfirst[c("ID", "Rat", "session")] <- w3input[1, c("ID", "Rat", "session") ]
    #add more data
    wfirst[c("stimuli", "type", "stID.total", "stID.within", "Reward", "possition", "designGroup")] <- w3input[1, c("stimuli", "type", "stID.total", "stID.within", "Reward", "possition", "designGroup")]
    #add time
    if(wfirst["stimuli"] == "Blank3") {
      wfirst["time"] <- 3000
    } else if (wfirst["stimuli"] == "Blank5") {
      wfirst["time"] <- 5000
    } else if (wfirst["type"] != "B"){
      wfirst["time"] <- max(wwinput$time)
    } else {
      print("Something is wrong with type")
    }
    #censoring data
    wfirst["censored"] <- 0
    sdata <<- rbind(sdata, wfirst)
    
    
  } else {
    print("something is wrong")
  }
  
  return()
}

sdata <- as.data.frame(matrix(NA, nrow = 0, ncol = (ncol(bdata)+1)))
lapply(rec_names, firstLever, bdata)

## fit surv model ----
surv.mod.L <- coxph(Surv(time=time, event = censored, type = "right") ~ Reward*type, 
                    id = ID, data = sdata)
car::Anova(surv.mod.L, test.statistic = "Wald")

#comparisons
pair.surv.lea <- emmeans(surv.mod.L,  ~Reward*type)
v <- summary(pair.surv.lea, type = "response")


#p-values
p.pair.surv.lea <-summary(contrast(pair.surv.lea, method = "tukey"))


#median survival time
#create variable stimuli
sdata$group <- paste0(sdata$Reward,sdata$type)
sdata$group <- factor(sdata$group, levels = c( "RL","NRL", "RN", "NRN"))

levels(sdata$group) <- groupLab


stimType <- groupLab
Med.Lea <- as.data.frame(
  do.call(rbind, lapply(stimType, function(x) 
    summary(
      survfit(Surv(time, censored) ~ 1, data = sdata[sdata$group==x,]))$table[7:9]
  )
  )
)
Med.Lea$stimuli <- stimType


### PLOT 


surv.eff.L <- ggpredict(surv.mod.L, terms = c("Reward", "type"), type = "surv", ci.lvl = 1-alphaci)
surv.eff.L$predicted <- 1-surv.eff.L$predicted
surv.eff.L$conf.high <- 1-surv.eff.L$conf.high
surv.eff.L$conf.low <- 1-surv.eff.L$conf.low
surv.eff.L$stimuli <- paste0(surv.eff.L$group, surv.eff.L$facet)
surv.eff.L$stimuli <- factor(surv.eff.L$stimuli, levels = c( "RL","NRL", "RN", "NRN"))
levels(surv.eff.L$stimuli) <- stimType

surv.eff.L$x <- surv.eff.L$x / 1000


##plot separately

#familiar
xmin <- 0
xMax <- 1

wdat <- surv.eff.L[surv.eff.L$facet=="L",]
wdat <- droplevels(wdat)

surv.plot.L <- ggplot(wdat, aes(x=x, y=predicted, group = stimuli, color = stimuli)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high, group = stimuli, fill = stimuli), alpha = 0.25, linetype = 0) +
  scale_color_manual(values = coloresL) +
  scale_fill_manual(values = coloresL) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(xmin, xMax), expand=c(0,0)) +
  xlab("time from stimulus onset (s)") +
  ylab("1st lever press prob") +
  geom_text(x=1, y=0.85, label = "*", color = coloresL[1], size = 12)  +
  theme(axis.title.x = element_text(margin = margin(t=0, r=0, b=5, l=0, unit = "mm"),
                                    size = 9)
  )
saveRDS(surv.plot.L, "Survival_TG_familiar.rds")

#generalized
wdat <- surv.eff.L[surv.eff.L$facet=="N",]
wdat <- droplevels(wdat)

surv.plot.N <- ggplot(wdat[wdat$x <= 10,], aes(x=x, y=predicted, group = stimuli, color = stimuli)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high, group = stimuli, fill = stimuli), alpha = 0.15, linetype = 0) +
  scale_color_manual(values = coloresN) +
  scale_fill_manual(values = coloresN) +
  scale_x_continuous(expand=c(0,0), limits = c(0,15)) +
  scale_y_continuous(limits=c(xmin, xMax), expand=c(0,0)) +
  xlab("time from stimulus onset (s)") +
  ylab("1st lever press prob") +
  # geom_text(x=1, y=0.85, label = "*", color = coloresN[1], size = 12)  +
  theme(axis.title.x = element_text(margin = margin(t=0, r=0, b=5, l=0, unit = "mm"),
                                    size = 9)
  )

saveRDS(surv.plot.N, "Survival_TG_novel.rds")



