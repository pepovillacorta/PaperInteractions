#Analysis and plot for generalization sessions, both familiar and novel stimuli


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
colGen <- c("purple", "darkgreen", "lightblue", "lightgray")

#get alpha for CIs
zalpha = 1.0 #1.96 for 95% ci, 1.0 for se
alphaci <- 2*pnorm(zalpha, lower.tail = FALSE)




### MEAN PROBABILITY ----
setwd("C:/Users/user/OneDrive/DavidComportamiento")

#load data
bdata <- readRDS("Behavioral_Data_extended.RDS")

#remove rat 43
bdata <- bdata[bdata$Rat != "r43",]


ScenesCycle <- c( 1, 6, 4, 5, 2, 5, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 5,
                  4, 6, 8, 8, 8, 8, 8, 8, 8, 8, 8, 8, 6, 3, 5, 2, 6,
                  1, 5, 2, 6, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 6, 4, 5,
                  10, 10, 10, 10, 10, 10, 10, 10, 10, 10, 6, 2, 5, 3,
                  6, 4, 6, 9, 9, 9, 9, 9, 9, 9, 9, 9, 9, 5, 2, 5, 8,
                  8, 8, 8, 8, 8, 8, 8, 8, 8, 6, 3, 6, 4, 5, 4, 5, 7, 
                  7, 7, 7, 7, 7, 7, 7, 7, 7, 5, 1, 5, 2, 6, 10, 10, 
                  10, 10, 10, 10, 10, 10, 10, 10, 6 )


# repetitions in dynamic stimuli are taken as the same stimulus so:
ScenesTime <- c( 15, 15, 15, 15, 3, 5, 10, 10, 10, 10 )

SceneNames <- c("Reward_Static", "NonRew_Static", 
                "Reward_Dynamic", "NonRew_Dynamic",
                "Blank3", "Blank5",
                "Generalize_Reward", "Generalize_NonRew",
                "Novelty_Control", "Familiar")
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
rec_names <- unique(bdata$ID)

#remove -1
bdata <- bdata[bdata$Scene != -1,]

#number of different recordings
Nid <- length(unique(bdata$ID))

#names of variables in data
var_names <- names(bdata)


#aggregate
adata <- aggregate(beginPress ~ ID+Rat+session+designGroup+stimuli+type, data = bdata, sum)
adata$offset <- 0
adata$offset <- apply(adata, 1, function(x) x["offset"] <- total.time[names(total.time) == x["stimuli"]] )
adata$rate <- adata$beginPress / adata$offset

### Learned stimuli
adata.Lea <- adata[adata$type == "L",]
adata.Lea$stimuli <- as.factor(adata.Lea$stimuli)

#check random effects
baseFinal <- glm(rate ~ 1, data = adata.Lea, family = binomial("logit"), weights = offset)
finalI <- glmer(rate ~ 1 + (1|Rat), data = adata.Lea, family = binomial("logit"), weights = offset)
finalIS <- glmer(rate ~ 1 + (1|Rat/session), data = adata.Lea, family = binomial("logit"), weights = offset)
finalISD <- glmer(rate ~ 1 + (1|designGroup/Rat/session), data = adata.Lea, family = binomial("logit"), weights = offset)

anova(finalI,baseFinal)
anova(finalIS, finalI)
anova(finalISD, finalIS)

#include Rat and session
fit.Lea <- glmer(rate ~ stimuli + (1|Rat/session), data = adata.Lea, family = binomial("logit"), weights = offset )

pred.Lea <- ggemmeans(fit.Lea, terms = c("stimuli"), type = "fe", ci_level = 1-alphaci)

#effects
effects.Lea <- emmeans(fit.Lea,  "stimuli",  max.degree = 1, type = "response")
peffects.Lea <- summary(effects.Lea, infer = c(TRUE), null = 0)

#comparisons
#p-values
pcomp.Lea <-summary(contrast(effects.Lea, method = "tukey"))


### PLOT FAMILIAR
#reorder levels for coloring
pred.Lea$x <- factor(pred.Lea$x, levels = c("Reward_Dynamic", "Reward_Static", "NonRew_Dynamic", "NonRew_Static"))

#relevel for plot
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
  geom_text(x=1.25, y=0.46, label = "*", color = color[1], size = 12) +
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

#save plot
saveRDS(plot.Lea, "MeanRate_Generalization_familiar.rds")
write.csv2(pred.Lea,file = "fig3_panel_A.csv")



### Generalized stimuli
adata.Gen <- adata[adata$type == "N",]
adata.Gen$stimuli <- as.factor(adata.Gen$stimuli)

#check random effects
baseFinal <- glm(rate ~ 1, data = adata.Gen, family = binomial("logit"), weights = offset)
finalI <- glmer(rate ~ 1 + (1|Rat), data = adata.Gen, family = binomial("logit"), weights = offset)
finalIS <- glmer(rate ~ 1 + (1|Rat/session), data = adata.Gen, family = binomial("logit"), weights = offset)
finalISD <- glmer(rate ~ 1 + (1|designGroup/Rat/session), data = adata.Gen, family = binomial("logit"), weights = offset)

anova(finalI,baseFinal)
anova(finalIS, finalI)
anova(finalISD, finalIS)

#include Rat and session
fit.Gen <- glmer(rate ~ stimuli + (1|Rat/session), data = adata.Gen, family = binomial("logit"), weights = offset )

pred.Gen <- ggemmeans(fit.Gen, terms = c("stimuli"), type = "fe", ci.lvl = 1-alphaci)

#effects
effects.Gen <- emmeans(fit.Gen,  "stimuli",  max.degree = 1, type = "response")
peffects.Gen <- summary(effects.Gen, infer = c(TRUE), null = 0)

#comparisons
#p-values
pcomp.Gen <-summary(contrast(effects.Gen, method = "tukey"))


### PLOT
#reorder levels for coloring
pred.Gen$x <- factor(pred.Gen$x,
                     levels = c("Generalize_Reward", "Generalize_NonRew", "Familiar", "Novelty_Control"))

levels(pred.Gen$x) <- c("Compl REW", "Compl non-REW", "Control 1", "Control 2")


#plot

plot.Gen <- ggplot(pred.Gen) +
  geom_point(aes(x=x, y=predicted, group = x, color = x), alpha = 1, size = 3) +
  geom_errorbar(aes(x=x, ymax = conf.high, ymin=conf.low, group = x, color = x, width = 0.3), linewidth = 0.75) +
  scale_color_manual(values = colGen, guide = guide_legend(label.position = "top", byrow = FALSE, override.aes = list(linetype = 1, shape = NA, linewidth = 5))) +
  ylab("lever pressing prob") +
  scale_x_discrete(expand = c(0.1,0.1)) +
  scale_y_continuous(limits = c(xmin,xMax), expand = c(0,0)) +
  labs(x="stimulus type") +
  geom_text(x=1.25, y=0.49, label = "*", color = colGen[1], size = 12) +
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

#save plot
saveRDS(plot.Gen, "MeanRate_Generalization_novel.rds")
write.csv2(pred.Gen, file = "fig3_panel_D.csv")

### FIRST LEVER PRESS ----

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
    sdata <<- rbind(sdata, wfirst)
    
    
  } else {
    print("something is wrong")
  }
  
  return()
}

sdata <- as.data.frame(matrix(NA, nrow = 0, ncol = (ncol(bdata)+1)))
lapply(rec_names, firstLever, bdata)


### Learned data
#model
data.surv.Lea <- sdata[sdata$type == "L",]
data.surv.Lea$stimuli <- as.factor(data.surv.Lea$stimuli)
surv.mod.L <- coxph(Surv(time=time, event = censored, type = "right") ~ stimuli, 
                    id = ID, data = data.surv.Lea)
car::Anova(surv.mod.L, test.statistic = "Wald")

#comparisons
pair.surv.lea <- emmeans(surv.mod.L,  ~stimuli)
v <- summary(pair.surv.lea, type = "response")


#p-values
p.pair.surv.lea <-summary(contrast(pair.surv.lea, method = "tukey"))


### PLOT 
surv.eff.L <- ggpredict(surv.mod.L, terms = c("stimuli"), type = "surv", 
                        ci_level =  1-alphaci)
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
  geom_ribbon(aes(x=x, ymin=conf.low, ymax=conf.high, group = group, fill = group), alpha = 0.25, linetype = 0) +
  scale_color_manual(values = color) +
  scale_fill_manual(values = color) +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_continuous(limits=c(xmin, xMax), expand=c(0,0)) +
  xlab("time from stimulus onset (s)") +
  ylab("1st lever press prob") +
  geom_text(x=1, y=0.95, label = "*", color = color[1], size = 12)  +
  theme(axis.title.x = element_text(margin = margin(t=0, r=0, b=5, l=0, unit = "mm"),
                                    size = 9)
  )

#save
saveRDS(surv.plot.L, "Survival_Generalization_familiar.rds")
write.csv2(surv.eff.L, file = "fig3_panel_C.csv")


### Generalized data
#model
data.surv.Gea <- sdata[sdata$type == "N",]
data.surv.Gea$stimuli <- as.factor(data.surv.Gea$stimuli)
surv.mod.N <- coxph(Surv(time=time, event = censored, type = "right") ~ stimuli, 
                    id = ID, data = data.surv.Gea)
car::Anova(surv.mod.N, test.statistic = "Wald")

#comparisons
pair.surv.gea <- emmeans(surv.mod.N,  ~stimuli)
sum.pair.surv.gea <- summary(pair.surv.gea, type = "response")


#p-values
p.pair.surv.gea <-summary(contrast(pair.surv.gea, method = "tukey"))


#plot
surv.eff.N <- ggpredict(surv.mod.N, terms = c("stimuli"), type = "surv",
                        ci_level =  1-alphaci)
surv.eff.N$predicted <- 1-surv.eff.N$predicted
surv.eff.N$conf.high <- 1-surv.eff.N$conf.high
surv.eff.N$conf.low <- 1-surv.eff.N$conf.low
surv.eff.N$group <- factor(surv.eff.N$group, 
                           levels = c("Generalize_Reward", "Generalize_NonRew", "Familiar", "Novelty_Control"))
levels(surv.eff.N$group) <- c("Compl REW", "Compl non-REW", "Control 1", "Control 2")
surv.eff.N$x <- surv.eff.N$x / 1000

surv.plot.N <- ggplot(surv.eff.N[surv.eff.N$x <= 10,], aes(x=x, y=predicted, group = group, color = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(x=x, ymax=conf.high, ymin=conf.low, group = group, fill=group), alpha = 0.15) +
  scale_color_manual(values = colGen) +
  scale_x_continuous(expand=c(0,0), limits = c(0,15)) +
  scale_y_continuous(limits=c(xmin, xMax), expand=c(0,0)) +
  xlab("time from stimulus onset (s)") +
  ylab("1st lever press prob") +
  # geom_text(x=1, y=0.95, label = "*", color = color[1], size = 12)  +
  theme(axis.title.x = element_text(margin = margin(t=0, r=0, b=5, l=0, unit = "mm"),
                                    size = 9)
  )

#save
saveRDS(surv.plot.N, "Survival_Generalization_novel.rds")
write.csv2(surv.eff.N[surv.eff.N$x <= 10,], file = "fig3_panel_F.csv")


### RATE distribution ----
#group times each N
tgdata <- aggregate(beginPress ~ ID + Rat + session + stimuli + time + type + stID.within, data = bdata, sum)
tgdata$each <- 0
bin.g <- 5



for(j in 1:length(rec_names)){
  for(i in 1:length(SceneNames)){
    for(h in 1:max(tgdata$stID.within[tgdata$stimuli == SceneNames[i] & tgdata$ID == rec_names[j]]))
      tgdata$each[tgdata$stimuli == SceneNames[i] & tgdata$ID == rec_names[j] & tgdata$stID.within == h] <- rep(
        seq(1,length(tgdata$each[tgdata$stimuli == SceneNames[i] & tgdata$ID == rec_names[j] & tgdata$stID.within == h] ) / bin.g ), each = bin.g)
  }
}

#remove blanks
tgdata <- tgdata[tgdata$type != "B",]

#check offset

t.group <- aggregate(beginPress ~ ID + Rat + session + stimuli + type + each, data = tgdata, sum)

t.offset <- aggregate(beginPress ~ ID + Rat + session + stimuli + type + each, data = tgdata, length)

t.group$time <- t.group$each * 40 * bin.g
t.group$offset <- t.offset$beginPress * 40 * bin.g  #each 1 ms
t.group$rate <- t.group$beginPress / t.group$offset


#genelarized
t.gea <- t.group[t.group$type == "N",]
t.gea$time <- as.numeric(t.gea$time)

baseT <- glm(rate ~ 1, family = binomial("logit"), data = t.gea, weights = offset)
fitI <- glmer(rate ~ 1 + (1|Rat), family = binomial("logit"), data = t.gea, weights = offset)
fitIS <- glmer(rate ~ 1 + (1|Rat/session), family = binomial("logit"), data = t.gea, weights = offset)
fitIST <- glmer(rate ~ 1 + (1+time|Rat/session), family = binomial("logit"), data = t.gea, weights = offset)

anova(fitI, baseT)
anova(fitIS, fitI)
anova(fitIST, fitIS)
anova(fitISTT, fitIST)


t.gea$stimuli <- as.factor(t.gea$stimuli)
fitTime.G <- glmer(rate ~ stimuli * poly(time, degree = 2) + (1|Rat/session), 
                   family = binomial("logit"), data = t.gea, weights = offset)


#comparisons
#pairwise comparisons: Tukey test
by.time.gea <- emtrends(fitTime.G,  "stimuli", "time", max.degree = 2)
sum.by.time.gea <- summary(by.time.gea, type = "response")

#check effects of each estimuli
by.time.familiar <- glmer(rate ~ poly(time, degree = 2) + (1|Rat/session), 
                          family = binomial("logit"), data = t.gea[t.gea$stimuli == "Familiar",], weights = offset)
by.time.novelty <- glmer(rate ~ poly(time, degree = 2) + (1|Rat/session), 
                         family = binomial("logit"), data = t.gea[t.gea$stimuli == "Novelty_Control",], weights = offset)

by.time.rewarded <- glmer(rate ~ poly(time, degree = 2) + (1|Rat/session), 
                          family = binomial("logit"), data = t.gea[t.gea$stimuli == "Generalize_Reward",], weights = offset)
by.time.nonrewarded <- glmer(rate ~ poly(time, degree = 2) + (1|Rat/session), 
                             family = binomial("logit"), data = t.gea[t.gea$stimuli == "Generalize_NonRew",], weights = offset)

#por ms

#p-values
p.by.time.gea <-summary(contrast(by.time.gea, method = "tukey"))



time.effects.gea <- ggpredict(fitTime.G, terms = c("time[all]", "stimuli"), 
                              type = "fe")

data.grouped <- aggregate(rate ~ time + stimuli + type, data = t.group, mean)
names(data.grouped)[2] <- "group"
time.effects.gea$x <- time.effects.gea$x / 1000
data.grouped$time <- data.grouped$time / 1000

#labels
stimLab <- c("Compl REW", "Compl non-REW", "Control 1", "Control 2")
groupLab <- c("Compl REW", "Compl non-REW", "Control 1", "Control 2")

### Plot
time.effects.gea$group <- factor(time.effects.gea$group, 
                                 levels = c("Generalize_Reward", "Generalize_NonRew", "Familiar", "Novelty_Control"))
levels(time.effects.gea$group) <- c("Compl REW", "Compl non-REW", "Control 1", "Control 2")

data.grouped$group <- factor(data.grouped$group, 
                             levels = c("Generalize_Reward", "Generalize_NonRew", "Familiar", "Novelty_Control"))
levels(data.grouped$group) <- c("Compl REW", "Compl non-REW", "Control 1", "Control 2")
#data.frame for asterisc
dat.as <- data.frame(label = "*", x=1, y=0.00015, group="Compl REW")

xmin <- 0
xMax <- 0.00019
#find maximun exponent
maxExp <- floor(log10(max(xMax)))
dat.exp <- data.frame(group=c("Compl REW", "Control 1"), lab = paste0("x10^{",maxExp,"}"), y = xMax)



plot.time.gea <- ggplot() +
  geom_line(data = time.effects.gea[time.effects.gea$x<=10,], aes(x=x, y=predicted, colour = group), linewidth = 1) +
  geom_col(data = data.grouped[data.grouped$type == "N",], aes(x=time, y = rate, group = group, fill = group), alpha = 0.5) +
  geom_text(data = dat.as, aes(x=x, y=y, label=label, color = group), size = 12) +
  ylab("lever pressing prob") +
  xlab("time from stimulus onset (s)") +
  # annotate("text", label = paste0("x10^{",maxExp,"}"), x=-Inf, y=xMax, size = 5, hjust = 1.5, parse = TRUE) +
  geom_text(data=dat.exp, aes(x=0, y=y, group = group, label = lab), parse = "TRUE", size = 5, hjust = 0.5, vjust = -0.5) +
  scale_x_continuous(limits = c(0, 15), n.breaks = 3, expand = c(0,0)) +
  scale_y_continuous(limits = c(xmin, xMax), n.breaks = 6, labels = function(x) x/10^maxExp, expand = c(0,0)) +
  coord_cartesian(xlim = c(0, 15), ylim = c(xmin, xMax), clip = "off") +
  scale_fill_manual(values = colGen, guide = "none") +
  scale_color_manual(values = colGen, labels = stimLab) +
  facet_wrap(~factor(group, levels = c("Compl REW", "Compl non-REW", "Control 1", "Control 2")), nrow = 2) +
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

#save
saveRDS(plot.time.gea, "RateDistribution_Generalization_novel.rds")
write.csv2(time.effects.gea[time.effects.gea$x<=10,], file = "fig3_panel_E_fit.csv")
write.csv2(data.grouped[data.grouped$type == "N",], file = "fig3_panel_E_histogram.csv")

#learned
t.lea <- t.group[t.group$type == "L",]
t.lea$time <- as.numeric(t.lea$time)
t.lea$stimuli <- as.factor(t.lea$stimuli)

fitTime.L <- glmer(rate ~ stimuli * poly(time, degree = 2) + (1|Rat/session), 
                   family = binomial("logit"), data = t.lea, weights = offset)


#comparisons
#pairwise comparisons: Tukey test
by.time.lea <- emtrends(fitTime.L,  "stimuli", "time", max.degree = 2)
sum.by.time.lea <- summary(by.time.lea, type = "response")


#p-values
p.by.time.lea <-summary(contrast(by.time.lea, method = "tukey"))



time.effects.lea <- ggpredict(fitTime.L, terms = c("time[all]", "stimuli"), type = "fe")

data.grouped <- aggregate(rate ~ time + stimuli + type, data = t.group, mean)
data.grouped.L <- data.grouped[data.grouped$type == "L",]
names(data.grouped.L)[2] <- "group"
time.effects.lea$x <- time.effects.lea$x / 1000
data.grouped.L$time <- data.grouped.L$time / 1000

#labels
stimLab.L <- c('Dynamic REW', 'Static REW', 'Dynamic non-REW', 'Static non-REW')
groupLab.L <- c('Dynamic REW', 'Static REW', 'Dynamic non-REW', 'Static non-REW')

### Plot
time.effects.lea$group <- factor(time.effects.lea$group, 
                                 levels = c("Reward_Dynamic", "Reward_Static", "NonRew_Dynamic", "NonRew_Static"))
levels(time.effects.lea$group) <- c("Dynamic REW", "Static REW", "Dynamic non-REW", "Static non-REW")


data.grouped.L$group <- factor(data.grouped.L$group, 
                               levels = c("Reward_Dynamic", "Reward_Static", "NonRew_Dynamic", "NonRew_Static"))
levels(data.grouped.L$group) <- c("Dynamic REW", "Static REW", "Dynamic non-REW", "Static non-REW")

#data.frame for asterisc
dat.as.L <- data.frame(label = "*", x=1, y=0.00015, group="Dynamic REW")

dat.exp <- data.frame(group=c("Dynamic REW", "Dynamic non-REW"), lab = paste0("x10^{",maxExp,"}"), y = xMax ) 

plot.time.lea <-ggplot() +
  geom_line(data = time.effects.lea, aes(x=x, y=predicted, colour = group), linewidth = 1) +
  geom_col(data = data.grouped.L, aes(x=time, y = rate, group = group, fill = group), alpha = 0.5) +
  geom_text(data = dat.as.L, aes(x=x, y=y, label=label), size = 12, color = color[1]) +
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

#save
saveRDS(plot.time.lea, "RateDistribution_Generalization_familiar.rds")
write.csv2(time.effects.lea, file = "fig3_panel_B_fit.csv")
write.csv2(data.grouped.L, file = "fig3_panel_B_histogram.csv")
