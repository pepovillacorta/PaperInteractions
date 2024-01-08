

### setup -----
#clear workspace
rm(list = ls())
#clear ram
gc()
#clear console
cat("\014")

library(ggplot2)
library(ggeffects)
library(gridExtra)
library(grid)
library(scales)
library(dplyr)

##function to get legends
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

maxExp <- -4

### General Options ----
optPlotGen <- theme(axis.title.x = element_text(margin = margin(t=2, r=0, b=0, l=0, unit = "mm"), size = 16),
                    axis.title.y = element_text(margin = margin(t=0, r=2, b=0, l=0, unit = "mm"), size = 16, vjust = 1),
                    plot.margin = margin(t=10, r=10, b=10, l=10, unit = "mm"),
                    panel.background = element_rect(fill = "white"),
                    axis.line = element_line(colour = "black", size = 1),
                    axis.text.x = element_text(size = 14),
                    axis.text.y = element_text(size = 14))

addLabels <- theme(plot.tag = element_text(size = 20, face = "bold"),
                   plot.tag.position = c(0,1))



##select figure: 1: TG configuration, 2: generalization of DG configuration, 3: trainning DG conf.
fig = 1

switch(fig,
       fig1={
         ###TG FIGURE -----
         ##individual plots from "GeneralizeSession_TGprotocol_figure.r"
         
         tags <- c("A", "B", "C", "D", "E", "F")
         #LOAD PANELS
         #mean reat
         MR_fam <- readRDS("MeanRate_TG_familiar.rds")
         MR_nov <- readRDS("MeanRate_TG_novel.rds")
         
         
         
         #rate distribution
         RD_fam <- readRDS("RateDistribution_TG_familiar.rds")
         RD_nov <- readRDS("RateDistribution_TG_novel.rds")
         
         #reaction time
         RT_fam <- readRDS("Survival_TG_familiar.rds")
         RT_nov <- readRDS("Survival_TG_novel.rds")
         
         
         plotMR_fam <- MR_fam + theme(legend.position = "bottom") +
           optPlotGen +
           labs(tag = tags[1]) +
           addLabels
         
         plotMR_nov <- MR_nov + theme(legend.position = "bottom") +
           optPlotGen+
           labs(tag = tags[4]) +
           addLabels
         
         plotRD_fam <- RD_fam + theme(legend.position = "none")+
           optPlotGen +
           labs(tag = tags[2]) +
           addLabels
         
         plotRD_nov <- RD_nov + theme(legend.position = "none")+
           optPlotGen+
           labs(tag = tags[5]) +
           addLabels
         
         plotRT_fam <- RT_fam + theme(legend.position = "none")+
           optPlotGen+
           labs(tag = tags[3]) +
           addLabels
         
         plotRT_nov <- RT_nov + theme(legend.position = "none")+
           optPlotGen+
           labs(tag = tags[6]) +
           addLabels
         
         
         #legends
         mylegendL<- g_legend(plotMR_fam)
         mylegendN <- g_legend(plotMR_nov)
         
         #remove legend
         plotMR_fam <- plotMR_fam + theme(legend.position = "none")
         plotMR_nov <- plotMR_nov + theme(legend.position = "none")
         
         
         
         plotL <- grid.arrange(arrangeGrob(plotMR_fam, plotRD_fam, plotRT_fam, 
                                           nrow = 1, ncol = 3, widths = c(1.5,2,1.5)),
                               mylegendL, nrow=2,heights= c(10, 3))
         
         plotN <- grid.arrange(arrangeGrob(plotMR_nov, plotRD_nov, plotRT_nov, 
                                           nrow = 1, ncol = 3, widths = c(1.5,2,1.5)),
                               mylegendN, nrow=2,heights=c(10, 3))
         
         plotGener <- grid.arrange(plotL, plotN, nrow=2)
         
         ggsave("Figure TG protocol_v2.tiff", plot = plotGener, units="mm", width=400, height=400, dpi=600, compression = 'lzw')
         
       }, #end of fig1
       
       fig2 ={
         ###GENERALIZATION FIGURE -----
         ##individual plots from "Figure 1.r"
         
         tags <- c("A", "B", "C", "D", "E", "F")
         #LOAD PANELS
         #mean reat
         MR_fam <- readRDS("MeanRate_Generalization_familiar.rds")
         MR_nov <- readRDS("MeanRate_Generalization_novel.rds")
         
         #rate distribution
         RD_fam <- readRDS("RateDistribution_Generalization_familiar.rds")
         RD_nov <- readRDS("RateDistribution_Generalization_novel.rds")
         
         #reaction time
         RT_fam <- readRDS("Survival_Generalization_familiar.rds")
         RT_nov <- readRDS("Survival_Generalization_novel.rds")
         
         
         plotMR_fam <- MR_fam + theme(legend.position = "bottom") +
           optPlotGen +
           labs(tag = tags[1]) +
           addLabels
         
         plotMR_nov <- MR_nov + theme(legend.position = "bottom") +
           optPlotGen+
           labs(tag = tags[4]) +
           addLabels
         
         plotRD_fam <- RD_fam + theme(legend.position = "none")+
           optPlotGen +
           labs(tag = tags[2]) +
           addLabels
         
         plotRD_nov <- RD_nov + theme(legend.position = "none")+
           optPlotGen+
           labs(tag = tags[5]) +
           addLabels
         
         plotRT_fam <- RT_fam + theme(legend.position = "none")+
           optPlotGen+
           labs(tag = tags[3]) +
           addLabels
         
         plotRT_nov <- RT_nov + theme(legend.position = "none")+
           optPlotGen+
           labs(tag = tags[6]) +
           addLabels
         
         
         #legends
         mylegendL<- g_legend(plotMR_fam)
         mylegendN <- g_legend(plotMR_nov)
         
         #remove legend
         plotMR_fam <- plotMR_fam + theme(legend.position = "none")
         plotMR_nov <- plotMR_nov + theme(legend.position = "none")
         
         plotL <- grid.arrange(arrangeGrob(plotMR_fam, plotRD_fam, plotRT_fam, 
                                           nrow = 1, ncol = 3, widths = c(1.5,2,1.5)),
                               mylegendL, nrow=2,heights =c(10, 3))
         
         plotN <- grid.arrange(arrangeGrob(plotMR_nov, plotRD_nov, plotRT_nov, 
                                           nrow = 1, ncol = 3, widths = c(1.5,2,1.5)),
                               mylegendN, nrow=2,heights=c(10, 3))
         
         plotGener <- grid.arrange(plotL, plotN, nrow=2)
         
         ggsave("Figure Generalization Session.tiff", plot = plotGener, units="mm", width=400, height=400, dpi=600, compression = 'lzw')
         
       },  #end of fig2
       
       
       fig3 ={
         ###TRAINNING FIGURE -----
         
         
         ##plots for first configuration from "Fig_Train_performance.r" (panels A and B)
         ##plots for last trainning session from "Figure1_last session.r" (panels C-E)
         
         
         addLabels <- theme(plot.tag = element_text(size = 20, face = "bold"),
                            plot.tag.position = c(-0.02,1))
         
         tags <- c("A", "B", "C", "D", "E")
         
         #LOAD PANELS
         #mean rate over sessions of the first configuration
         MR_ses <- readRDS("MeanRate_FirstConfiguration_familiar.rds")
         
         #Hazard ratio over sessions of the first configuration
         HR_ses <- readRDS("Survival_FirstConfiguration_familiar.rds")
         
         #mean reat
         MR_fam <- readRDS("MeanRate_Trainning_familiar.rds")
         
         
         #rate distribution
         RD_fam <- readRDS("RateDistribution_Trainning_familiar.rds")
         
         
         #reaction time
         RT_fam <- readRDS("Survival_Trainning_familiar.rds")
         
         
         plotMR_ses <- MR_ses + theme(legend.position = "none")+
           optPlotGen +
           labs(tag = tags[1]) +
           addLabels
         
         plotHR_ses <- HR_ses + theme(legend.position = "none")+
           optPlotGen +
           labs(tag = tags[2]) +
           addLabels
         
         plotMR_fam <- MR_fam + theme(legend.position = "bottom") +
           optPlotGen +
           labs(tag = tags[3]) +
           addLabels
         
         plotRD_fam <- RD_fam + theme(legend.position = "none")+
           optPlotGen +
           labs(tag = tags[4]) +
           addLabels
         
         plotRT_fam <- RT_fam + theme(legend.position = "none")+
           optPlotGen+
           labs(tag = tags[5]) +
           addLabels
         
         
         
         
         #legends
         mylegendL<- g_legend(plotMR_fam)
         
         
         #remove legend
         plotMR_fam <- plotMR_fam + theme(legend.position = "none")
         
         
         plotL <- grid.arrange(arrangeGrob(plotMR_ses, plotHR_ses, 
                                           nrow = 1, ncol = 2))
         
         
         plotN <- grid.arrange(arrangeGrob(plotMR_fam, plotRD_fam, plotRT_fam, 
                                           nrow = 1, ncol = 3, widths = c(1.5,2,1.5)),
                               mylegendL, nrow=2,heights=c(10, 3))
         
         plotGener <- grid.arrange(plotL, plotN, nrow=2, heights = c(1.5,2))
         
         ggsave("Figure Trainning.tiff", plot = plotGener, units="mm", width=400, height=200, dpi=600, compression = 'lzw')
         
       }  #end of fig3
       
) #end of switch
