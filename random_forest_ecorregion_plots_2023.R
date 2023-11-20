library(raster)
library(caret)
library(plyr)
library(randomForest)
library(rgdal)
library(fasterize)
library(sf)
library(tools)

setwd("C:/Data/50Reefs/Felipe/iucn/human_footprint_carbon")

rm(list=ls())
gc()
source('functions_random_forests_carbon.R')

###########################

library(dplyr)
library(plyr)


myfiles2 = list.files("./Random_forests_analyses/AGC_BGC_RF",pattern = ".csv",
                     full.names = T) 

myfiles = list.files("./Random_forests_analyses/irrep_carb_RF",pattern = ".csv",
                     full.names = T) 

results<-do.call(rbind,lapply(myfiles, read.csv))
results2<-do.call(rbind,lapply(myfiles2, read.csv))
length(unique(results2$region))

length(unique(results$region))
length(unique(results2$region))

extra<-subset(results2,!(results2$region %in% results$region))
head(extra)
unique(extra$region)

results$carbloc<-"irrep"

head(results)

results<-rbind(results,extra)

#######################

results$biome_code<-mapvalues(results$biome,from=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), 
                              to= c("TropMoBF", "TropDBF", "CF ","TempBMixF",
                                    "TempCF","Taig","TropGSS",
                                    "TempGSS",
                                   "FloodSS","MGS","Tund",
                                    "MFS","DXS","M"))


results_st<-results %>% group_by(variable) %>% 
  dplyr::summarise(mean_run = mean(mean))

results_st2<-results %>% group_by(biome_code) %>% 
  dplyr::summarise(mean_run = mean(Rsquared))

View(results_st2)

setwd("C:/Data/50Reefs/Felipe/iucn/human_footprint_carbon")

hola<-read.csv("./Data/biomes/biomes_country.csv")

#MERGE RESULTS####

results<-merge(results,hola[c("ECO_NAME","SUBREGION",'CONTINENT',"REGION_UN")],
               by.x = "region",
               by.y = "ECO_NAME",all.x = T)

results<-subset(results,results$SUBREGION != " ")

#SUMMARIZE RESULTS####

results1<-results %>% group_by(variable,SUBREGION) %>% 
  dplyr::summarise(mean_run = median(mean))

results2<-results %>% group_by(variable,region,CONTINENT,biome) %>% 
  dplyr::summarise(mean = mean(mean), sd = mean(sd))

#GET BIOME TO PLOT####

results2<-subset(results,results$biome == 13)
#GET RID OF DUPLICATES
results2<-results2[!duplicated(results2[1:12]),]
results2$name<-paste(round(results2$Rsquared,1),results2$region)


#CALCULATE STATS####
restat<-subset(results2,results2$Rsquared > 0.1)
restat<-restat %>% group_by(variable) %>% 
  dplyr::summarise(mean = median(mean), sd = mean(sd))
sd(results2$Rsquared)
restat

#SPLIT BY CONTINENT
results3<-split(results2,results2$CONTINENT)

#FUNCTION TO PLOT

toplotRF<-function(df){
  plotrf<-ggplot(subset(df,variable!="y" & variable!="Built Infrastructure" 
                        #& SUBREGION != " "
                        #& biome == 1 
                        #& results$carbloc == "BGC"
  ), 
  aes(y = mean, x = name, fill = variable))+
    geom_bar(position="dodge",stat = "identity")+
    geom_errorbar( aes(x=name, ymin=mean-sd, ymax=mean+sd), 
                   colour="black", alpha=0.9, #size=1,
                   position = position_dodge(width = 0.9), width = 0.1)+
    #scale_color_manual(values = c('#D7191C','#FDAE61','#ABDDA4','#2B83BA'))+
    #scale_fill_manual(values = c('#D7191C','#FDAE61','#ABDDA4','#2B83BA'))+
    scale_color_manual(values = c("#FFD37F","#FFAA00", "#73FFDF","#00734C", "#DF73FF", "#A80084"))+
    scale_fill_manual(values = c("#FFD37F","#FFAA00", "#73FFDF","#00734C", "#DF73FF", "#A80084"))+
    #scale_shape_continuous()+
    theme_classic()+
    ggtitle(paste0(#"",
      #" ",
      re$CONTINENT))+
    theme(axis.line.x = element_line(color="black", size = 0.5),
          axis.line.y = element_line(color="black", size = 0.5),
          axis.text=element_text(size=16),
          axis.title=element_text(size=16),
          legend.position = "none",
          plot.title = element_text(size=22,
                                    hjust = 0.5),
          plot.title.position = "plot")+ 
    scale_y_continuous(breaks = seq(0, 110, 10), limits =c(0,110))+
    #scale_x_continuous(breaks = seq(0, 120, 20))+
    #expand_limits(x=c(0,50), y=c(0, 300))+
    labs(x = "",
         y = "Mean Variable importance")+
    theme(legend.position="bottom",legend.box = "horizontal")+
    coord_flip()#+
  #facet_grid(~ SUBREGION)
  return(plotrf)
}


library(ggplot2)
p<-list()

for (i in 1:length(results3)) {
  re<-results3[[i]]
  re<-subset(re,re$Rsquared>0.3)
  reg<-sort(unique(re$name),decreasing = T)
  if(length(reg)>10){
    breaks<-round(length(reg)/3)
    if(breaks > 10){
      br<-seq(1,length(reg),10)
    }else{
      br<-seq(1,length(reg),breaks)
    }
    p2<-list()
    for (k in 1:length(br)) {
      df<-subset(re, re$name %in% reg[br[[k]]:(br[[k]] + 9)]) 
      p2[[k]]<-toplotRF(df)
    }
  }else{
    p2<-toplotRF(re)
    }
  p[[i]]<-p2
}

p

##############
library(egg)
library(gridExtra)

figure <- ggarrange(p2[[1]],p2[[2]],p2[[3]],
                    #labels = c("A", "B", "C"),
                    ncol = 1, nrow = 3)
figure
p2[[1]]

