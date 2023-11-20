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

# borneo<-readOGR("./BorneoShapefile","borneo_moll")
# colombia<-readOGR("./Colombia","Amazon_Orinoco_moll")

myfiles = list.files("./Data/biomes/ecorregions_dissolve",
                     pattern="*_5_0_diss.shp") 

#myfiles = list.files("./Data/biomes/ecorregions_dissolve",pattern="*diss.shp") 

list_pol<-list()
for (i in 1:length(myfiles)){
  print(file_ext(myfiles[[i]]))
  if(file_ext(myfiles[[i]]) == "xml"|
     file_ext(myfiles[[i]]) == "lock"){
    list_pol[[i]]<-NULL
  }else{
    list_pol[[i]]<-myfiles[[i]]
    
  }
}

list_pol<-plyr::compact(list_pol)

###############
setwd("./Data/biomes/ecorregions_dissolve")

#forests
#186,64,28,95,107

allregions<-lapply(list_pol[1:length(list_pol)], st_read)

a<-lapply(allregions, st_area)

rem<-which(a < 1e8)

allregions[rem]<-NULL

# reg1<-readOGR(list_pol[[194]])
# reg2<-readOGR(list_pol[[110]])
# reg3<-readOGR(list_pol[[184]])
# reg4<-readOGR(list_pol[[47]])
# reg5<-readOGR(list_pol[[36]])
# 
# allregions<-list(reg1,reg2,reg3,reg4,reg5)

#here I am starting with layers that have been prepared and are in the same projection. 

setwd("C:/Data/50Reefs/Felipe/iucn/human_footprint_carbon")


#load carbon map
carbon <- raster("./Data/BGC_2010_1k_moll.tif")
carbon <- raster("./Data/Irrecoverable_C_Total_2010_1km_mol.tif")

# load human footprint layer
human.footprint<-raster("./HF_2013/hfp_meris/hfp2013_meris.tif")


# Load in human footprint covariates

built.r <- raster("./HF_2013/pressure_rasters/built_areas2013.tif")# Base map of Australia
croplands.r <- raster("./HF_2013/pressure_rasters/crops_meris2013.tif")# Mean annual precipitation/rainfall;
navwater.r <- raster("./HF_2013/pressure_rasters/navwaters2013.tif")# Mean annual temperature
pastures.r <- raster("./HF_2013/pressure_rasters/pastures_meris2013.tif")# MODIS tree cover
popdensity.r <- raster("./HF_2013/pressure_rasters/popden2013.tif")# Ruggedness (SD of elevation)
#railways <- raster(".//hf_variables/carbon_rep.tif")#
roads <- raster("./HF_2013/pressure_rasters/roads_hfp.tif")
biome<- raster("./Data/world_biomes_moll.tif")# biomes



myExpl<- list(carbon=carbon,built=built.r,croplands=croplands.r,navwater=navwater.r,
              pastures=pastures.r,popdensity=popdensity.r,
              roads=roads,biome=biome)
# 
# newextent<-extent(Reduce(extend,myExpl))
# 
# re = list()
# #set all rasters to same extent
# for (k in 1:length(myExpl)){
#   r<-myExpl[[k]]
#   r<-extend(r,newextent)
#   r[is.na(r[])] <- 0 
#   re[[k]]<-r
#   #print(k)
# }
# 
# myExpl<- stack(re[[1]],re[[2]],re[[3]],re[[4]],
#                re[[5]],re[[6]],
#                re[[7]],re[[8]])
# 
# # region<-allregions[[4]]

#save(myExpl,file = "myExpl.Rdata")

# load("myExpl.R")

# for(i in 1:length(list_pol)){
#   setwd("./Data/biomes/ecorregions_dissolve")
#   a<-st_read(list_pol[i])
#   ar<-st_area(a)
#   if(as.numeric(ar) > 1e8){
#     region_xy_df(a,myExpl)
#   }


system.time(regions_hf<- lapply(allregions,region_xy_df,myExpl))

regions_hf<-plyr::compact(regions_hf)

library(randomForest)
library(dplyr)

random_forests_region<-list()

for (i in 30:length(regions_hf)) {
  timestamp()
  region_random_forest<- rep_random_forests(regions_hf[[i]],3,1)
  reg<-get_data_random_forest(region_random_forest)
  test<-lapply(region_random_forest,function(x)x[[1]]$results)
  test<-do.call(rbind,test)
  test<-test %>% group_by(mtry) %>% summarise_all(mean)
  test<-subset(test,test$Rsquared == max(test$Rsquared))
  reg$mtry<-test$mtry
  reg$RMSE<-test$RMSE
  reg$Rsquared<-test$Rsquared
  reg$MAE<-test$MAE
  random_forests_region[[i]]<-reg
  print(i)
  # save(reg,file = paste("./Random_forests_analyses/",
  #                                        regions_hf[[i]]$region[1],"_",
  #                       regions_hf[[i]]$biome[1],
  #                       "_irrec", ".R",sep = ""))
  timestamp()
}

reg<-do.call(rbind,random_forests_region)

write.csv(reg,paste("./Random_forests_analyses/",
                    "rf_ecoregions_carb_",
                    5,
                    "_irrec2", ".csv",sep = ""))
###########################

library(dplyr)
library(plyr)

setwd("~/human_footprint_carbon")
myfiles = list.files("./Random_forests_analyses/HUMID_FORESTS",pattern = "_AGC_1k") 

list_pol<-list()
for (i in 1:length(myfiles)){
  print(file_ext(myfiles[[i]]))
  if(file_ext(myfiles[[i]]) == "xml"){
    list_pol[[i]]<-NULL
  }else{
    list_pol[[i]]<-myfiles[[i]]
    
  }
}


setwd("./Random_forests_analyses/HUMID_FORESTS")
results_reg<-list()
for (i in 1:length(list_pol)){
  load(list_pol[[i]])
  reg<-get_data_random_forest(region_random_forest)
  reg$carbloc<-"AGC"
  results_reg[[i]]<-reg
  rm(region_random_forest)
  gc()
}


results<-do.call(rbind,results_reg)

library(ggplot2)
#PLOT NICELY###########
ggplot(subset(results,variable!="y" & variable!="Built Infrastructure" & biome == 1 & results$carbloc == "AGC"), 
       aes(y = mean, x = region, fill = variable))+
  geom_bar(position="dodge",stat = "identity")+
  #geom_errorbar( aes(x=variable, ymin=mean-sd, ymax=mean+sd), colour="blue", alpha=0.9, size=1.3,position = position_dodge(width = 0.9), width = 0.25)+
  #scale_color_manual(values = c('#D7191C','#FDAE61','#ABDDA4','#2B83BA'))+
  #scale_fill_manual(values = c('#D7191C','#FDAE61','#ABDDA4','#2B83BA'))+
  scale_color_manual(values = c("#FFD37F","#FFAA00", "#73FFDF","#00734C", "#DF73FF", "#A80084"))+
  scale_fill_manual(values = c("#FFD37F","#FFAA00", "#73FFDF","#00734C", "#DF73FF", "#A80084"))+
  #scale_shape_continuous()+
  theme_classic()+
  theme(axis.line.x = element_line(color="black", size = 0.5),
        axis.line.y = element_line(color="black", size = 0.5),
        axis.text=element_text(size=11),
        axis.title=element_text(size=11))+ 
  #scale_y_continuous(breaks = seq(0, 300, 50), limits = )+
  #scale_x_continuous(breaks = seq(0, 120, 20))+
  #expand_limits(x=c(0,50), y=c(0, 300))+
  labs(x = "",
       y = "Mean Variable importance", size=14)+
  #ggtitle("Dipersal 2 / generation=300")+
  #theme(legend.position="bottom",legend.box = "horizontal")+
  coord_flip()


######################
biomes_wwf<-read.csv("./Data/biomes/ECORREGIONS/biomes_wwf.csv")
BGC_ecorregion<-read.csv("./Data/biomes/ECORREGIONS/BGC_per_ecorregion.csv")

bgc_ecorregion<-merge(BGC_ecorregion,biomes_wwf[c("BIOME","ECO_NAME")],by.x = "ECO_NAME",all.x = TRUE)
biome_forest_temp<-biome_4[!duplicated(biome_4),]

biome_forest_temp<-biome_forest_temp[order(biome_forest_temp$AREA),]
