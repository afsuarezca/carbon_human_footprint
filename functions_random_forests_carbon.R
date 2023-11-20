
region_xy_df<- function(region,myExpl){
                # region_expl<-list()
                # reg <- crop(myExpl, extent(region))
                # reg<-stack(reg)
                # reg1<- fasterize(st_as_sf(region),reg[[1]])
                # region_mask<-mask(reg,reg1)
                # hola<-rasterToPoints(region_mask)
                # region_hf<-as.data.frame(hola)
                region_expl <-list()
                system.time(for(i in 1:length(myExpl)){
                  reg <- crop(myExpl[[i]], extent(region)) #crops the raster to the extent of the polygon, I do this first because it speeds the mask up
                  #
                  #system.time(region_expl[[i]] <- mask(region_expl[[i]],region)) #crops the raster to the polygon boundary
                  #region_expl[[i]] <- rasterize(region,region_expl[[i]], mask=TRUE)
                  reg1<- fasterize(st_as_sf(region),reg)
                  reg2<-mask(reg,reg1)
                  #plot(reg2)
                  region_expl[[i]] <- reg2
                })
                region_name<-as.character(region$ECO_NAME)
                print(region_name)
                # newextent<-extent(Reduce(extend,region_expl))
                # for (k in 1:length(region_expl)){
                #   region_expl[[k]]<-extend(region_expl[[k]],newextent)
                # #print(k)
                #   }

                region2<-stack(region_expl)
                hola<-rasterToPoints(region2)
                region_hf<-as.data.frame(hola)
                region_hf<-region_hf[complete.cases(region_hf), ]
                print(paste(nrow(region_hf),region_name))
                if(nrow(region_hf)>100){
                  region_hf$region<-region_name
                  colnames(region_hf)<-c("x","y","carbon","built",
                                         "croplands",
                                         "navwater","pastures","popdensity","roads","biome","region")
                  
                }else{
                  region_hf<-NULL
                }
                return(region_hf)
              }



rf_region<-function(sample_df,biome,region){
  
  #sample_df<-subset(df,biome==biome)
  #sample_df<-sample_df[sample(nrow(sample_df),5000),]
  
  #for(i in c(4,5,10))
  #{
  # sample_df[[i]]<-as.factor(sample_df[[i]])
  #}
  
  # Define training control: 10 fold cross-validation. 
  train_control <- trainControl(method="cv", number=10)
  
  # Split into Train and Validation sets
  # Training Set : Validation Set = 70 : 30 (random)
  train <- sample(nrow(sample_df), 0.7*nrow(sample_df), replace = FALSE)
  #print(length(train))
  TrainSet <- sample_df[train,]
  #print(nrow(TrainSet))
  ValidSet <- sample_df[-train,]
  
  df<-sample_df[ , -which(names(sample_df) %in% c("x","y","carbon","biome","region"))]
  
  vec <- as.vector(colnames(df), mode="any")
  
  formula_model<-as.formula(paste("carbon~y+",paste(colnames(df),collapse = "+"),
                                  sep = ""))
  
  
  model<- train(formula_model, data=TrainSet, trControl=train_control, method="rf",
                importance=T)
  
  # Make predictions and produce Mean Absolute Error (MAE) scores to evaluate
  # model performance
  predictions <- predict(model, ValidSet)
  result <- data.frame(Actual=ValidSet$carbon,Predicted=predictions)
  result$Difference <- abs(result$Actual - result$Predicted)
  # Plot predicted vs observed biomass to inspect model performance
  ValidSet$predictions <- predictions
  #RMSE <- sqrt(sum((predictions - ValidSet$rastervalues)^2)/length(predictions))
  #print(RMSE/mean(ValidSet$rastervalues)) 
  varimport<-varImp(model)
  varimport<- varimport$importance
  varimport$variable<-rownames(varimport)
  varimport$biome<-biome
  varimport$region<-region
  return(list(model,ValidSet,varimport))
}

rep_random_forests<-function(region_df,reps,biome){
  pb <- txtProgressBar(min = 0, max = reps, style = 3) #To check the progress
  hola<-list()
  for(m in 1:reps){
    setTxtProgressBar(pb, m) 
    df<-region_df
    sample_df<-subset(df,biome==biome)
    sample_df<-sample_df[sample(nrow(sample_df),5000),]
    for(i in c(4,5,10))
    {
      sample_df[[i]]<-as.factor(sample_df[[i]])
    }
    hola[[m]]<- sample_df
  }
  results_col<-list()
  for(i in 1:reps){
    setTxtProgressBar(pb, i) 
    df<-hola[[i]]
    uniquepercolumn<-apply(df,2,unique)
    namescols<-unlist(lapply(uniquepercolumn,length))
    toexclude<-subset(namescols[1:9],namescols[1:9]==1)
    toexclude<-names(toexclude)
    if(length(toexclude>0)){
      df<-df[ , -which(names(df) %in% toexclude)]
    }else{
      df<-df
    }
    results_col[[i]]<- rf_region(df,biome,unique(region_df$region))
  }
  return(results_col)
}

get_data_random_forest<-function(results_rf){
  results<-list()
  como<-list()
  for (i in 1:length(results_rf)) {
    como[[i]]<-results_rf[[i]][[3]]
    results[[i]]<-do.call(rbind.fill,como)
  }
  
  results<-do.call(rbind.fill,results)
  
  
  results$variable<-mapvalues(results$variable,from=c( "built10",
                                                       "croplands7",
                                                       "navwater",
                                                       "pastures",
                                                       "popdensity","roads"), to= c("Built Infrastructure",
                                                                                    "Croplands","Navigable water",
                                                                                    "Pastures",
                                                                                    "Population density","Roads"))
  
  results$biome2<-mapvalues(results$biome,from=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14), to= c("Tropical & Subtropical Moist Broadleaf Forests",
                                                                                          "Tropical & Subtropical Dry Broadleaf Forestsc",
                                                                                          "Tropical & Subtropical Coniferous Forests",
                                                                                         " Temperate Broadleaf & Mixed Forests",
                                                                                          "Temperate Conifer Forests",
                                                                                         "Boreal Forests/Taiga",
                                                                                         "Tropical & Subtropical Grasslands, Savannas & Shrublands",
                                                                                          "Temperate Grasslands, Savannas & Shrublands",
                                                                                          "Flooded Grasslands & Savannas",
                                                                                          "Montane Grasslands & Shrublands",
                                                                                          "Tundra",
                                                                                          "Mediterranean Forests, Woodlands & Scrub",
                                                                                          "Deserts & Xeric Shrublands",
                                                                                          "Mangroves"
  ))
  
  #results$Region<-mapvalues(results$region,from = c("borneo","colombia"),to=c("Borneo","Colombia"))
  
  
  results<-aggregate(Overall ~ biome + region + variable, results, function(x) c(mean = mean(x), sd = sd(x)))
  
  results$mean<-results$Overall[,1]
  results$sd<-results$Overall[,2]
  return(results)
}
