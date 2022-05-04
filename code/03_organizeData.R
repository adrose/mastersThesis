# ---- load-packages -----------------------------------------------------------
library(tidyverse)
library(stringr)
source("~/GitHub/adroseHelperScripts/R/afgrHelpFunc.R")


# ---- organize-data-ignore-DIF ---------------------------------------------------------
## Identify all of the models
all.dat <- system("ls ./data/simulatedDataBin/*_*[0-9]PMModsAll.RDS", intern = T)
# Go through these and load each model into a specific iteration
all.mods <- vector(mode="list", length=10)
pb = txtProgressBar(min = 0, max = length(all.dat), initial = 0, style = 3)
pb.tick <- 1
iter.val <- 1
for(i in all.dat){
  ## Load the file
  in.file <- readRDS(i)
  ## Identify the total number of models in this RDS
  name.tmp <- basename(i)
  val.one <- strsplit(name.tmp, "_")[[1]][1]
  val.two <- strsplit(strsplit(name.tmp, "_")[[1]][2], "P")[[1]][1]
  mod.count <- length(seq(val.one, val.two))
  if(length(in.file)==mod.count){
    rm(name.tmp, val.one, val.two)
    ## go through each model and isolate the DIF models based on the number of
    ## purification iterations each one performs
    for(m in 1:mod.count){
      ## load each individual list
      iso.mod <- in.file[[m]]
      ## now grab only the values we are intrested in
      iso.mod <- iso.mod %>% filter(lhs=="f")
      all.mods[[iter.val]] <- iso.mod
      iter.val <- iter.val + 1
    }
  }
  setTxtProgressBar(pb,pb.tick)
  pb.tick <- pb.tick + 1
  
}
close(pb)

## Now separate the multiple outcomes into two separate files and write the data to disk
all.mods.fl <- all.mods %>% bind_rows() %>% 
  filter(op =="=~")
saveRDS(all.mods.fl, file="./data/finalDatasets/difIgnoreFactorLoadings.RDS")
all.mods.me <- all.mods %>% bind_rows() %>% 
  filter(op =="~")
saveRDS(all.mods.me, file="./data/finalDatasets/difIgnoreMainEffects.RDS")

## ---- organize-accuracy-results ---------------------------------------------------------
all.dat <- system("ls ./data/simulatedDataBin/*_*[0-9]ModsAll.RDS", intern = T)
# Go through these and load each model into a specific iteration
all.conf.table <- NULL
pb = txtProgressBar(min = 0, max = length(all.dat), initial = 0, style = 3)
pb.tick <- 1
for(i in all.dat){
  ## Load the file
  in.file <- readRDS(i)
  ## Identify the total number of models in this RDS
  name.tmp <- basename(i)
  val.one <- strsplit(name.tmp, "_")[[1]][1]
  val.two <- strsplit(strsplit(name.tmp, "_")[[1]][2], "M")[[1]][1]
  mod.count <- length(seq(val.one, val.two))
  rm(name.tmp, val.one, val.two)
  ## go through each model and isolate the DIF models based on the number of
  ## purification iterations each one performs
  for(m in 1:mod.count){
    ## load each individual list
    iso.mod <- in.file[[m]]
    if(length(iso.mod)>20){
      ## First find the total number of models we should have
      identVals <- iso.mod[[length(iso.mod)]]
      ## Now grab the model population parameters
      mod.params <- iso.mod[[1]][1,c("magDif", "sampSize","facLoad.e", "facLoad.o", "nDIFItems", "magEff", "minDif", "seedVal")]
      ## Now based on our our metrics create the outcome correct column
      if(mod.params$nDIFItems==2){
        difTrue <- matrix(c(rep(1, 2),rep(0, 18)) , dim(identVals)[1], dimnames = list(c(rownames(identVals))))
      }else if(mod.params$nDIFItems==4){
        difTrue <- matrix(c(rep(1, 4),rep(0, 16)) , dim(identVals)[1], dimnames = list(c(rownames(identVals))))
      }else if(mod.params$nDIFItems==6){
        difTrue <- matrix(c(rep(1, 6),rep(0, 14)) , dim(identVals)[1], dimnames = list(c(rownames(identVals))))
      }
      if(mod.params$magDif==0){
        difTrue <- matrix(0, dim(identVals)[1], dimnames = list(c(rownames(identVals))))
      }
      ## Now bind these and grab the confusion matrix
      flagVals <- rowSums(identVals)
      flagVals <- factor(flagVals, levels = c(0, 1))
      difTrue <- factor(difTrue, levels = c(0, 1))
      table.vals <- table(flagVals, difTrue, dnn = list(c("flagStatus"),c("trueStatus")))
      tn <- table.vals[1,1]
      fp <- table.vals[2,1]
      fn <- table.vals[1,2]
      tp <- table.vals[2,2]
      tpr <- tp / (tp + fn)
      fnr <- fn / (tn + fn)
      tnr <- tn / (tn + fp) # this is also recall
      fpr <- fp / (tn + fp)
      precision <- tp / (tp + fp) # out of all + what % is true positive
      recall <- tnr
      f1.score <- (2 * (precision * recall)) / (precision + recall)
      acc <- (tp + tn) / (tp + fp + tn + fn)
      balAcc <- (tpr + tnr) / 2
      rounds <- sum(colSums(identVals)!=0)
      all.vals <- cbind(mod.params,tn, fp, fn, tp, fpr, fnr, tnr, tpr, f1.score, acc, balAcc, rounds)
      all.conf.table <- rbind(all.conf.table, all.vals)
    }
  }
  setTxtProgressBar(pb,pb.tick)
  pb.tick <- pb.tick + 1
}
close(pb)

## Now export this data
saveRDS(all.conf.table, file="./data/finalDatasets/confTableDifIdentify.RDS")


# ---- organize-data-DIF-ident ---------------------------------------------------------
## Identify all of the models
all.dat <- system("ls ./data/simulatedDataBin/*_*[0-9]ModsAll.RDS", intern = T)
# Go through these and load each model into a specific iteration
all.mods <- vector(mode="list", length=10)
pb = txtProgressBar(min = 0, max = length(all.dat), initial = 0, style = 3)
pb.tick <- 1
for(i in all.dat){
  ## Load the file
  in.file <- readRDS(i)
  ## Identify the total number of models in this RDS
  name.tmp <- basename(i)
  val.one <- strsplit(name.tmp, "_")[[1]][1]
  val.two <- strsplit(strsplit(name.tmp, "_")[[1]][2], "M")[[1]][1]
  mod.count <- length(seq(val.one, val.two))
  rm(name.tmp, val.one, val.two)
  ## go through each model and isolate the DIF models based on the number of
  ## purification iterations each one performs
  for(m in 1:mod.count){
    ## load each individual list
    iso.mod <- in.file[[m]]
    if(length(iso.mod)>20){
      ## First find the total number of models we should have
      identVals <- iso.mod[[length(iso.mod)]]
      ## Now see how many rounds were performed
      roundCount <- sum(colSums(identVals)!=0)
      itemBank <- 20 ## Need to find a way to make this adaptive
      mods.per.round <- list()
      mods.per.round[[1]] <- 20
      ## Now identify the length of each round
      if(roundCount>0){
        iter <- 2
        for(w in 1:roundCount){
          itemBank <- itemBank - colSums(identVals)[w]
          mods.per.round[[iter]] <- itemBank
          iter <- iter + 1
        }
      }
      ## Now isolate the mods within each round of purification
      for(w in 1:length(mods.per.round)){
        mods.export <- iso.mod[1:mods.per.round[[w]][1]]
        ## Now organize these
        mods.export <- mods.export %>% bind_rows() %>% 
          mutate(roundCount = w) %>% 
          filter(str_detect(lhs , "X") & op =="~")
        all.mods[[w]] <- append(all.mods[[w]], list(mods.export))
        ## 
        iso.mod <- iso.mod[-c(1:mods.per.round[[w]][1])]
      }
    }
  }
  setTxtProgressBar(pb,pb.tick)
  pb.tick <- pb.tick + 1
}
close(pb)
## Save the output list
saveRDS(all.mods, file="./data/finalDatasets/difExploreIndivdualItem.RDS")

# ---- organize-data-remove-DIF-flagged ---------------------------------------------------------
## Identify all of the models
all.dat <- system("ls ./data/simulatedDataBin/*_*[0-9]TPMModsAll.RDS", intern = T)
# Go through these and load each model into a specific iteration
all.mods <- vector(mode="list", length=10)
pb = txtProgressBar(min = 0, max = length(all.dat), initial = 0, style = 3)
pb.tick <- 1
iter.val <- 1
for(i in all.dat){
  ## Load the file
  in.file <- readRDS(i)
  ## Identify the total number of models in this RDS
  name.tmp <- basename(i)
  val.one <- strsplit(name.tmp, "_")[[1]][1]
  val.two <- strsplit(strsplit(name.tmp, "_")[[1]][2], "T")[[1]][1]
  mod.count <- length(seq(val.one, val.two))
  if(length(in.file)==mod.count){
    rm(name.tmp, val.one, val.two)
    ## go through each model and isolate the DIF models based on the number of
    ## purification iterations each one performs
    for(m in 1:mod.count){
      ## load each individual list
      iso.mod <- in.file[[m]]
      ## now grab only the values we are intrested in
      iso.mod <- iso.mod %>% filter(lhs=="f")
      all.mods[[iter.val]] <- iso.mod
      iter.val <- iter.val + 1
    }
  }
  setTxtProgressBar(pb,pb.tick)
  pb.tick <- pb.tick + 1
  
}
close(pb)

all.mods.fl <- all.mods %>% bind_rows() %>% 
  filter(op =="=~")
saveRDS(all.mods.fl, file="./data/finalDatasets/difRemoveFactorLoadings.RDS")
all.mods.me <- all.mods %>% bind_rows() %>% 
  filter(op =="~")
saveRDS(all.mods.me, file="./data/finalDatasets/difRemoveMainEffects.RDS")