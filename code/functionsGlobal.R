modPopulation <- function(numCaus=1, numInd=6, magDif=.3, numDif=1, facLoad = .6, magEff=.3, sub.val = .15, add.val=.15, seedVal=16){
  ## Set seed
  set.seed(seedVal)
  ## Now check for null effects
  if(magDif==0){
    add.val=0
    sub.val=0
  }
  ## This function will return the necessary text for the MODEL POPULATION statement in mplus
  ## This first section will return all of the scaled indicator variables
  ## Prep all of the indicator variables
  facLoad.sqr <- facLoad^2
  resid.var <- 1 - facLoad.sqr
  ## check to see if an integer or a character vector was provided for the indicator var 
  if(class(numInd)=="numeric"){
    sec.two <- paste0(
      paste("\n", collapse = "\n"),
      paste0("X", 1:numInd, "~~",resid.var,"*X", 1:numInd ,sep="", collapse = "\n")
    )
  }else if(class(numInd)=="character"){
    sec.two <- paste0(
      paste("\n", collapse = "\n"),
      paste0(numInd, "~~",resid.var,"*", numInd ,sep="", collapse = "\n")
    )    
  }
  ## Now create our factor variable -- only 1 for the moment
  ## Now randomly assign the factor loadings
  min.val <- facLoad-sub.val
  max.val <- facLoad +add.val
  fac.load.vec <- round(runif(n = numInd, min = min.val, max = max.val), 3)
  if(class(numInd)=="numeric"){
    sec.thr <- paste0("\nf =~", paste0(fac.load.vec,"*X", 1:numInd, collapse = "+") ,collapse = "")
  }else if(class(numInd)=="character"){
    sec.thr <- paste0("\nf =~", paste0(fac.load.vec,"*", numInd,collapse = "+") ,collapse = "")  
  }
  ## Assign variance of the factor
  sec.fou <- paste0("\nf~~1*f;\n",collapse = "")
  ##Assign the DIF here
  min.val.dif <- magDif -sub.val
  max.val.dif <- magDif +add.val
  magDif.vec <- round(runif(numDif, min = min.val.dif, max = max.val.dif), digits = 3)
  sec.fiv <- paste0(
    paste("\n", collapse = "\n"),
    paste0("Y", 1:numCaus, "~",paste0(magDif.vec,"*X", 1:numDif ,sep="", collapse = "+"), collapse = "\n")
  )
  ## Assign magnitude of ME here
  sec.six <- paste0(
    paste("\n", collapse = "\n"),
    paste0("f", 1:numCaus, "~",magEff ,"*Y", 1:numCaus ,sep="", collapse = "\n")
  )
  ## Constrain indicator variance here
  magEff.sqr <- magEff^2
  resid.var.me <- 1 - magEff.sqr
  sec.sev <- paste0(
    #paste("\n", collapse = "\n"),
    #paste0("[Y", 1:numInd, "@0];",sep="", collapse = "\n"),
    paste("\n", collapse = "\n"),
    paste0("Y", 1:numCaus, "~~",resid.var.me,"*Y", 1:numCaus ,sep="", collapse = "\n")
  )
  ## Combine all of these
  all.out <- list(outText=paste0(sec.thr,sec.fiv, sec.six), outCau = numCaus, outDif=numDif, outMag = magDif, outInd=numInd, facLoad=facLoad, facLoadVec = fac.load.vec, magDifVec=magDif.vec)
  return(all.out)
}

modTest <- function(modPop = modPopulation()){
  ## This function will reutnr a list of all of the unifrom DIF models to test given a set of input
  ## causal varaibles and indicator variables simulated through a population model
  ## First identify all of the indicator variables to iterate through
  indVals <- modPop$outInd
  causVals <- modPop$outCau
  ## Now prepare the model statements
  if(class(indVals)=="numeric"){
    u_mod <- function(item_num){paste0(
      paste0("f =~",paste0("X", 1:indVals,collapse = "+")),
      paste("\n", collapse = "\n"),
      paste0("f ~",paste0("Y", 1:causVals,collapse = "+")),
      paste("\n", collapse = "\n"),
      paste0("X",item_num, " ~ ", paste0("Y" ,1:causVals, collapse = "+"))
    )
    }
    all.mods <- lapply(1:indVals, u_mod)
  }
  if(class(indVals)=="character"){
    u_mod <- function(item_num){paste0(
      paste0("f =~",paste0(indVals,collapse = "+")),
      paste("\n", collapse = "\n"),
      paste0("f ~",paste0("Y", 1:causVals,collapse = "+")),
      paste("\n", collapse = "\n"),
      paste0(item_num, " ~ ", paste0("Y" ,1:causVals, collapse = "+"))
    )
    }
    all.mods <- lapply(indVals, u_mod)
  }  
  ## Now grab all models
  #all.mods <- lapply(1:indVals, u_mod)
  ## Now return these
  return(all.mods)
}

modBase <- function(modPop = modPopulation()){
  ## This function will reutnr a list of all of the unifrom DIF models to test given a set of input
  ## causal varaibles and indicator variables simulated through a population model
  ## First identify all of the indicator variables to iterate through
  indVals <- modPop$outInd
  causVals <- modPop$outCau
  ## Now prepare the model statements
  if(class(indVals)=="numeric"){
    u_mod <- function(item_num){paste0(
      paste0("f =~",paste0("X", 1:indVals,collapse = "+")),
      paste("\n", collapse = "\n"),
      paste0("f ~",paste0("Y", 1:causVals,collapse = "+")),
      paste("\n", collapse = "\n")
    )
    }
    all.mods <- lapply(1:indVals, u_mod)
  }
  if(class(indVals)=="character"){
    u_mod <- function(item_num){paste0(
      paste0("f =~",paste0(indVals,collapse = "+")),
      paste("\n", collapse = "\n"),
      paste0("f ~",paste0("Y", 1:causVals,collapse = "+")),
      paste("\n", collapse = "\n")
    )
    }
    all.mods <- lapply(indVals, u_mod)
  }  
  ## Now grab all models
  all.mods <- u_mod(item_num = 1) # Providing item num although we don't really need it -- jsut lazy coding here
  ## Now return these
  return(all.mods)
}

rm.files <- function(n = 500, magDif = .5, facLoad = .8, facLoad.e = .4, facLoad.o = .4, percItems = 1, nCause = 1, nIndicator=6, magEff = .5, minDif = -1, dataDir = "./data/simulatedData/", seedVal = 16, pureMim = FALSE){
  ## Define the output
  output.dir <- paste("./data/simulatedDataBin/n_", n, "_magDif_", magDif, 
                      "_facLoad_e_", facLoad.e,"_facLoad_o_",facLoad.o,"_percItems_", percItems,
                      "_nIndicator_", nIndicator,"_nCause_",nCause,
                      "_magEff_", magEff,"_minDif_", minDif,
                      "/", sep='')
  out.file <- paste0(output.dir, seedVal, "_lavaanMods.RData", sep='')
  if(pureMim){
    out.file <- paste0(output.dir, seedVal, "_lavaanModMIMIC.RData", sep='')
  }
  ## Now remove the file
  if(file.exists(out.file)){file.remove(out.file)}
}

all.steps.dif <- function(n = 500, magDif = .5, facLoad = .8, facLoad.e = .4, facLoad.o = .4, percItems = 1, nCause = 1, nIndicator=6, magEff = .5, minDif = -1, dataDir = "./data/simulatedData/", seedVal = 16, pureMim = FALSE, mplusDat = FALSE, writeFile=FALSE, checkPrior=FALSE){
  ## Load library(s)
  library(dplyr)
  library(lavaan)
  ## Define the output
  output.dir <- paste(dataDir, "n_", n, "_magDif_", magDif, 
                      "_facLoad_e_", facLoad.e,"_facLoad_o_",facLoad.o,"_percItems_", percItems,
                      "_nIndicator_", nIndicator,"_nCause_",nCause,
                      "_magEff_", magEff,"_minDif_", minDif,
                      "/", sep='')
  nDIFItems <- percItems
  facLoad <- rep(c(facLoad.o, facLoad.e), nIndicator/2)
  ## Make the directory if it does not exist
  if(!dir.exists(output.dir)){system(paste("mkdir ", output.dir))}
  ## First simulate the data -- not sure how to handle seed, not sure if worth worrying about it atm
  popMod <- modPopulation(magDif = magDif, numCaus = nCause, numInd = nIndicator, 
                          numDif = percItems, facLoad = facLoad, magEff = magEff, seedVal = seedVal)
  if(!mplusDat){
    set.seed(seedVal)
    myData <- try(simstandard::sim_standardized(m=popMod$outText, n=n, errors = FALSE, latent = FALSE), silent=FALSE)
    
    if(is.null(nrow(myData))){
      ## Now run w/o standardized because cov broke the mvrnorm call
      myData <- try(lavaan::simulateData(model = popMod$outText, sample.nobs = n, model.type = "sem", orthogonal = FALSE, 
                                         std.lv = TRUE, standardized = FALSE, seed = seedVal, return.fit = TRUE), silent=FALSE)
      
    }
  }else{
    myData <- read.table(paste0(output.dir,"MIMICrep_", "n_", n, "difMag_", popMod$outMag, "numInd_", popMod$outInd, "numCause_", popMod$outCau,seedVal,".dat",collapse = ""))
    colnames(myData) <- c(paste("X", 1:nIndicator, sep=''), paste("Y", nCause, sep=''))
  }
  ## Now create our DIF models
  mod.dif <- modTest(modPop = popMod)
  out.file <- paste0(output.dir, seedVal, "_lavaanMods.RData", sep='')
  if(pureMim){
    mod.dif <- modBase(modPop = popMod)
    out.file <- paste0(output.dir, seedVal, "_lavaanModMIMIC.RData", sep='')
  }
  if(pureMim & !is.null(checkPrior)){
    ## Find which items to remove
    if(sum(checkPrior!=0)){
      nonsigDif <- rownames(checkPrior)[which(rowSums(checkPrior)==0)]
      ## Now create the new vector of indicator variables
      popMod <- modPopulation(magDif = magDif, numCaus = nCause, numInd = nonsigDif, 
                              numDif = nDIFItems, facLoad = facLoad, magEff = magEff, seedVal = seedVal)
      mod.dif <- modBase(modPop = popMod)
      out.file <- paste0(output.dir, seedVal, "_lavaanModMIMICTrim.RData", sep='')
    }
  }
  ## Now train the models
  ## Check to see if file exists
  if(!file.exists(out.file)){
    # Check to see if we have any indicators without any variance
    if(sum(diag(var(myData))==0)==0){
      moreMods <- TRUE
      identDif <- matrix(0, nrow = nIndicator, ncol=10)
      rownames(identDif) <- colnames(myData)[1:nIndicator]
      iter <- 1
      all.out <- list()
      while(moreMods){
        ## Add a while loop here - while any partial mediation exmples exist run these mods
        all.mods <- lapply(mod.dif, function(x) suppressWarnings(sem(model = x, data = myData, std.lv=T, ordered = colnames(myData)[1:nIndicator])))
        ## Now go ahead and isolate the parameter estimates only
        all.mods <- lapply(all.mods, lavaan::parameterestimates)
        ## Now add the model params to each model
        for(i in 1:length(all.mods)){
          all.mods[[i]]$sampSize = n
          all.mods[[i]]$magDif = magDif
          all.mods[[i]]$facLoad.e =facLoad.e
          all.mods[[i]]$facLoad.o =facLoad.o
          all.mods[[i]]$nDIFItems = nDIFItems
          all.mods[[i]]$nCause = nCause
          all.mods[[i]]$nIndicator = nIndicator
          all.mods[[i]]$magEff = magEff
          all.mods[[i]]$minDif = minDif
          all.mods[[i]]$seedVal = seedVal
        }
        ## Now filter these to the direct indicator paths
        nonsigDif <- bind_rows(all.mods) %>% 
          filter(op == "~" & lhs != "f" & pvalue > .05) %>% 
          select(lhs)
        sigDif <- bind_rows(all.mods) %>% 
          filter(op == "~" & lhs != "f" & pvalue <= .05) %>% 
          select(lhs)
        all.out <- append(all.out, all.mods)
        ## Add these to the binarymatrix
        if(dim(sigDif)[1]>0){
          ## Flag the rows  
          identDif[sigDif$lhs,iter] <- 1
          ## Now add 1 to the iter
          iter <- iter + 1
          ## Now create the new vector of indicator variables
          popMod <- modPopulation(magDif = magDif, numCaus = nCause, numInd = nonsigDif$lhs, 
                                  numDif = nDIFItems, facLoad = facLoad, magEff = magEff, seedVal = seedVal)
          mod.dif <- modTest(modPop = popMod)
        }else{
          moreMods <- FALSE
          identDif <- as.data.frame(identDif)
          all.out <- append(all.out, list(identDif))
          if(writeFile){saveRDS(all.out, out.file)}
        }
      }
      ## Now save the models
      #saveRDS(object=all.mods, file = out.file)
    } else{
      out.file <- paste("NO VAR IND")
    }
  }else{
    all.out <- readRDS(out.file)
  }
  # and fin
  return(all.out)
}

create_nu_mod <- function(modPop=modPopulation(), n=500, nReps=50, seed=16, dataDir="./data/trainedModels/") {
  if(is.null(modPop)){
    errorCondition("Please provide population model from modPopulation function")
  }
  nu_mod <- paste0(
    "MONTECARLO:\n",
    ## Begin with the names statement
    paste0("NAMES ARE Y1-Y", modPop$outInd, " X1-X", modPop$outCau,";\n" ,collapse = ""),
    ## Now do the number of observations
    paste0("NOBSERVATIONS = ", n,";\n" ,collapse = ""),
    ## Number of models
    paste0("NREPS = ", nReps,";\n" ,collapse = ""),
    ## SEED
    paste0("SEED = ", seed,";\n" ,collapse = ""),
    ## Save output
    paste0("REPSAVE = All",";\n" ,collapse = ""),
    ## data dir
    paste0("SAVE = ", "MIMICrep_", "n_", n, "difMag_", modPop$outMag, "numInd_", modPop$outInd, "numCause_", modPop$outCau, "*.dat;\n", collapse = "")
  )
  
  return(nu_mod)
}


modMCL <- function(all.mod=modTest(), dataDir="./data/trainedModels/", n=500, modPop=NULL){
  ## First identify the data files
  all.dat <- system(paste("ls", paste0(dataDir, "MIMICrep_", "n_", n, "difMag_", modPop$outMag, "numInd_", modPop$outInd, "numCause_", modPop$outCau, "list.dat")), intern = T)
  all.dat <- read.table(all.dat)
  ## Now iterate through each of these and train the models
  out.params <- list()
  iterator <- 1
  for(q in all.dat$V1){
    ## Train the models
    in.dat <- read.table(paste(dataDir, "/", all.dat$V1[iterator], sep=''))
    mimic_mod <- mplusObject(
      autov = FALSE,
      TITLE  = "MIMIC DIF sim;",
      #VARIABLE = paste0("V",1:dim(in.dat)[2]),
      MODEL = all.mod,
      rdata=in.dat,
      usevariables = paste0("V",1:dim(in.dat)[2]),
      
      imputed = FALSE)
    ## Now train the model
    # create random file names
    tmp.val <- sample(1:1000000, size = 1)
    dataOuttmp <- paste("numod", tmp.val, ".txt", sep='')
    modelOuttmp <- paste("numod", tmp.val, ".inp", sep='')
    modelOutrestmp <- paste("numod", tmp.val, ".out", sep='')
    nu_model <- mplusModeler(mimic_mod,dataout=dataOuttmp,modelout=modelOuttmp,run=1)
    system(paste("rm ", dataOuttmp))
    system(paste("rm ", modelOuttmp))
    system(paste("mv ", modelOutrestmp, dataDir))
    ## Now grab params of int
    out.params [[iterator]] <- nu_model$results
    iterator <- iterator + 1
  }
  return(out.params)
}
