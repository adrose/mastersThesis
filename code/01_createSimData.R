# ---- load-packages -----------------------------------------------------------
library(MplusAutomation)
library(progress)
source("./code/valuesGlobal.R")
source("./code/functionsGlobal.R")


# ---- create-simulated-datasets -----------------------------------------------------------
orgwd <- getwd()
pb <- txtProgressBar(min=0, max = dim(all.perms)[1], style = 3)
for(i in 1:dim(all.perms)[1]){
  ## identify our variables
  n <- all.perms[i,"n"]
  magDif <- all.perms[i,"magDif"]
  facLoad.e <- all.perms[i,"facLoad.e"]
  facLoad.o <- all.perms[i,"facLoad.o"]
  percItems <- all.perms[i,"percItems"]
  nCause <- all.perms[i,"nCause"]
  nIndicator <- all.perms[i,"nIndicator"]
  magEff <- all.perms[i, "magEff"]
  minDif <- all.perms[i,"minDif"]
  facLoad <- rep(c(facLoad.o, facLoad.e), nIndicator/2)
  ## Now create our simulation
  # First create the output directory
  output.dir <- paste("./data/simulatedDataBin/n_", n, "_magDif_", magDif, 
                      "_facLoad_e_", facLoad.e,"_facLoad_o_",facLoad.o,"_percItems_", percItems,
                      "_nIndicator_", nIndicator,"_nCause_",nCause,
                      "_magEff_", magEff,"_minDif_", minDif,
                      "/", sep='')
  if(!dir.exists(output.dir)){dir.create(output.dir)}
  ## Now create the model population values
  mod.simp <- modPopulation(numCaus = nCause, numInd = nIndicator, magDif = magDif, numDif = percItems, magEff = magEff, facLoad = facLoad, minDif = minDif, maxDif = minDif + 2)
  mod.simp.2 <- create_nu_mod(modPop = mod.simp, n = n, dataDir = output.dir, seed = i, nReps = modCount)
  test <- mplusObject(TITLE="base",
                      MONTECARLO = mod.simp.2,
                      MODELPOPULATION = mod.simp$outText)
  ## Now see if the files have already been simulated - if not run the model
  list.name <- paste(output.dir,"MIMICrep_n_", n,"difMag_", magDif, "numInd_", nIndicator, "numCause_", nCause, "list.dat", sep='')
  if(!file.exists(list.name)){out.test <- mplusModeler(test, dataout = output.dir, modelout = paste(output.dir, "simVals.inp", sep='/'), run = 1)}
  ## Now make the directory if it doesn't exist
  setTxtProgressBar(pb, i)
}