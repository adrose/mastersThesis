# ---- load-packages -----------------------------------------------------------
library(lavaan)
library(doParallel)
library(parallel)
library(foreach)
source("~/GitHub/adroseHelperScripts/R/afgrHelpFunc.R")

# ---- train-MIMIC ---------------------------------------------------------
all.pure.models <- bettermc::mclapply(all.perms.list,mc.cores = 4,
                                      FUN = function(x) all.steps.dif(n = x$n, magDif = x$magDif, facLoad.e = x$facLoad.e,facLoad.o = x$facLoad.o,
                                                                      percItems = x$percItems,nCause = x$nCause, nIndicator = x$nIndicator, magEff = x$magEff, minDif = x$minDif,
                                                                      dataDir = "./data/simulatedDataBin/", seedVal = x$seedVal, mplusDat = TRUE, pureMim = TRUE, writeFile = TRUE),
                                      mc.preschedule = TRUE,
                                      mc.progress=TRUE
)

# ---- train-MIMIC-purification ---------------------------------------------------------
all.pure.models <- bettermc::mclapply(all.perms.list,mc.cores = 4,
                              FUN = function(x) all.steps.dif(n = x$n, magDif = x$magDif, facLoad.e = x$facLoad.e,facLoad.o = x$facLoad.o,
                                                              percItems = x$percItems,nCause = x$nCause, nIndicator = x$nIndicator, magEff = x$magEff, minDif = x$minDif,
                                                              dataDir = "./data/simulatedDataBin/", seedVal = x$seedVal, mplusDat = TRUE, pureMim = FALSE, writeFile = TRUE),
                              mc.preschedule = TRUE,
                              mc.progress=TRUE
)