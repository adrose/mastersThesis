# ---- load-packages -----------------------------------------------------------
library(tidyverse)
library(stringr)
source("~/GitHub/adroseHelperScripts/R/afgrHelpFunc.R")

## ---- rmse-ME-AOV ---------------------------------------------------------
all.dat <- readRDS("./data/finalDatasets/difIgnoreMainEffects.RDS")
trim.dat <- all.dat %>% filter(str_detect(rhs , "Y")) %>%
  filter(op == "~") %>%
  mutate(diffVal = abs(est) - magEff) %>%
  mutate(diffValSqu = diffVal^2) %>%
  mutate(meanVal = sqrt(diffValSqu)) %>% 
  mutate(sampSize = as.factor(sampSize),
         magDif = as.factor(magDif),
         facLoad.e = as.factor(facLoad.e),
         facLoad.o = as.factor(facLoad.o),
         nDIFItems = as.factor(nDIFItems),
         magEff = as.factor(magEff),
         minDif = as.factor(minDif),
         facLoad = factor(paste(facLoad.e, facLoad.o),levels = c("0.4 0.4", "0.4 0.8", "0.8 0.4", "0.8 0.8")))

rm(all.dat)
mod.4 <- lm(meanVal ~ (sampSize+magDif+facLoad+nDIFItems+magEff+minDif)^4, data=trim.dat)
mod.4.aov <- aov(meanVal ~ (sampSize+magDif+facLoad+nDIFItems+magEff+minDif)^4, data=trim.dat)
eta.vals <- effectsize::eta_squared(mod.4.aov, partial = FALSE)
eta.vals <- data.frame(eta.vals)
val.4 <- car::Anova(mod.4)


## ---- rmse-IND-AOV ---------------------------------------------------------
## This model requires > 100 GB RAM to run!!!
all.dat <- readRDS(file="./data/finalDatasets/difIgnoreFactorLoadings.RDS")
trim.dat.e <- all.dat %>% filter(str_detect(rhs , "X")) %>% 
  filter(rhs %in% paste("X", c(2,4,6,8,10,12,14,16,18,20), sep='')) %>%
  filter(op == "=~") %>%
  mutate(diffVal = abs(est) - facLoad.e) %>% 
  mutate(meanValR = diffVal^2) %>%
  mutate(meanVal = sqrt(meanValR)) %>% 
  mutate(sampSize = as.factor(sampSize),
         magDif = as.factor(magDif),
         facLoad.e = as.factor(facLoad.e),
         facLoad.o = as.factor(facLoad.o),
         nDIFItems = as.factor(nDIFItems),
         magEff = as.factor(magEff),
         minDif = as.factor(minDif),
         facLoad = factor(paste(facLoad.o, facLoad.e),levels = c("0.4 0.4", "0.4 0.8", "0.8 0.4", "0.8 0.8")), 
         magEff = as.factor(magEff),
         minDif = as.factor(minDif),
         rhs = factor(rhs, levels=paste("X", 1:20, sep='')))
trim.dat.o <- all.dat %>% filter(str_detect(rhs , "X")) %>% 
  filter(rhs %in% paste("X", c(1,3,5,7,9,11,13,15,17,19), sep='')) %>%
  filter(op == "=~") %>%
  mutate(diffVal = abs(est) - facLoad.o) %>% 
  mutate(meanValR = diffVal^2) %>%
  mutate(meanVal = sqrt(meanValR)) %>% 
  mutate(sampSize = as.factor(sampSize),
         magDif = as.factor(magDif),
         facLoad.e = as.factor(facLoad.e),
         facLoad.o = as.factor(facLoad.o),
         nDIFItems = as.factor(nDIFItems),
         magEff = as.factor(magEff),
         minDif = as.factor(minDif),
         facLoad = factor(paste(facLoad.o, facLoad.e),levels = c("0.4 0.4", "0.4 0.8", "0.8 0.4", "0.8 0.8")), 
         magEff = as.factor(magEff),
         minDif = as.factor(minDif),
         rhs = factor(rhs, levels=paste("X", 1:20, sep='')))
trim.dat <- bind_rows(trim.dat.o, trim.dat.e)
rm(all.dat, trim.dat.e, trim.dat.o)
gc()

mod.4 <- lm(meanVal ~ (sampSize+magDif+facLoad+nDIFItems+magEff+minDif)^4, data=trim.dat)
mod.4.aov <- aov(meanVal ~ (sampSize+magDif+facLoad+nDIFItems+magEff+minDif)^4, data=trim.dat)
eta.vals <- effectsize::eta_squared(mod.4.aov, partial = FALSE)
eta.vals <- data.frame(eta.vals)
val.4 <- car::Anova(mod.4)


## ---- class-metrics-AOV ---------------------------------------------------------
## Now organize these data
all.conf.table <- readRDS("./data/finalDatasets/confTableDifIdentify.RDS")
all.conf.table <- as.data.frame(all.conf.table)
all.out <- all.conf.table %>% 
  group_by(sampSize, magDif, facLoad.e, facLoad.o, nDIFItems, magEff, minDif) %>%
  dplyr::mutate(id_ind = 1:n(),
                groupVal = cur_group_id(),
                sampSize = as.factor(sampSize),
                magDif = as.factor(magDif),
                facLoad.e = as.factor(facLoad.e),
                facLoad.o = as.factor(facLoad.o),
                nDIFItems = as.factor(nDIFItems),
                magEff = as.factor(magEff),
                minDif = as.factor(minDif),
                facLoad = factor(paste(facLoad.e, facLoad.o),levels = c("0.4 0.4", "0.4 0.8", "0.8 0.4", "0.8 0.8")))
mod.4.ac <- lm(acc~(sampSize+magDif+facLoad+nDIFItems+magEff+minDif)^4, data=all.out, na.action = "na.exclude")
## Now create the final model using balanced accruacy
mod.4.aov <- aov(acc ~(sampSize+magDif+facLoad+nDIFItems+magEff+minDif)^4, data=all.out)
eta.vals <- effectsize::eta_squared(mod.4.aov, partial = TRUE)
eta.vals <- data.frame(eta.vals)
val.4 <- car::Anova(mod.4.ac)
## ---- rmse-ME-AOV-across ---------------------------------------------------------
all.dat.one <- readRDS("./data/finalDatasets/difRemoveMainEffects.RDS")
all.dat.two <- readRDS("./data/finalDatasets/difIgnoreMainEffects.RDS")
all.dat.one$difStatus <- "Remove"
all.dat.two$difStatus <- "Ignore"
all.dat <- bind_rows(all.dat.one, all.dat.two)
rm(all.dat.one, all.dat.two)
trim.dat <- all.dat %>% filter(str_detect(rhs , "Y")) %>%
  filter(op == "~") %>%
  mutate(diffVal = abs(est) - magEff) %>%
  mutate(diffValSqu = diffVal^2) %>%
  mutate(meanVal = sqrt(diffValSqu)) %>% 
  #mutate(meanVal = sqrt(meanValR)) %>% 
  mutate(sampSize = as.factor(sampSize),
         magDif = as.factor(magDif),
         facLoad.e = as.factor(facLoad.e),
         facLoad.o = as.factor(facLoad.o),
         nDIFItems = as.factor(nDIFItems),
         magEff = as.factor(magEff),
         minDif = as.factor(minDif),
         facLoad = factor(paste(facLoad.e, facLoad.o),levels = c("0.4 0.4", "0.4 0.8", "0.8 0.4", "0.8 0.8"))) %>% 
  filter(diffVal < 10)

rm(all.dat)

## Now try the regression model here
mod.4 <- lm(meanVal ~ (sampSize+magDif+facLoad+nDIFItems+magEff+minDif+difStatus)^4, data=trim.dat)
mod.4.aov <- aov(meanVal ~ (sampSize+magDif+facLoad+nDIFItems+magEff+minDif+difStatus)^4, data=trim.dat)
eta.vals <- effectsize::eta_squared(mod.4.aov, partial = TRUE)
eta.vals <- data.frame(eta.vals)
val.4 <- car::Anova(mod.4)

## ---- rmse-IND-AOV-across ---------------------------------------------------------
all.dat.one <- readRDS(file="./data/finalDatasets/difIgnoreFactorLoadings.RDS")
all.dat.two <- readRDS(file="./data/finalDatasets/difRemoveFactorLoadings.RDS")
all.dat.two$difStatus <- "RemoveFlagged"
all.dat.one$difStatus <- "Ignore"
all.dat <- bind_rows(all.dat.one, all.dat.two)
trim.dat.e <- all.dat %>% filter(str_detect(rhs , "X")) %>% 
  filter(rhs %in% paste("X", c(2,4,6,8,10,12,14,16,18,20), sep='')) %>%
  filter(op == "=~") %>%
  mutate(diffVal = abs(est) - facLoad.e) %>% 
  mutate(meanValR = diffVal^2) %>%
  mutate(meanVal = sqrt(meanValR)) %>% 
  mutate(sampSize = as.factor(sampSize),
         magDif = as.factor(magDif),
         facLoad.e = as.factor(facLoad.e),
         facLoad.o = as.factor(facLoad.o),
         nDIFItems = as.factor(nDIFItems),
         magEff = as.factor(magEff),
         minDif = as.factor(minDif),
         facLoad = factor(paste(facLoad.o, facLoad.e),levels = c("0.4 0.4", "0.4 0.8", "0.8 0.4", "0.8 0.8")), 
         magEff = as.factor(magEff),
         minDif = as.factor(minDif),
         rhs = factor(rhs, levels=paste("X", 1:20, sep=''))) %>% 
  filter(abs(diffVal) < 1)
trim.dat.o <- all.dat %>% filter(str_detect(rhs , "X")) %>% 
  filter(rhs %in% paste("X", c(1,3,5,7,9,11,13,15,17,19), sep='')) %>%
  filter(op == "=~") %>%
  mutate(diffVal = abs(est) - facLoad.o) %>% 
  mutate(meanValR = diffVal^2) %>%
  mutate(meanVal = sqrt(meanValR)) %>% 
  mutate(sampSize = as.factor(sampSize),
         magDif = as.factor(magDif),
         facLoad.e = as.factor(facLoad.e),
         facLoad.o = as.factor(facLoad.o),
         nDIFItems = as.factor(nDIFItems),
         magEff = as.factor(magEff),
         facLoad = factor(paste(facLoad.o, facLoad.e),levels = c("0.4 0.4", "0.4 0.8", "0.8 0.4", "0.8 0.8")), 
         magEff = as.factor(magEff),
         minDif = as.factor(minDif),
         rhs = factor(rhs, levels=paste("X", 1:20, sep=''))) %>% 
  filter(abs(diffVal) < 1)
trim.dat <- bind_rows(trim.dat.o, trim.dat.e)
rm(all.dat, trim.dat.e, trim.dat.o)
gc()

mod.4 <- lm(meanVal ~ (sampSize+magDif+facLoad+nDIFItems+magEff+minDif+difStatus)^4, data=trim.dat)
mod.4.aov <- aov(meanVal ~ (sampSize+magDif+facLoad+nDIFItems+magEff+minDif+difStatus)^4, data=trim.dat)
eta.vals <- effectsize::eta_squared(mod.4.aov, partial = TRUE)
eta.vals <- data.frame(eta.vals)
val.4 <- car::Anova(mod.4)