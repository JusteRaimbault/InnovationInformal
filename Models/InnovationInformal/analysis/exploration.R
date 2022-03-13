
setwd(paste0(Sys.getenv('CS_HOME'),'/QuantitativeEpistemology/InnovationInformal/Models/InnovationInformal/openmole'))

library(dplyr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

params<-c("firmSizeScaling","crossOverProba","crossOverShare","mutationProba","mutationAmplitude",
          "currentProductShare","interactionProba","distanceDecay")
indics<-c("bestFitness","averageFitness","fitnessDiff","fitnessEntropy","diversity","interactionIntensity")

# stochasticity
resprefix = '20220313_214507_STOCHASTICITY'
resdir = paste0(Sys.getenv('CS_HOME'),'/QuantitativeEpistemology/InnovationInformal/Results/',resprefix,'/');dir.create(resdir,recursive = T)

res <- as.tbl(read.csv(file=paste0('exploration/',resprefix,'.csv')))

