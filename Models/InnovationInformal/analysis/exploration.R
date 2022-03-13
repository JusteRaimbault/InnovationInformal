
setwd(paste0(Sys.getenv('CS_HOME'),'/QuantitativeEpistemology/InnovationInformal/Models/InnovationInformal/openmole'))

library(dplyr)
library(readr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

params<-c("firmSizeScaling","crossOverProba","crossOverShare","mutationProba","mutationAmplitude",
          "currentProductShare","interactionProba","distanceDecay")
indics<-c("bestFitness","averageFitness","fitnessDiff","fitnessEntropy","diversity","interactionIntensity")

# stochasticity
resprefix = '20220313_214507_STOCHASTICITY'
resdir = paste0(Sys.getenv('CS_HOME'),'/QuantitativeEpistemology/InnovationInformal/Results/',resprefix,'/');dir.create(resdir,recursive = T)

res <- read_csv(file=paste0('exploration/',resprefix,'.csv'))

# histograms? no need

# sharpes
sres = res %>% group_by(id) %>% summarise(
  sdbestFitness=sd(bestFitness),meanbestFitness=mean(bestFitness),medianbestFitness=median(bestFitness),sharpebestFitness=abs(meanbestFitness/sdbestFitness),
  sdaverageFitness=sd(averageFitness),meanaverageFitness=mean(averageFitness),medianaverageFitness=median(averageFitness),sharpeaverageFitness=abs(meanaverageFitness/sdaverageFitness),
  sdfitnessDiff=sd(fitnessDiff),meanfitnessDiff=mean(fitnessDiff),medianfitnessDiff=median(fitnessDiff),sharpefitnessDiff=abs(meanfitnessDiff/sdfitnessDiff),
  sdfitnessEntropy=sd(fitnessEntropy),meanfitnessEntropy=mean(fitnessEntropy),medianfitnessEntropy=median(fitnessEntropy),sharpefitnessEntropy=abs(meanfitnessEntropy/sdfitnessEntropy),
  sddiversity=sd(diversity),meandiversity=mean(diversity),mediandiversity=median(diversity),sharpediversity=abs(meandiversity/sddiversity),
  sdinteractionIntensity=sd(interactionIntensity),meaninteractionIntensity=mean(interactionIntensity),medianinteractionIntensity=median(interactionIntensity),sharpeinteractionIntensity=abs(meaninteractionIntensity/sdinteractionIntensity)
)
summary(sres)

# relative distances
reldistance <- function(indic,sdindic){
  c(2*abs(matrix(rep(sres[[indic]],nrow(res)),nrow = nrow(res),byrow = T) - matrix(rep(sres[[indic]],nrow(res)),nrow = nrow(res),byrow = F))/(matrix(rep(sres[[sdindic]],nrow(res)),nrow = nrow(res),byrow = T) + matrix(rep(sres[[sdindic]],nrow(res)),nrow = nrow(res),byrow = F)))
}

summary(reldistance("meanbestFitness","sdbestFitness"))
summary(reldistance("medianbestFitness","sdbestFitness"))

summary(reldistance("meanaverageFitness","sdaverageFitness"))
summary(reldistance("medianaverageFitness","sdaverageFitness"))

summary(reldistance("meanfitnessDiff","sdfitnessDiff"))
summary(reldistance("medianfitnessDiff","sdfitnessDiff"))

summary(reldistance("meanfitnessEntropy","sdfitnessEntropy"))
summary(reldistance("medianfitnessEntropy","sdfitnessEntropy"))

summary(reldistance("meandiversity","sddiversity"))
summary(reldistance("mediandiversity","sddiversity"))

summary(reldistance("meaninteractionIntensity","sdinteractionIntensity"))
summary(reldistance("medianinteractionIntensity","sdinteractionIntensity"))
