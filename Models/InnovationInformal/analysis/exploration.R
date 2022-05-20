
setwd(paste0(Sys.getenv('CS_HOME'),'/QuantitativeEpistemology/InnovationInformal/Models/InnovationInformal/openmole'))

library(dplyr,warn.conflicts = F)
library(readr)
library(ggplot2)

source(paste0(Sys.getenv('CS_HOME'),'/Organisation/Models/Utils/R/plots.R'))

params<-c("firmSizeScaling","crossOverProba","crossOverShare","mutationProba","mutationAmplitude",
          "currentProductShare","interactionProba","distanceDecay")
indics<-c("bestFitness","averageFitness","fitnessDiff","fitnessEntropy","diversity","interactionIntensity")
indicnames=list(bestFitness=expression(b),
  averageFitness=expression(bar(f)),
  fitnessDiff=expression(Delta*f),
  fitnessEntropy=expression(Epsilon[f]),
  diversity=expression(d),
  interactionIntensity="interactionIntensity")
paramnames <- list(firmSizeScaling=expression(alpha[S]),
crossOverProba=expression(p[C]),
crossOverShare=expression(s[C]),
mutationProba=expression(p[M]),
mutationAmplitude=expression(x[M]),
currentProductShare=expression(s[P]),
interactionProba=expression(p[E]),
distanceDecay=expression(d[E])
)

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
#summary(reldistance("medianbestFitness","sdbestFitness"))

summary(reldistance("meanaverageFitness","sdaverageFitness"))
#summary(reldistance("medianaverageFitness","sdaverageFitness"))

summary(reldistance("meanfitnessDiff","sdfitnessDiff"))
#summary(reldistance("medianfitnessDiff","sdfitnessDiff"))

summary(reldistance("meanfitnessEntropy","sdfitnessEntropy"))
#summary(reldistance("medianfitnessEntropy","sdfitnessEntropy"))

summary(reldistance("meandiversity","sddiversity"))
#summary(reldistance("mediandiversity","sddiversity"))

summary(reldistance("meaninteractionIntensity","sdinteractionIntensity"))
#summary(reldistance("medianinteractionIntensity","sdinteractionIntensity"))



#####
## Exploration plots

# as of 14/03/2022 - 11:34: miss 1800 model runs / 52800
resprefix = '20220314_071017_EXPLORATION'

resdir = paste0(Sys.getenv('CS_HOME'),'/QuantitativeEpistemology/InnovationInformal/Results/',resprefix,'/');dir.create(resdir,recursive = T)

res <- read_csv(file=paste0('exploration/',resprefix,'.csv'))

res$interactionProba=as.character(res$interactionProba)
res$crossOverShare = paste0('s[C]*"="*',res$crossOverShare)
res$firmSizeScaling = paste0('alpha[S]*"="*',res$firmSizeScaling)

for(crossOverProba in unique(res$crossOverProba)){
  for (indic in indics){
    ggsave(
      ggplot(res[res$crossOverProba==crossOverProba,],
             aes_string(x = "distanceDecay", y=indic, color = "interactionProba", group="interactionProba" ))+
      geom_point(pch='.')+geom_smooth()+facet_grid(crossOverShare~firmSizeScaling,scales = 'free',labeller = label_parsed)+
        scale_colour_discrete(name=expression(p[E]))+xlab(expression(d[E]))+ylab(indicnames[[indic]])+stdtheme
      ,filename = paste0(resdir,indic,'-distanceDecay_color-interactionProba_facet-crossOverShare-firmSizeScaling_crossOverProba',crossOverProba,'.png'),width=30,height=20,units='cm'
      )
  }
}


#############
#### optimisation results

resprefix = '20220314_095225_OPTIMISATION';finalgen='10000'
resdir = paste0(Sys.getenv('CS_HOME'),'/QuantitativeEpistemology/InnovationInformal/Results/',resprefix,'/');dir.create(resdir)

res <- read_csv(file=paste0('optimisation/',resprefix,'/population',finalgen,'.csv'))

res = res[res$`evolution$samples`>=5,]

res$diversity = res$diversity...12
res$averageFitness = res$averageFitness...11
res$firmSizeScaling = res$firmSizeScaling...3
res$crossOverShare = res$crossOverShare...5
res$mutationProba = res$mutationProba...6
res$mutationAmplitude = res$mutationAmplitude...7
res$currentProductShare = res$currentProductShare...8
res$interactionProba = res$interactionProba...9
res$distanceDecay = res$distanceDecay...10
res$crossOverProba = res$crossOverProba...4

res$samples = res$`evolution$samples`


res = res[res$diversity< -0.15,]

for(param in params){
  g=ggplot(res,aes_string(x="diversity",y="averageFitness",color=param,size="samples"))
  g+geom_point(alpha=0.6)+xlab("Diversity")+ylab("Utility")+
    scale_size_continuous(name="Samples")+scale_colour_continuous(name=paramnames[[param]])+stdtheme
  ggsave(filename = paste0(resdir,"paretoDiversity-Fitness_color",param,'.png'),width=23,height=20,units='cm')
}

ggplot(res,aes(x=interactionProba,y=diversity,size=samples,colour=distanceDecay))+geom_point(alpha=0.5)#+geom_smooth(span=0.5)

ggplot(res,aes(x=interactionProba,y=averageFitness,size=samples,colour=distanceDecay))+geom_point(alpha=0.5)+geom_smooth()

ggplot(res,aes(x=firmSizeScaling,y=averageFitness,size=samples,colour=distanceDecay))+geom_point(alpha=0.5)+geom_smooth()

fres = res[res$diversity<(-0.4)&res$averageFitness<(-400),]
summary(fres[,params])
colMeans(fres[,params])
apply(fres[,params],2,sd)

