#human colon 4
#Note not all NSP is useable therefore must adjust input values so just put in useable amount
#Look at 3 MFGs: 'Bacteroides','NoButyStarchDeg','Acetogens'
#Add in multiple strains
#pH change halfway through simulation as in human2.R

simulation.times=seq(0,4,1/24)

myRateFuncs=rateFuncsDefault
myRateFuncs$pHFunc=function(time,parms){
    if (time<=max(simulation.times)/2){pH=5.5}else{pH=6.5}
    return(pH)
}

out=microPopModel(
    microbeNames=c('Bacteroides','NoButyStarchDeg','Acetogens'),
    times=simulation.times,
    resourceSysInfo=resourceSysInfoHuman,
    microbeSysInfo=microbeSysInfoHuman,
    numStrains=5,
    rateFuncs=myRateFuncs,
    plotOptions=list(yLabel='concentration (g/l)',xLabel='time (d)',
                     plotFig=TRUE,sumOverStrains=FALSE,
                     saveFig=FALSE,figType='eps',figName='Human4'),
    pHLimit=TRUE,
    strainOptions=list(randomParams=c('halfSat','yield','maxGrowthRate','pHtrait'),seed=1,
                       distribution='uniform',percentTraitRange=5,maxPHshift=0.05,
                       applyTradeOffs=TRUE,tradeOffParams=c('halfSat','maxGrowthRate'),
                       paramsSpecified=TRUE,paramDataName=strainParams)
) 

avpHtrait=plotTraitChange(out,'pHtrait',c('Bacteroides','NoButyStarchDeg','Acetogens'),
                          resource.name=NULL,path=NULL,xlabel='Time (days)',
                          saveFig=FALSE,figType='eps',figName='Human4Traits')


