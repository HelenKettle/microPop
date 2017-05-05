#human colon 2
#note not all NSP is useable therefore must adjust input values so just put in useable amount
#Look at 3 MFGs: 'Bacteroides','NoButyStarchDeg','Acetogens'
#Dependence of microbial growth on pH is included and pH changes from 5.5 to 6.5 halfway through the simulation time

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
    rateFuncs=myRateFuncs,
    plotOptions=list(yLabel='concentration (g/l)',xLabel='time (d)',plotFig=TRUE,sumOverStrains=FALSE,saveFig=FALSE,figType='eps',figName='Human2'),
    pHLimit=TRUE
)
