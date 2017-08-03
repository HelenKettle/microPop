#human colon 1
#Note not all NSP is useable therefore must adjust input values so just put in useable amount.
#Simplest example - look at 3 MFGs: 'Bacteroides','NoButyStarchDeg','Acetogens'
#No dependence on pH
  
out=microPopModel(
    microbeNames=c('Bacteroides','NoButyStarchDeg','Acetogens'),
    times=seq(0,4,1/24),
    resourceSysInfo=resourceSysInfoHuman,
    microbeSysInfo=microbeSysInfoHuman,
    plotOptions=list(yLabel='concentration (g/l)',xLabel='time (d)',
                     plotFig=TRUE,sumOverStrains=FALSE,saveFig=FALSE,
                     figType='eps',figName='Human1')
)


