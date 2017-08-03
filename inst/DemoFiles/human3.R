#human colon 3
#Note not all NSP is useable therefore must adjust input values so just put in useable amount
#Look at 3 MFGs: 'Bacteroides','NoButyStarchDeg','Acetogens'
#Simulate growth in two compartments where the contents of the first flow into the second
#Dependence of microbial growth on pH is included
#pH in the first compartment is 5.5 and 6.5 in the second

simulation.times=seq(0,4,1/24)
num.compartments=2

myRateFuncs=rateFuncsDefault

myRateFuncs$pHFunc=function(time,parms){
    if (compartment==1){pH=5.5}
    if (compartment==2){pH=6.0}
    return(pH)
}

myRateFuncs$entryRateFunc=function(varName,varValue,stateVarValues,
                                   time,inflowRate,parms){
    
  #output is the resource (or microbial strain mass) per unit time
    if (compartment==1){
        gname=getGroupName(varName,microbeNames)
        if (gname%in%parms$microbeNames){
            v=inflowRate[gname]/parms$numStrains
        }else{
            v=inflowRate[varName]
        }
    }else if (compartment==2){
   #now the entry rate is the output from the previous compartment at the given time step
        v=approx(inflow.mat[,'time'],inflow.mat[,varName],time,
                 yleft=inflow.mat[1,varName],
                 yright=inflow.mat[length(inflow.mat[,1]),varName])$y
    }
    return(v)
}

for (compartment in 1:num.compartments){

    print(paste('simulating growth in compartment',compartment,'- please wait!'))
    if (compartment==2){
        inflow.mat=makeInflowFromSoln(out)
    }
    
    out=microPopModel(
        microbeNames=c('Bacteroides','NoButyStarchDeg','Acetogens'),
        times=simulation.times,
        resourceSysInfo=resourceSysInfoHuman,
        microbeSysInfo=microbeSysInfoHuman,
        numStrains=1,
        rateFuncs=myRateFuncs,
        plotOptions=list(yLabel='concentration (g/l)',xLabel='time (d)',
                         plotFig=TRUE,sumOverStrains=FALSE,
                         saveFig=FALSE,figType='eps',
                         figName=paste('Human3comp',compartment,sep='')),
        pHLimit=TRUE
    )
    #save the output from the first compartment (otherwise it is over written)
    if (compartment==1){out1=out}else{out2=out}
}


