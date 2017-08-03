#Bacteria with viruses
#B=number of bacterial cells
#V=number of virus cells
#B feeds on N, V feeds on B
run=as.numeric(readline('Choose case number by entering number from 1 to 4: '))

run1='B1 & B2 feed on N'
run2='B1 & B2 feed on N, V1 attacks B1'
run3='B1 & B2 feed on N, V1 attacks B1, V2 attacks B2'
run4='B1 & B2 feed on N, V1 attacks B1, V2 attacks B2,
B1 becomes resistant to V1 with mutation rate fB'

runtxt=c(run1,run2,run3,run4)

fB=0 # fraction of Bacteria1 population that mutates to a resistant strain per day

#create group dataframes 
grpParNames=c("halfSat","yield","maxGrowthRate","Rtype","stoichiom","keyResource")
Bmat=matrix(c(0.001,0.3,12,'Se',NA,'Nutrient'),nrow=6,ncol=1)
rownames(Bmat)=grpParNames; colnames(Bmat)=c('Nutrient')
Bacteria1=data.frame(Bmat,stringsAsFactors=FALSE)
resistantBacteria1=Bacteria1
Bacteria2=Bacteria1; Bacteria2['maxGrowthRate','Nutrient']=10

Vmat=matrix(c(NA,10,1,'Sm',NA,'Bacteria1'),nrow=6,ncol=1)
rownames(Vmat)=grpParNames; colnames(Vmat)=c('Bacteria1')
Virus1=data.frame(Vmat,stringsAsFactors=FALSE)
Virus2=Virus1; colnames(Virus2)='Bacteria2'; Virus2['keyResource',1]='Bacteria2'

allMicrobeNames=c('Bacteria1','Bacteria2','Virus1','Virus2','resistantBacteria1')
mutation.frac=rep(0,length(allMicrobeNames)); names(mutation.frac)=allMicrobeNames

myRateFuncs=rateFuncsDefault

myRateFuncs$growthLimFunc=function(strainName,groupName,pathName,
                                   varName,resourceValues,allSubType,strainHalfSat,stateVarValues){
    #Returns the value of growthLim (must lie in interval [0,1] i.e. unitless) which is used to scale the maximum growth rate
    
    if (resourceValues[varName]<=0){
        v=0
    }else{
        
        if (varName=='Nutrient'){
            v=resourceValues[varName]/(resourceValues[varName]+strainHalfSat[varName])
        }else{
            if (varName=='Bacteria1' | varName=='Bacteria2'){ # growth on Bacteria1
                v=resourceValues[varName]
            }        
        }
    }
    return(max(v,0))
}

myRateFuncs$entryRateFunc=function(varName,varValue,
                                   stateVarValues,time,inflowRate,parms){

    gname=getGroupName(varName,parms$microbeNames)
    if (gname%in%parms$microbeNames){
        v.in=inflowRate[gname]/parms$numStrains
    }else{
        v.in=inflowRate[varName]
    }

    #add in mutations from Bacteria1 to resistant group resistantBacteria1
    vr=0
    if (gname=='resistantBacteria1'){vr=fB*stateVarValues['Bacteria1']}
    if (gname=='Bacteria1'){vr=-fB*stateVarValues['Bacteria1']}
        
    return(v.in+vr)
}

print(paste('run',run))
print(runtxt[run])
if (run==1){groupNums=c(1,2); fB=0}
if (run==2){groupNums=c(1,2,3); fB=0}
if (run==3){groupNums=c(1,2,3,4); fB=0}
if (run==4){groupNums=c(1,2,3,4,5); fB=0.001}

microbeNames=allMicrobeNames[groupNums]
if (run==4){mutation.frac['Bacteria1']=-0.001
    mutation.frac['resistantBacteria1']=0.001}

out=microPopModel(
    microbeNames=microbeNames,
    times=seq(0,150,0.1),
    resourceSysInfo=systemInfoResourcesVirus,
    microbeSysInfo=systemInfoMicrobesVirus,
    rateFuncs=myRateFuncs,
    checkingOptions=list(checkStoichiomBalance=FALSE),
    plotOptions=list(yLabel='Number of cells',saveFig=FALSE,
                     figType='eps',figName=paste('Virus',run,sep='')),
        odeOptions=list('atol'=1e-8,'rtol'=1e-8)
)
    

