#rumen model
#3 MFGs: Xsu, Xaa, Xh2
#No dependence on pH

microbeNames=c('Xsu','Xh2','Xaa')
polymer.names=c('Zndf','Znsc','Zpro')

#Need to add polymers as extra resources c('Zndf','Znsc','Zpro') to a microbe dataframe so that they become state variables
Xsu[['Zndf']]=c('X',rep(NA,6))
Xsu[['Znsc']]=c('X',rep(NA,6))
Xsu[['Zpro']]=c('X',rep(NA,6))

time.hours=seq(0,3,1/(24*4))*24

#parameters
kd = 8.3333e-4 #death rate of microbes
f.X=c(0.2,0.55); names(f.X)=c('Znsc','Zpro') #fraction of dead cells that turn to polymers
khyd=c(0.05,0.20,0.22); names(khyd)=c('Zndf','Znsc','Zpro')#hydrolysis of polymers

myRateFuncs=rateFuncsDefault

myRateFuncs$removalRateFunc=function(varName,varValue,stateVarValues,
                                     time,washOut,parms){
    death=0;    hydrolysis=0       
    if (varValue<=0){
        v=0
    }else{
        if (varName%in%polymer.names){
            hydrolysis=khyd[varName]}#hydrolysis of polymers
        if (getGroupName(varName,microbeNames)%in%microbeNames){death=kd}#death of microbes
        v=(washOut[varName] + hydrolysis + death)*varValue 
    }
    return(v)
}

myRateFuncs$entryRateFunc=function(varName,varValue,stateVarValues,
                                   time,inflowRate,parms){

    #entry rate from outside the system----------------------------------------------
    gname=getGroupName(varName,microbeNames)
    if (varName%in%parms$microbeNames){
        v.in=inflowRate[gname]/parms$numStrains
    }else if (varName%in%polymer.names){
        v.in=inflowRate[varName]
    }else{
        v.in=inflowRate[varName]
    }

    #entry rate from inside the system-----------------------------------------------
    hydrolysis=0;      input.from.dead.cells=0
    
    #entry rate of sugar from hydrolysed polymers
    if (varName=='Ssu'){
        hydrolysis=sum(khyd[c('Zndf','Znsc')]*stateVarValues[c('Zndf','Znsc')])}#g/L/h

    #entry rate of amino acids from hydrolysed proteins
    if (varName=='Saa'){
        hydrolysis=khyd['Zpro']*stateVarValues['Zpro']}#g/L/h
  
    #dead microbial cells become Znsc and Zpro
    if (varName=='Znsc' | varName=='Zpro'){
        input.from.dead.cells=f.X[varName]*kd*sum(stateVarValues[parms$microbeNames])} #g/L/h

    v=v.in+hydrolysis+input.from.dead.cells
    return(v)
}


z.in=c(1,5,10,15,20) 
n=length(z.in)
methane=array(NA,dim=c(length(time.hours),n,3))
X=array(NA,dim=c(length(time.hours),n,3))
H2=array(NA,dim=c(length(time.hours),n,3))
SIC=array(NA,dim=c(length(time.hours),n,3))
nh3=array(NA,dim=c(length(time.hours),n,3))
Xa=array(NA,dim=c(length(time.hours),n,3))
Sa=array(NA,dim=c(length(time.hours),n,3))


for (z in 1:3){
    
    resourceDF=resourceSysInfoRumen #reset dataframe

    for (i in 1:n){

        resourceDF['startValue',polymer.names[z]]=z.in[i]
        print(paste(polymer.names[z],'=',z.in[i],'g/l'))

        out=microPopModel(
            microbeNames=microbeNames,
            times=time.hours,
            rateFuncs=myRateFuncs,
            resourceSysInfo=resourceDF,
            microbeSysInfo=microbeSysInfoRumen,
            odeOptions=list(rtol=1e-10,atol=1e-10),
            checkingOptions=list(balanceTol=1e-2,
                                 reBalanceStoichiom=FALSE,checkMassConv=FALSE,
                                 checkStoichiomBalance=FALSE),
            plotOptions=list(yLabel='concentration (g/l)',xLabel='time (h)',
                             plotFig=FALSE,saveFig=FALSE,figType='eps',figName='Rumen')
        )
        methane[,i,z]=out$solution[,'Sch4']
        X[,i,z]=out$solution[,'Xh2']
        H2[,i,z]=out$solution[,'Sh2']
        SIC[,i,z]=out$solution[,'SIC']
        nh3[,i,z]=out$solution[,'Snh3']
        Xa[,i,z]=out$solution[,'Xaa']
        Sa[,i,z]=out$solution[,'Saa']
    }

}

dev.new()
par(mfrow=c(3,3))
par(mar=c(4,4,2,1))
vars=list(methane,X,nh3)
v.names=list('methane (Sch4)','H-utilizers (Xh2)','ammonia (Snh3)')
N=length(vars)
for (z in 1:3){
    for (i in 1:N){
        mat=vars[[i]]
        
        if (z<3 & i==N){maxy=0.3}else{ maxy=max(mat,na.rm=TRUE)}
        
        plot(range(time.hours),c(min(mat,na.rm=TRUE),maxy),type='n',
             xlab='Time (hours)',ylab=paste(v.names[i],'(g/l)'),cex.lab=1.2,
             cex.axis=1.2,lwd=2,main=paste('Varying',polymer.names[z]),cex.main=1.2)
        for (j in 1:n){
            lines(time.hours,mat[,j,z],col=j,lwd=2)
        }
        if (i==2){legend('topright',paste(z.in),lty=1,lwd=2,col=1:n,
                         title='initial concentration (g/l)',bty='n',cex=1.0)}
    }
}
#dev.copy2eps(file=paste('Rumen.eps',sep=''))

