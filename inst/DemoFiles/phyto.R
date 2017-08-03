#Phytoplankton
#Simulating how phytoplankton bloom maxima occur where there is a balance between
#upwelling nutrients and light availability.
#The system starts with the phytoplankton and nutrient mixed evenly through the water column.

#CHOOSE CASE:
caseNum=readline('Choose case number by entering 1 or 2
(Case 1 is for just one microbial group, Case 2 is for 3 microbial groups)')

if (caseNum=='1'){
    print('Running phyto example with only one microbial group')
    microbeNames='Phyto1'
}else{
    print('Running phyto example with 3 microbial groups')
    microbeNames=c('Phyto1','Phyto2','Phyto3')
}

#create microbiota group dataframes
grpParNames=c("halfSat","yield","maxGrowthRate","Rtype","stoichiom","keyResource")
mat=matrix(c(1e-6,0.3,10,'Se',NA,'Nutrient'),nrow=6,ncol=1)
rownames(mat)=grpParNames; colnames(mat)=c('Nutrient')
Phyto1=data.frame(mat,stringsAsFactors=FALSE)

if (length(microbeNames)>1){
    Phyto2=Phyto1; Phyto2['halfSat','Nutrient']=1e-4
    Phyto3=Phyto1; Phyto3['halfSat','Nutrient']=1e-2
}

sim.times=round(seq(0,91,0.1),digits=2) #days
depths=round(seq(0,20,1),digits=1) #m

#-----------------------------------
kL=0.5 #light attenutation factor (/m)
vn=0.01 #rate of nutrient inflow with depth (g/l/m)
Kl=c(0.8,0.4,0.2); names(Kl)=c('Phyto1','Phyto2','Phyto3') #half sat for light

myRateFuncs=rateFuncsDefault

myRateFuncs$extraGrowthLimFunc=function(strainName,groupName,
                                        pathName,stateVarValues,stateVarNames,time,parms){
    light.level=exp(-kL*z)
    v=light.level/(light.level+Kl[groupName])
    return(min(max(v,0),1))    
}

myRateFuncs$entryRateFunc=function(varName,varValue,
                                   stateVarValues,time,inflowRate,parms){
    if (varName=='Nutrient'){
          v=vn*z
      }else{
          gname=getGroupName(varName,parms$microbeNames)
          if (gname%in%parms$microbeNames){
              v=inflowRate[gname]/parms$numStrains
          }else{
              v=inflowRate[varName]}
      }
    return(max(v,0))
}

nutri=matrix(NA,nrow=length(sim.times),ncol=length(depths))
bac1=matrix(NA,nrow=length(sim.times),ncol=length(depths))
bac2=matrix(NA,nrow=length(sim.times),ncol=length(depths))
bac3=matrix(NA,nrow=length(sim.times),ncol=length(depths))

z.ct=1#indexing
for (z in depths){
  print(paste('depth =',z,'m'))
  out=microPopModel(
    microbeNames=microbeNames,
    times=sim.times,
    resourceSysInfo=systemInfoResourcesPhyto,
    microbeSysInfo=systemInfoMicrobesPhyto,
    rateFuncs=myRateFuncs,
    checkingOptions=list(checkStoichiomBalance=FALSE),
    plotOptions=list(plotFig=FALSE),
    odeOptions=list('atol'=1e-8,'rtol'=1e-8,'method'='lsoda')
  )

  bac1[,z.ct]=out$solution[,'Phyto1'] 
  nutri[,z.ct]=out$solution[,'Nutrient'] 
  if (length(microbeNames)>1){
      bac2[,z.ct]=out$solution[,'Phyto2'] 
      bac3[,z.ct]=out$solution[,'Phyto3'] 
  }
  z.ct=z.ct+1
}

#plotting-------------------------------------------------------------------
mats=list(bac1,nutri)
titles=c('a) Phytoplankton (1 group)','Nutrient'); legendloc=c('topleft','top')
p.times=round(seq(0,max(sim.times),max(sim.times)/10),digits=0)
cols=rainbow(length(p.times))
ct=1
for (mat in mats){
    dev.new()
    par(mar=(c(5,5,2,1)))
    plot(c(0,max(mat)),range(depths),ylim=rev(range(depths)),type='n',
         xlab='Concentration (g/l)',ylab='Depth (m)',cex.lab=1.5,cex.axis=1.5,main=titles[ct])
    for (p in 1:length(p.times)){
        lines(mat[sim.times==p.times[p],],depths,col=cols[p],lwd=2)
    }
    legend(legendloc[ct],legend=p.times,col=cols,lty=1,lwd=2,title='days from start',bty='n')
    ct=ct+1
}

if (length(microbeNames)>1){
    L=length(sim.times)
    p.times=round(seq(0,max(sim.times),max(sim.times)/3),digits=0)
    mats=list(bac1,bac2,bac3)
    dev.new()
    par(mar=(c(5,5,2,1)))
    plot(c(0,3.5),range(depths),ylim=rev(range(depths)),type='n',
         xlab='Concentration (g/l)',ylab='Depth (m)',cex.lab=1.5,cex.axis=1.5,
         main='b) Phytoplankton (3 groups)')
    for (m in 1:length(mats)){
        for (p in 1:length(p.times)){
            lines(mats[[m]][sim.times==p.times[p],],depths,col=m,lwd=p,lty=7)
        }
    }
    legend('topright',legend=p.times,col=1,lty=1,lwd=1:length(p.times),
           title='days from start',bty='n')

    #find depth of maxima at end of simulation
    print(paste('depth of group 1 maximum is ',depths[bac1[L,]==max(bac1[L,])],'m'))
    print(paste('depth of group 2 maximum is ',depths[bac2[L,]==max(bac2[L,])],'m'))
    print(paste('depth of group 3 maximum is ',depths[bac3[L,]==max(bac3[L,])],'m'))
}
