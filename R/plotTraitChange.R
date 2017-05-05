#' plot changes in trait over time
#' @param out Output from microPopModel()
#' @param trait.name can be 'halfSat','yield','maxGrowthRate' and 'pHtrait' or 'strainpHcorners'
#' @param group.names can be a vector of group names or just one string for one name
#' @param resource.name String
#' @param path String
#' @param xlabel String
#' @param saveFig Logical
#' @param figType String
#' @param figName String
#' @importFrom graphics legend lines par plot
#' @importFrom grDevices dev.copy2eps dev.copy2pdf dev.new rainbow tiff dev.print png
#' @export
#' 
plotTraitChange=function(out,trait.name,group.names,resource.name=NULL,path=NULL,xlabel='Time (days)',saveFig=FALSE,figType='eps',figName='Traits'){

    wlen=7;hlen=7 #width and height for png files

  Lg=length(group.names)
  times=out$solution[,1]
  avTrait=matrix(NA,nrow=length(times),ncol=Lg)
  
  numStrains=out$parms$numStrains
  ct=1
  for (gname in group.names){
    
    strain.names=paste(gname,'.',seq(1,numStrains),sep='')
    weighted.vals=NA*out$solution[,strain.names]

    for (strain in strain.names){
      if (trait.name=='strainPHcorners' | trait.name=='pHtrait'){
        trait.val=pHcentreOfMass(strain,gname,out$parms$pHLimFunc,out$parms)
      }else{
        trait.val=out$parms$Pmats[[trait.name]][[strain]][path,resource.name]
      }
      mass=out$solution[,strain]
      weighted.vals[,strain]=trait.val*mass
    }

  #at each point in time find the sum(xi*Mi)/sum(Mi) over all strains i
    avTrait[,ct]=rowMeans(weighted.vals)/rowMeans(out$solution[,strain.names])
    ct=ct+1
  }

  if (trait.name=='strainPHcorners' | trait.name=='pHtrait'){
    ylabel='Biomass-weighted mean value of pH trait'
  }else{
    ylabel=paste('Biomass-weighted mean value of',trait.name,'parameter for',group.names,'on',resource.name)
  }

  cols=rainbow(Lg)
  dev.new()
  par(mar=c(5,5,5,2))
  plot(range(times),c(range(avTrait)[1],1.1*range(avTrait)[2]-0.1*range(avTrait)[1]),type='n',xlab=xlabel,ylab=ylabel,cex.lab=1.3,cex.main=1.5,cex.axis=1.3,main='Traits')
  for (g in 1:Lg){
    lines(times,avTrait[,g],col=cols[g],lwd=2)
  }
  if (Lg>1){legend('topright',group.names,col=cols,lty=1,lwd=2)}

  if (saveFig){
      if (figType=='pdf'){dev.copy2pdf(file=paste(figName,'.pdf',sep=""))}
      if (figType=='eps'){dev.copy2eps(file=paste(figName,'.eps',sep=""))}
      if (figType=='png'){dev.print(png,filename=paste(figName,'.png',sep=""),res=100,width=wlen,height=hlen,units='in')}
      if (figType=='tiff'){dev.print(tiff,filename=paste(figName,'.tiff',sep=""),res=100,width=wlen,height=hlen,units='in')}
  }

  
  return(avTrait)
}



