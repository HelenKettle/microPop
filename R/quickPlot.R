#' Generic plotting showing results of microPop
#' @param out Output from microPopModel()
#' @param numR Scalar. Number of resources
#' @param numStrains Scalar. Number of strains per group
#' @param microbeNames Vector of strings which contains the names of the microbial groups in the system e.g. c('Bacteroides','Acetogens')
#' @param yLabel String for y axis label
#' @param xLabel String for x axis label
#' @param sumOverStrains Logical
#' @param saveFig Logical
#' @param figType String
#' @param figName String
#' @return Nothing just generates a plot
#' @importFrom grDevices dev.copy2eps dev.copy2pdf dev.new rainbow tiff dev.print png
#' @importFrom graphics legend lines par plot

quickPlot=function(out,numR,numStrains,microbeNames,yLabel,xLabel,sumOverStrains,saveFig=FALSE,figType='eps',figName='microPopFig'){

  wlen=7;hlen=7 #width and height for png files
    
  numMFG=length(microbeNames)
  numM=numStrains*numMFG
  time=out[,1]

  if (numM==1){ #one strain and one group (need to keep as a matrix)
    X=as.matrix(out[,2:(numM+1)])
    colnames(X)=colnames(out)[2]
  }else{
    X=out[,2:(numM+1)]}
  
  R=out[,(numM+2):(numM+numR+1),drop=FALSE]
  
  if (numStrains>1 & sumOverStrains){
    gmat=matrix(NA,nrow=length(time),ncol=numMFG)
    for (g in 1:numMFG){
      st=(g-1)*numStrains+1
      fin=g*numStrains
      gmat[,g]=rowSums(X[,st:fin])
    }
    Xmax=max(gmat,na.rm=TRUE)
  }else{
    Xmax=max(X,na.rm=TRUE)
  }
  
  if (figType=='png'){
      dev.new(bg="white",horizontal=FALSE,onefile = FALSE, paper = 'special',width=wlen,height=hlen)
  }else{
      dev.new()}
  par(mar=c(5,5,5,2))
  cols=rainbow(numMFG) #different groups have different colours
  plot(range(time),c(min(0,min(X,na.rm=TRUE)),1.1*Xmax),xlab=xLabel,main='Microbes',ylab=yLabel,cex.lab=1.5,cex.axis=1.3,cex.main=1.5,type='n')
  for (g in 1:numMFG){
    if (numStrains>1 & sumOverStrains){
      lines(time,gmat[,g],lwd=2,col=cols[g])
    }else{
      for (i in 1:numStrains){
        j=(g-1)*numStrains+i
        lines(time,X[,j],lwd=2,col=cols[g])
      }
    }
  }
  legend('topleft',bg='transparent',legend=microbeNames,col=cols,lty=1,lwd=2)
  if (saveFig){
      if (figType=='pdf'){dev.copy2pdf(file=paste(figName,'Microbes.pdf',sep=""))}
      if (figType=='eps'){dev.copy2eps(file=paste(figName,'Microbes.eps',sep=""))}
      if (figType=='png'){dev.print(png,filename=paste(figName,'Microbes.png',sep=""),res=100,width=wlen,height=hlen,units='in')}
      if (figType=='tiff'){dev.print(tiff,filename=paste(figName,'Microbes.tiff',sep=""),res=100,width=wlen,height=hlen,units='in')}
  }

  if (figType=='png'){
      wlen=7;hlen=7
      dev.new(bg="white",horizontal=FALSE,onefile = FALSE, paper = 'special',width=wlen,height=hlen)
  }else{
      dev.new()}
  par(mar=c(5,5,5,2))
  cols=rainbow(numR)
  plot(range(time),c(min(0,min(R,na.rm=TRUE)),1.1*max(R,na.rm=TRUE)),xlab=xLabel,main='Resources',ylab=yLabel,cex.lab=1.5,cex.axis=1.3,type='n',cex.main=1.5)
  for (i in 1:numR){
    lines(time,R[,i],lwd=2,col=cols[i])
  }
  legend('topleft',bg='transparent',colnames(R),col=cols,lty=1,lwd=2)
  if (saveFig){
      if (figType=='pdf'){dev.copy2pdf(file=paste(figName,'Resources.pdf',sep=""))}
      if (figType=='eps'){dev.copy2eps(file=paste(figName,'Resources.eps',sep=""))}
      if (figType=='png'){dev.print(png,filename=paste(figName,'Resources.png',sep=""),res=100,width=wlen,height=hlen,units='in')}
      if (figType=='tiff'){dev.print(tiff,filename=paste(figName,'Resources.tiff',sep=""),res=100,width=wlen,height=hlen,units='in')}
  }
  
}
  
 
  
