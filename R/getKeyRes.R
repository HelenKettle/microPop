#' Finds the name of the key resource for each path for each MFG
#' @param microbeNames Vector of strings which contains the names of the microbial groups in the system e.g. c('Bacteroides','Acetogens')
#' @param numPaths Named vector. Number of paths for each microbial group
#' @return list of vectors where the names are microbeNames

getKeyRes=function(microbeNames,numPaths){

  key=list()
  
  for (gname in microbeNames){
    
    data=get(gname)
    
    vec=seq(1,numPaths[gname])*NA
    names(vec)=paste('path',seq(1,numPaths[gname]),sep='')
    
    for (p in 1:numPaths[gname]){
      
      if (p==1){
        kvar='keyResource'
        Rvar='Rtype'
      }else{
        kvar=paste('keyResource.',p,sep='')
        Rvar=paste('Rtype.',p,sep='')
      }

        if (any(colnames(data)=='units') | any(colnames(data)=='Units')){
            vec[p]=data[kvar,][[2]]
        }else{
            vec[p]=data[kvar,][[1]]}

        if (any(data[Rvar,]=='Se') | any(data[Rvar,]=='Sb') | any(data[Rvar,]=='Sm')){
#            if (!vec[p]%in%colnames(data)){stop(paste('MICROPOP ERROR: The key resource for',gname,'on path',p,'is not one of its resources'))}
        }else{
           if (!is.na(vec[p])){
                print(vec[p])
                print(paste('MICROPOP NOTE TO USER: The key resource for',gname,'on path',p,'is not used'))
            }
        }

    }#path loop
      
    key=append(key,list(vec))
  }
  
  names(key)=microbeNames
          
  return(key)
}
