#' Gets parameter values for parameters halfSat, yield and maxGrowthRate from the MFGs and puts into a matrix
#' also assigns strain traits
#' since traits are assigned to all strains at once for one param they are
#' stored in Mat[strain,res,path], this is then rearranged to make a matrix
#' for each strain (mat[path,res]).
#' 
#' @param resNames Vector of strings which contains the names of the resources in the system
#' @param microbeNames Vector of strings which contains the names of the microbial groups in the system e.g. c('Bacteroides','Acetogens')
#' @param parameterName Name of parameter
#' @param numPaths Named vector. Number of paths for each microbial group
#' @param numStrains Scalar. Number of strains per group
#' @param strainOptions List of strain options
#' @param oneStrainRandomParams Logical. TRUE for randomized params even if there is only one strain.
#' @return A list called parameterName which contains matrices for all strains (in all groups) with paths on rows and resources on columns


makeParamMatrixS=function(resNames,microbeNames,parameterName,numPaths,numStrains,strainOptions,oneStrainRandomParams){

  pList=list()
  nam=NULL

  #check for capitalization
  if (parameterName=='halfSat'){alt.parameterName='HalfSat'}
  if (parameterName=='yield'){alt.parameterName='Yield'}
  if (parameterName=='maxGrowthRate'){alt.parameterName='MaxGrowthRate'}
  
  for (gname in microbeNames){

    Lr=length(resNames)
    Lp=numPaths[gname]
    Ls=numStrains

    #assign strain names to str.names
    if (numStrains==1){str.names=gname}else{str.names=paste(gname,'.',seq(1,Ls),sep='')}
    
    path.names=paste('path',seq(1,Lp),sep='')

    Mat=array(NA,dim=c(Ls,Lr,Lp),dimnames=list(str.names,resNames,path.names))
 
    data=get(gname) #get dataframe for microbial group
      
    for (path in 1:Lp){
    
      if (path==1){
        var=parameterName
        alt.var=alt.parameterName
      }else{
        var=paste(parameterName,'.',path,sep='')
        alt.var=paste(alt.parameterName,'.',path,sep='')
      }
            

    if (!var%in%rownames(data) & !alt.var%in%rownames(data)){
      stop(paste('MICROPOP ERROR: parameter',var,'is not in',gname))
    }
    if (!var%in%rownames(data) & alt.var%in%rownames(data)){
      var=alt.var
    }
    

    #assign traits
    for (r in 1:Lr){

      rname=resNames[r]
      
      if (rname%in%colnames(data)){ #rname is a resource for group g
        
        pval=as.numeric(data[var,rname])
        
          if (numStrains>1 | oneStrainRandomParams){
              
              if (parameterName%in%strainOptions$randomParams){
                  traitVals=assignStrainTraits(numStrains,pval,strainOptions,gname,parameterName,pHtrait=FALSE)
              }else{
                  traitVals=pval*rep(1,numStrains)
              }

              Mat[,r,path]=traitVals
              
          }else{
              Mat[1,r,path]=pval
          }
            
      }else{ #rname is not a resource for group g

        Mat[,r,path]=NA
      }
      
    }#r

    }#path

    #make matrix for each strain
    for (s in 1:numStrains){
        mat=matrix(NA,nrow=Lp,ncol=Lr,dimnames=list(path.names,resNames))
        for (p in 1:Lp){
            mat[p,]=Mat[s,,p]
        }
        if (numStrains==1){mat.name=gname}else{mat.name=paste(gname,'.',s,sep='')}
        nam=append(nam,mat.name)
        pList=append(pList,list(mat))
    }

}
  
  names(pList)=nam
      
  return(pList)
    
}

