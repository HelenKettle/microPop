#' Makes vector of unique resource names
#' @param microbeNames Vector of strings which contains the names of the microbial groups in the system e.g. c('Bacteroides','Acetogens')
#' @return vector of resource names
#' 
getAllResources=function(microbeNames){

    #read in all the microbe files and get the full list of resources needed

    allResources=NULL
    ct=0
    for (gname in microbeNames){
      data=get(gname)
        nres=colnames(data)
        nres=nres[nres!='units' & nres!='Biomass' & nres!='biomass' & nres!='Units']
        allResources=append(allResources,nres,after=ct)
        ct=ct+length(colnames(data))
      }
    
    resNames=unique(allResources[!is.na(allResources)])

    return(resNames)
  }
