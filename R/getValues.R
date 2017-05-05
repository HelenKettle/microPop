#' get quantity for all variables in 'Names'
#' e.g. get 'startValue' for all resources
#' @param sysInfoMicrobes sys info dataframe for microbes
#' @param sysInfoRes sys info dataframe for resources
#' @param Names Vector of names of microbial group or resource 
#' @param quantity String. Name of quantity to get value for
#' @param strainNames Vector of strings of strain names
#' @param microbeNames Vector of strings which contains the names of the microbial groups in the system e.g. c('Bacteroides','Acetogens')
#' @param resourceNames Vector of strings which contains the names of the resources in the system 
#' @param numStrains Scalar. Number of strains per group
#' 
getValues=function(sysInfoMicrobes,sysInfoRes,Names,quantity,strainNames,microbeNames,resourceNames,numStrains){


    L=length(Names)
    x=0*seq(1,L)
    names(x)=Names

    for (i in 1:L){
        
        if (Names[i]%in%microbeNames){
            data=sysInfoMicrobes
        }else if (Names[i]%in%resourceNames){
            data=sysInfoRes
        }

        if (!(Names[i]%in%colnames(data))){
            stop(paste('MICROPOP ERROR:', Names[i],'is missing from systemInfoMicrobes file'))
        }
        
        if (is.na(data[quantity,Names[i]]) & quantity!='inflowRate'){
            stop(paste('MICROPOP ERROR: Missing data for',quantity,'in systemInfoMicrobes file'))
        }else{
            x[i]=as.numeric(data[quantity,Names[i]])
        }
        
      }

    if (quantity=='startValue'){
      #need to make start values for each strain (not just group)
      strainICs=seq(1,length(microbeNames)*numStrains)
      for (i in 1:length(microbeNames)){
        strainICs[((i-1)*numStrains+1):(numStrains*i)]=x[i]/numStrains
      }
      resx=x[-seq(1,length(microbeNames))]
      x=c(strainICs,resx)
      names(x)=c(strainNames,resourceNames)
    }

    return(x)
}
