getStrainPHcorners=function(microbeNames,allStrainNames,numStrains,pHcorners,pHLimit,strainOptions){

    mat=matrix(NA,ncol=4,nrow=length(allStrainNames),dimnames=list(allStrainNames))

    if (pHLimit & 'pHtrait'%in%strainOptions$randomParams){
    #assign strain traits
        for (g in 1:length(microbeNames)){
            shifts=assignStrainTraits(numStrains,1,strainOptions,microbeNames[g],parName='pHtrait',pHtrait=TRUE)-1
            for (i in 1:numStrains){
                mat[((g-1)*numStrains+i),]=pHcorners[microbeNames[g],]+shifts[i]
            }
        }
    }

    return(mat)
}


