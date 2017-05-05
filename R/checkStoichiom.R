#' Checks whether the stoichiometries in each MFG conserve mass
#' within a specified tolerance 
#' If they do not then if reBalanceStoichiom=TRUE
#' the stoichiometry will be adjusted
#' @param stoichiom Array. stoichiom[gname,R,path]
#' @param Rtype Resource type
#' @param microbeNames Vector of strings which contains the names of the microbial groups in the system e.g. c('Bacteroides','Acetogens')
#' @param numPaths Named vector. Number of paths for each microbial group
#' @param stoiTol Scalar. tolerance i.e. if abs(prod-up)>stoiTol then prints warning
#' @param reBalanceStoichiom Logical to turn off or on rebalancing
#' @return new stoichiom matrix

checkStoichiom=function(stoichiom,Rtype,microbeNames,numPaths,stoiTol,reBalanceStoichiom=FALSE){

    for (gname in microbeNames){
        
        for (p in 1:numPaths[gname]){
            path.name=paste('path',p,sep='')

            prod=sum(stoichiom[gname,Rtype[gname,,path.name]=='P',path.name])+sum(stoichiom[gname,Rtype[gname,,path.name]=='Pb',path.name])
            
            if (any(Rtype[gname,,path.name]=='S')){ #substitutable resources
                upS=mean(stoichiom[gname,Rtype[gname,,path.name]=='S',path.name],na.rm=TRUE)
            }else{
                upS=0}
                
            up=upS+sum(stoichiom[gname,Rtype[gname,,path.name]=='Se',path.name])+sum(stoichiom[gname,Rtype[gname,,path.name]=='Sb',path.name])+sum(stoichiom[gname,Rtype[gname,,path.name]=='Sw',path.name])
            
            if (abs(prod-up)>stoiTol){

                if (reBalanceStoichiom){
                    print(paste('MICROPOP WARNING: Stoichiometry did not balance for',gname,'on path',p,'. It has been rebalanced. Uptake was',up,', production was',prod))
                    frac=sum(up)/sum(prod)
                    stoichiom[gname,Rtype[gname,,path.name]=='P',path.name]=frac*stoichiom[gname,Rtype[gname,,path.name]=='P',path.name]
                    stoichiom[gname,Rtype[gname,,path.name]=='Pb',path.name]=frac*stoichiom[gname,Rtype[gname,,path.name]=='Pb',path.name]
                }else{
                    print(paste('MICROPOP WARNING: Stoichiometry does not balance for',gname,'on path',p,'. Uptake was',up,', production was',prod))
                }
            }
        }
    }
    return(stoichiom)
}
