#' Find the pH value which is the centre of mass of the pH limitation function
#' (used for the pH trait)
#' @param strainName Name of the strain 
#' @param groupName Name of microbial group 
#' @param pHLimFunc function specified in rateFuncs$pHLimFunc
#' @param parms List of all parameters
#' @return pH value at centre of mass
#' @export

pHcentreOfMass=function(strainName,groupName,pHLimFunc,parms){
    pHcorners=parms$strainPHcorners[strainName,]
    pHvec=seq(pHcorners[1],pHcorners[4],0.01)
    lim=NA*pHvec
    for (p in 1:length(pHvec)){
        lim[p]=pHLimFunc(strainName,groupName,pHvec[p],parms)
    }
    
    return(mean(lim*pHvec)/mean(lim))
    
}
