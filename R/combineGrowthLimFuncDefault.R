#' combines the growth limitation functions and max growth rates to get the growth rate of strain
#' 
#' Returns the specific growth rate in units of inverse time 
#'
#' @aliases combineGrowthLimFunc

#' @param strainName Name of the strain that is being looped through in the ODE solver
#' @param groupName Name of microbial group that is being looped through in the ODE solver
#' @param pathName Name of metabolic path (e.g. path1) that is being looped through in the ODE solver
#' @param subst Vector of strings giving the names of the substitutable resources  for given strain, pathway
#' @param ess Vector of strings giving the names of the essential resources  for given strain, pathway
#' @param boost Vector of strings giving the names of the boosting resources  for given strain, pathway
#' @param bio.sub Vector of strings giving the names of the microbial resources  for given strain, pathway
#' @param maxGrowthRate Vector containing maximum growth rate on each resource (named by resourceNames). If a resource is not on the pathway the value is NA
#' @param growthLim Vector containing the growth limitation from each resource (named by resourceNames). If a resource is not on the pathway the value is NA
#' @param keyResName String giving the name of the key resource on this pathway
#' @param nonBoostFrac (scalar) Fraction of max growth achievable if boosting resource is not present but is required on this pathway

#' @return (scalar) specific growth rate in units of inverse time 
#'
#' @export
combineGrowthLimFuncDefault=function(strainName,groupName,pathName,subst,ess,boost,bio.sub,maxGrowthRate,growthLim,keyResName,nonBoostFrac){
 
    if (length(subst)>0){#sum growth limitations for substitutable resources
        subst.growth=sum(maxGrowthRate[subst]*growthLim[subst])
    }
    
    if (length(ess)>0){#multiply growth lims for essential resources
        ess.lim=prod(growthLim[ess],na.rm=T)
    }
    
    if (length(boost)>0){#multiply growth lims for essential resources
        boost.lim=prod(growthLim[boost],na.rm=T)
    }
    
    if (length(ess)>0 & length(subst)==0 & length(boost)==0){#all res are essential res
        v=maxGrowthRate[keyResName]*ess.lim
    }
    
    if (length(subst)>0 & length(ess)==0 & length(boost)==0){ #all res are subst res
        v=subst.growth
    }
    
    if (length(subst)>0 & length(ess)>0 & length(boost)==0){#res are a mix of subst and ess
        v=subst.growth*ess.lim
    }
    
    if (length(subst)>0 & length(boost)>0 & length(ess)==0){#res are a mix of subst and boost
        v=subst.growth*(nonBoostFrac[boost]+(1-nonBoostFrac[boost])*boost.lim)
    }

    if (length(subst)==0 & length(boost)>0 & length(ess)>0){#res are a mix of ess and boost
        v=maxGrowthRate[keyResName]*ess.lim*(nonBoostFrac[boost]+(1-nonBoostFrac[boost])*boost.lim)
    }
    
    
    if (!exists('v')){stop('MICROPOP ERROR: An unaccounted for combination of resource types in combineGrowthLimFunc')}
    
    return(v)
    
}
