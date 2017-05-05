#' Uptake Function
#'
#' Return the value of resource uptake per biomass (i.e. resource quantity per unit time per mass unit of biomass)
#'
#' @aliases uptakeFunc
#' @param strainName Name of the strain that is being looped through in the ODE solver
#' @param groupName Name of microbial group that is being looped through in the ODE solver
#' @param pathName Name of metabolic path (e.g. path1) that is being looped through in the ODE solver
#' @param varName (string). Calculate uptake for this variable
#' @param keyResName (string). Name of the key resource on this pathway
#' @param subst Vector of strings giving the names of the substitutable resources  for given strain, pathway
#' @param ess Vector of strings giving the names of the essential resources  for given strain, pathway
#' @param boost Vector of strings giving the names of the boosting resources  for given strain, pathway
#' @param maxGrowthRate Vector containing maximum growth rate on each resource (named by resourceNames). If a resource is not on the pathway the value is NA
#' @param growthLim Vector containing the growth limitation from each resource (named by resourceNames). If a resource is not on the pathway the value is NA
#' @param yield Named vector (names are resourceNames) giving the mass yield of biomass on each resource (mass microbe/mass resource)
#' @param nonBoostFrac (scalar) Fraction of max growth achievable if boosting resource is not present but is required on this pathway
#' @param stoichiom Named vector (names are resourceNames) giving the mass of each resource in the stoichiometry i.e. molar mass of resource multiplied by the number of moles in the stoichiometry
#' @param parms List containing all system parameters
#'
#' @return (scalar) uptake of resource per mass unit of biomass (units are resource mass/biomass/time)
#' @export
uptakeFuncDefault=function(strainName,groupName,pathName,varName,keyResName,subst,ess,boost,maxGrowthRate,growthLim,yield,nonBoostFrac,stoichiom,parms){
        
    if (length(ess)>0){#multiply growth lims for essential resources
        ess.lim=prod(growthLim[ess],na.rm=T)
    }
    if (length(boost)>0){#multiply growth lims for essential resources
        boost.lim=prod(growthLim[boost],na.rm=T)
        nbf=nonBoostFrac[boost]
    }
    
    if (varName%in%subst){
        
        if (length(ess)==0 & length(boost)==0){
            v=maxGrowthRate[varName]*growthLim[varName]/yield[varName]
        }else if (length(ess)>0 & length(boost)==0){#include ess res
            v=ess.lim*maxGrowthRate[varName]*growthLim[varName]/yield[varName]
        }else if (length(ess)==0 & length(boost)>0){#include boost res
            v=(nbf+(1-nbf)*boost.lim)*maxGrowthRate[varName]*growthLim[varName]/yield[varName]
        }
        
    }else if (varName%in%ess){
        
        if (length(subst)==0){#all res are ess - use key resource
            if (varName==keyResName){
                v=maxGrowthRate[keyResName]*ess.lim/yield[keyResName]
            }else{
                v=(stoichiom[varName]/stoichiom[keyResName])*maxGrowthRate[keyResName]*ess.lim/yield[keyResName]}
        }else{# mix of subst and ess res - do not use key resource
            subst.uptake=ess.lim*sum(maxGrowthRate[subst]*growthLim[subst]/yield[subst])
            v=(stoichiom[varName]/mean(stoichiom[subst]))*subst.uptake
        }
        
    }else if (varName%in%boost){
        #compute uptake of substitutable resources due to booster - do not use key resource
        subst.uptake=(1-nbf)*boost.lim*sum(maxGrowthRate[subst]*growthLim[subst]/yield[subst])
        v=subst.uptake*stoichiom[boost]/mean(stoichiom[subst])
     }else{
        v=0
    }
    
    return(max(v,0))
}
