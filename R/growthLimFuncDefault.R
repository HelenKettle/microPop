#' growth rate limitation function
#' 
#' Returns the value of growthLim (must lie in interval [0,1] i.e. unitless) of strainName on varName which is used to scale the maximum growth rate
#' Contains two options - one for essential resources and one for substitutable resources (based on Ballyk and Wolkowicz, 1993)
#'
#' @aliases growthLimFunc

#' @param strainName Name of the strain that is being looped through in the ODE solver
#' @param groupName Name of microbial group that is being looped through in the ODE solver
#' @param pathName Name of metabolic path (e.g. path1) that is being looped through in the ODE solver
#' @param varName (string) Name of variable (resource) of interest 
#' @param resourceValues State vector of resources  (with names)
#' @param allSubType Vector of strings (with names corresponding to the resourceNames) which describes the type of each resource ('Rtype') - Rtypes are S (substitutable resource), Se (essential resource), Sb (booster resource), Sm (microbial resource), P (product) and Pb (biomass product)
#' @param strainHalfSat Vector (with names corresponding to the resourceNames) of half-saturation constants for the given strain. If resource is not a substrate for the given strain, the value is NA 
#' @param stateVarValues State vector (resources and microbes) (with names)
#' @return scalar giving limitation on growth rate - must be >=0 and <=1
#' @export
#' 
growthLimFuncDefault = function(strainName, groupName, pathName, varName, resourceValues, 
    allSubType, strainHalfSat, stateVarValues) {
    
    if (resourceValues[varName] <= 0) {
        v = 0
    } else {
        
        if (allSubType[varName] == "Sb" | allSubType[varName] == "Se") {
            v = resourceValues[varName]/(resourceValues[varName] + strainHalfSat[varName])
        }
        
        if (allSubType[varName] == "S" | allSubType[varName] == "Sm") {
            # monod eq adjusted for growth on multiple substitutable resources
            v = resourceValues[varName]/(strainHalfSat[varName] * (1 + sum(resourceValues[allSubType == 
                "S" | allSubType == "Sm"]/strainHalfSat[allSubType == "S" | allSubType == 
                "Sm"])))
        }
    }
    return(max(v, 0))
}
