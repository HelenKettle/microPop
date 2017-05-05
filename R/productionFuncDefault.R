#' Production Function
#' 
#' Production rate of resource (units are resource mass/time)
#'
#' @aliases productionFunc
#' @param strainName Name of the strain that is being looped through in the ODE solver
#' @param groupName Name of microbial group that is being looped through in the ODE solver
#' @param pathName Name of metabolic path (e.g. path1) that is being looped through in the ODE solver
#' @param varName (string). Calculate production for this variable
#' @param all.substrates Vector of strings giving the names of the all the substrates used on this pathway
#' @param keyResName (string). Name of the key resource on this pathway
#' @param stoichiom Named vector (names are resourceNames) giving the mass of each resource in the stoichiometry i.e. molar mass of resource multiplied by the number of moles in the stoichiometry
#' @param products Vector of strings giving the names of the all the metabolic products created on this pathway
#' @param bio.products Vector of strings giving the names of the all the microbial products created on this pathway
#' @param uptake Vector with names given by resourceNames which given mass uptake of each resource per unit time
#' @param growthRate (scalar) microbial growth rate (mass per unit time) on the given pathway
#' @param yield Named vector (names are resourceNames) giving the mass yield of biomass on each resource (mass microbe/mass resource)
#' @param parms List containing all system parameters
#' @param water Name of resource with Rtype 'Sw' - i.e resource could be called 'water' or 'H2O' etc
#'
#' @return (scalar) production rate of resource (units are resource mass/time)
#' @export 
 productionFuncDefault=function(strainName,groupName,pathName,varName,all.substrates,keyResName,stoichiom,products,bio.products,uptake,growthRate,yield,parms,water){

    if (growthRate>0){
        
        if (length(bio.products)>0){#biomass is in stoichiom so don't remove
            uptakeKeyRes=growthRate/yield[keyResName]
            v=stoichiom[varName]/stoichiom[keyResName]*uptakeKeyRes
            
        }else{
            #sum all uptakes and remove microbial growth to find mass available for products
            if (length(water)>0){waterUp=uptake[water]}else{waterUp=0}#uptake rate of water
            v=(stoichiom[varName]/sum(stoichiom[products]))*(sum(uptake[all.substrates])+waterUp-growthRate)
        }
        
    }else{
        v=0}
    
    return(max(v,0))
}

