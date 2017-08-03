#' Combine microbial growth on different pathways by one microbe 
#' 
#' Returns a vector specifying the fraction of the total microbial growth on each pathway.
#' This function is needed to ensure that groups which have the most paths do not automatically have the most growth - i.e. need to weight the growth on each pathway.
#'
#' @aliases combinePathsFunc
#' @param strainName Name of the strain that is being looped through in the ODE solver
#' @param groupName Name of microbial group that is being looped through in the ODE solver
#' @param growthRate (vector) microbial growth rate (mass per unit time) on each pathway
#' @param num.paths (integer) is the number of paths for the given strain
#' @param pathNames Vector of names of all metabolic paths e.g. c('path1','path2')
#'
#' @return vector specifying the fraction of the total microbial growth on each pathway
#' @export
combinePathsFuncDefault = function(strainName, groupName, growthRate, num.paths, 
    pathNames) {
    
    if (sum(growthRate) > 0) {
        path.frac = growthRate/sum(growthRate)
    } else {
        path.frac = rep(1, num.paths)
    }
    names(path.frac) = pathNames
    return(path.frac)
}
