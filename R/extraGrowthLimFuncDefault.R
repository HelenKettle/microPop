#' Extra Growth Limitation Function
#'
#' Return the value of extraGrowthLim (number between 0 and 1)
#'
#' @aliases extraGrowthLimFunc
#' @param strainName Name of strain
#' @param groupName Name of group
#' @param pathName metabolic path name e.g. 'path1'
#' @param stateVarValues values of all state variables at the current time step
#' @param stateVarNames names of all state variables
#' @param time time,t, in ODE solver
#' @param parms list of all parameters
#' @return (scalar) limitation on growth (between 0 and 1)
#' @export
extraGrowthLimFuncDefault=function(strainName,groupName,pathName,stateVarValues,stateVarNames,time,parms){
    #the output from this function will multiply maxGrowthRate
    #it can therefore be used to inhibit growth in any way you like!
    #the default is 1 (no limitation).
    lim=1
    return(min(max(lim,0),1))    #IMPORTANT! - output must be between 0 and 1
}
