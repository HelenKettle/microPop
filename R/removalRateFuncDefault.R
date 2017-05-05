#' Removal Rate Function
#' 
#' Return the rate of removal of any state variable from the system
#'
#' @aliases removalRateFunc
#'
#' @param varName (string) Name of state variable of interest (this is group name or a resource name - NOT a strain name)
#' @param varValue (scalar) value of state variable of interest
#' @param stateVarValues (named vector) values of all state variables
#' @param time (scalar) time
#' @param washOut (named vector) of wash out rates (per unit time) of groups and resources (specified in SysInfo files)
#' @param parms List containing all system parameters
#' @return (scalar) rate of removal (quantity per unit time) for the state variable varName
#' @export
#' 
removalRateFuncDefault=function(varName,varValue,stateVarValues,time,washOut,parms){
    if (varValue<=0){
        v=0
    }else{
        v=washOut[varName]*varValue
    }
    return(v)
}
    
  
