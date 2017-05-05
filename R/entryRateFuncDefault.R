#' entry Rate Function
#' 
#' Return the rate of entry to the system for any state variable
#'
#' @aliases entryRateFunc
#'
#' @param varName (string) Name of state variable of interest (resource name or strain name)
#' @param varValue (scalar) value of state variable of interest
#' @param stateVarValues (named vector) values of all state variables
#' @param time (scalar) time
#' @param inflowRate (named vector) on inflow rates (specified in SysInfo files)
#' @param parms List containing all system parameters
#' @return (scalar) rate of entry (quantity per unit time) for any state variable
#'
#' @export

entryRateFuncDefault=function(varName,varValue,stateVarValues,time,inflowRate,parms){

    gname=getGroupName(varName,parms$microbeNames)
    if (gname%in%parms$microbeNames){
        v=inflowRate[gname]/parms$numStrains
    }else{
        v=inflowRate[varName]
    }
    return(v)
}

