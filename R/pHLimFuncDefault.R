#' pH Limitation Function
#'
#' Return the value of pHLim (must lie in interval [0,1])
#'
#' @aliases pHLimFunc
#' @param strainName Name of the strain that is being looped through in the ODE solver
#' @param groupName Name of microbial group that is being looped through in the ODE solver
#' @param pH (scalar). The current pH value.
#' @param parms List of all parameters
#' @return (scalar) pH limitation (0 to 1)
#' @export

pHLimFuncDefault = function(strainName, groupName, pH, parms) {
    pHcorners = parms$strainPHcorners[strainName, ]

    #error check inputs:
    if (is.na(sum(pHcorners))){
        stop(paste("MICROPOP ERROR: There is an NA in pH corners for group",groupName))
    }
    if (!is.finite(pH)) {
        stop("MICROPOP ERROR: The value for pH is not defined")
    }

    if (!all(diff(pHcorners)>=0)){
        stop(paste("MICROPOP ERROR: pH corners for group",groupName,"must be in increasing order"))
    }


    #----------------------------------------------------------------------------
    grad1 = 1/(pHcorners[2] - pHcorners[1])
    grad2 = 1/(pHcorners[3] - pHcorners[4])
    if (pH < pHcorners[1]) {
        lim = 0
    }
    if (pH >= pHcorners[1] & pH < pHcorners[2]) {
        lim = grad1 * (pH - pHcorners[1])
    }
    if (pH >= pHcorners[2] & pH < pHcorners[3]) {
        lim = 1
    }
    if (pH >= pHcorners[3] & pH < pHcorners[4]) {
        lim = 1 + grad2 * (pH - pHcorners[3])
    }
    if (pH >= pHcorners[4]) {
        lim = 0
    }
    # print(lim)
    return(lim)
}
