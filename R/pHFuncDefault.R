#' pH Function
#'
#' Return the value of pH in pH units
#'
#' @aliases pHFunc
#' @param time (scalar). The current time point in the ODE solver.
#' @param parms List which contains all information required by the ODE solver
#' @return (scalar) pH at the given time
#' @export
pHFuncDefault <- function(time,parms){
        pH=parms$pHVal
        return(pH)
}
