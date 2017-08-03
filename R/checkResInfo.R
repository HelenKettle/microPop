#' Checks whether the all the resources needed are included
#' in the system information file (e.g. start value, washout rate etc)
#' @param resNames Vector of strings which contains the names of the resources in the system
#' @param sys.data data frame sysInfoRes i.e. resource sys info data frame
#' @return nothing
#'
#' @keywords export
#' 
checkResInfo = function(resNames, sys.data) {
    
    missing = rep(0, length(resNames))
    names(missing) = resNames
    for (rname in resNames) {
        if (!rname %in% names(sys.data)) {
            missing[rname] = 1
        }
    }
    if (any(missing == 1)) {
        stop(paste("MICROPOP ERROR: entries for", paste(resNames[missing == 1], collapse = " and "), 
            "are missing from resourceSysInfo"))
    }
    
}
