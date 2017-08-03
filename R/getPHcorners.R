#' get pH corners Function
#' 
#' Returns the values of the pH values of the limit function i.e. where the limit is c(0,1,1,0)
#' Reads these in from the microbe group dataframes
#' 
#' @param microbeNames (vector of strings). Names of microbes in the system
#' @param pHLimit (logical) Is microbial growth affected by pH?
#'
#' @return (matrix) values of the pH values of the limit function i.e.
#' where the limit is c(0,1,1,0). Row names are microbeNames
#'
#' @export 
getPHcorners = function(microbeNames, pHLimit) {
    
    L = length(microbeNames)
    pHcorners = matrix(NA, ncol = 4, nrow = L, dimnames = list(microbeNames))
    if (pHLimit) {
        for (gname in microbeNames) {
            data = get(gname)
            if (any(rownames(data) == "pHcorners")) {
                
                if (any(colnames(data) == "units") | any(colnames(data) == "Units")) {
                  pHcorners[gname, ] = as.numeric(data["pHcorners", 2:5])
                } else {
                  pHcorners[gname, ] = as.numeric(data["pHcorners", 1:4])
                }
                if (is.na(sum(pHcorners[gname, ]))) {
                  stop(paste("MICROPOP ERROR: Missing pH corner value for", gname))
                }
                
            } else {
                
                print(paste("MICROPOP WARNING: pHcorners missing from", gname, "dataframe"))
                
                pHcorners[gname, ] = rep(NA, 4)
                
            }
        }
    }
    
    return(pHcorners)
}
