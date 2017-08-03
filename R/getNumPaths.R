#' get the number of metabolic pathways for the given group
#' @param microbeNames Vector of strings which contains the names of the microbial groups in the system e.g. c('Bacteroides','Acetogens')
#' @return a named vector of the number of paths for each group
#' if numPathways is not in dataframe then it is set to 1.
#' @export
#' 
getNumPaths = function(microbeNames) {
    
    L = length(microbeNames)
    numPaths = NA * seq(1, L)
    names(numPaths) = microbeNames
    for (gname in microbeNames) {
        data = get(gname)
        if (any(colnames(data) == "units") | any(colnames(data) == "Units")) {
            n = as.numeric(data["numPathways", ][[2]])
        } else {
            n = as.numeric(data["numPathways", ][[1]])
        }
        if (!is.na(n)) {
            numPaths[gname] = n
        } else {
            numPaths[gname] = 1
        }
    }
    return(numPaths)
}
