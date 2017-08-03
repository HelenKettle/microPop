#' get system quantity (e.g. startValue, inflowRate, washOut) for all state variables
#' (convention is that microbes are before resources) 
#' @param sysInfoMicrobes sys info dataframe for microbes
#' @param sysInfoRes sys info dataframe for resources
#' @param stateVarNames Vector of names of all the state variables
#' @param quantity String. Name of quantity to get value for e.g. 'startValue'
#' @param strainNames Vector of strings of strain names
#' @param microbeNames Vector of strings which contains the names of the microbial groups in the system e.g. c('Bacteroides','Acetogens')
#' @param resourceNames Vector of strings which contains the names of the resources in the system 
#' @param numStrains Integer. Number of strains per group
#' @export
#' 
getValues = function(sysInfoMicrobes, sysInfoRes, stateVarNames, quantity,
                     strainNames, microbeNames, resourceNames, numStrains) {
    
    L = length(stateVarNames)
    x = 0 * seq(1, L)
    names(x) = stateVarNames
    
    for (i in 1:L) {
        
        if (stateVarNames[i] %in% microbeNames) {
            data = sysInfoMicrobes
        } else if (stateVarNames[i] %in% resourceNames) {
            data = sysInfoRes
        }
        
        if (!(stateVarNames[i] %in% colnames(data))) {
            stop(paste("MICROPOP ERROR:", stateVarNames[i], "is missing from systemInfoMicrobes file"))
        }
        
        if (is.na(data[quantity, stateVarNames[i]]) & quantity != "inflowRate") {
            stop(paste("MICROPOP ERROR: Missing data for", quantity, "in systemInfoMicrobes file"))
        } else {
            x[i] = as.numeric(data[quantity, stateVarNames[i]])
        }
        
    } #i loop
    
    if (quantity == "startValue") {
        # need to make start values for each strain (not just group)
        strainICs = seq(1, length(microbeNames) * numStrains)
        for (i in 1:length(microbeNames)) {
            strainICs[((i - 1) * numStrains + 1):(numStrains * i)] = x[i]/numStrains
        }
        resx = x[-seq(1, length(microbeNames))]
        x = c(strainICs, resx)
        names(x) = c(strainNames, resourceNames)
    }
    
    return(x)
}
