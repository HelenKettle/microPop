#' obtains the none boosting fraction of growth for given MFG
#' if there is a boosting resource
#' @param microbeNames Vector of strings which contains the names of the microbial groups in the system e.g. c('Bacteroides','Acetogens')
#' @param resourceNames Vector of strings which contains the names of the resources in the system 
#' @param numPaths Named vector. Number of paths for each microbial group
#' @return an array with format [group,resource,path]
#' @export
#' 
getNonBoostFrac = function(microbeNames, resourceNames, numPaths) {
    
    nonBoostFrac = array(1, c(length(microbeNames), length(resourceNames), max(numPaths)), 
        dimnames = list(microbeNames, resourceNames, paste("path", seq(1, max(numPaths)), 
            sep = "")))
    
    for (gname in microbeNames) {
        
        data = get(gname)
        nres = colnames(data)
        res = nres[nres != "units" & nres != "Biomass" & nres != "biomass" & nres != 
            "Units"]
        
        for (path in 1:numPaths[gname]) {
            
            if (path == 1) {
                Rvar = "Rtype"
            } else {
                Rvar = paste("Rtype.", path, sep = "")
            }
            if (path == 1) {
                Bvar = "nonBoostFrac"
            } else {
                Bvar = paste("nonBoostFrac.", path, sep = "")
            }
            
            if (any(as.character(data[Rvar, ]) == "Sb", na.rm = TRUE)) {
                
                booster.name = nres[data[Rvar, ] == "Sb"]
                # print(booster.name)
                
                if (Bvar %in% rownames(data)) {
                  if ("units" %in% colnames(data)) {
                    nonBoostFrac[gname, booster.name, path] = as.numeric(data[Bvar, 
                      2])
                  } else {
                    nonBoostFrac[gname, booster.name, path] = as.numeric(data[Bvar, 
                      1])
                  }
                } else {
                  stop(paste("MICROPOP ERROR: There is a boosting resource specified for", 
                    gname, "on path", path, "(i.e. ", Rvar, "=Sb for", booster.name, 
                    ") but no nonBoostFrac specified"))
                }
            }
        }
    }
    # print(nonBoostFrac)
    return(nonBoostFrac)
}
