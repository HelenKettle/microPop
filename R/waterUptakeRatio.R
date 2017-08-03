#' Computes the mass ratio of water uptake to substrate uptake
#' (i.e. mass uptake of water divided by total mass uptake) 
#' 
#' @param microbeNames Vector of strings which contains the
#' names of the microbial groups in the system e.g. c('Bacteroides','Acetogens')
#' @param stoichiom Array: stoichiom[gname,resName,pathName]
#' @param Rtype Resource type array: Rtype[gname,resName,pathName]
#' @param numPaths Named vector. Number of paths for each microbial group
#' @return water mass ratio matrix [gname,pathName] with names.
#' @keywords internal
#' 
waterUptakeRatio = function(microbeNames, stoichiom, Rtype, numPaths) {
    
    Lp = max(numPaths)
    pathNames = paste("path", seq(1, Lp), sep = "")
    
    water.ratio = matrix(0, nrow = length(microbeNames), ncol = max(numPaths),
                         dimnames = list(microbeNames,  pathNames))
    
    for (gname in microbeNames) {
        
        for (p in 1:numPaths[gname]) {
            
            path.name = pathNames[p]
            
            if ("Sw" %in% Rtype[gname, , path.name]) { #i.e. water is a substrate 
                
                if (any(Rtype[gname, , path.name] == "S")) {
                  # substitutable resources
                  upS = mean(stoichiom[gname, Rtype[gname, , path.name] == "S",
                                       path.name], na.rm = TRUE)
                } else {
                  upS = 0
                }
                
                #compute total mass uptake
                up = upS +
                    sum(stoichiom[gname,Rtype[gname, ,path.name]=="Se", path.name]) +
                    sum(stoichiom[gname,Rtype[gname, ,path.name]=="Sb", path.name]) +
                    sum(stoichiom[gname,Rtype[gname, ,path.name]=="Sm", path.name])
                
                water.ratio[gname, path.name] = sum(stoichiom[gname,
                                   Rtype[gname, , path.name] == "Sw", path.name])/up
                
            }
            
        }
    }
    return(water.ratio)
}
