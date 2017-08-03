#' generic function which assigns values for Rtype or stoichiom
#' to an array of the same name
#' array has form [group,resource,path]
#' reads in data from MFG dataframes
#' @param microbeNames Vector of strings which contains the names of the microbial groups in the system e.g. c('Bacteroides','Acetogens')
#' @param parameterName Name of parameter  (Rtype and stoichiom)
#' @param numPaths Named vector. Number of paths for each microbial group
#' @param resData sys info dataframe for resources
#' @param microbeMolarMass Scalar. Molar mass of microbe e.g. 113g/mol
#' @param resNames Vector of strings which contains the names of the resources in the system
#' @return array of form: parameterName [gname,rname,pname]
#' @keywords internal


makeParamMatrixG = function(microbeNames, parameterName, numPaths, resData, microbeMolarMass, 
    resNames) {

    #get molarMasses
    if (any(colnames(resData) == "units") | any(colnames(resData) == "Units")) {
        molarMassData = resData["molarMass", -1]
        names(molarMassData) = colnames(resData[, -1])
    } else {
        molarMassData = resData["molarMass", ]
        names(molarMassData) = colnames(resData)
    }

    Lg = length(microbeNames)
    # read in all microbe files and get the full list of resources needed
    allResources = NULL
    ct = 0
    for (gname in microbeNames) {
        data = get(gname)
        cn = colnames(data)
        nres = cn[cn != "units" & cn != "Units"]
        allResources = append(allResources, nres[substring(nres, 1, 2) != "X."], 
            after = ct)
        if (any(data["Rtype", ] == "Pb", na.rm = TRUE)) {
            #add molar mass for microbes to molar mass vector if needed
            molarMassData = c(molarMassData, microbeMolarMass)
            names(molarMassData) = c(names(molarMassData[1:length(molarMassData) - 
                1]), "Biomass")
        }
        ct = ct + length(colnames(data))
    }
    
    resNames = unique(allResources[!is.na(allResources)])
    Lr = length(resNames)
    Lp = max(numPaths)
    pathNames = paste("path", seq(1, Lp), sep = "")

    Mat = array(NA, c(Lg, Lr, Lp), dimnames = list(microbeNames, resNames, pathNames))
    
    for (g in 1:Lg) {
        
        gname = microbeNames[g]
        data = get(gname)
        
        for (p in 1:numPaths[microbeNames[g]]) {
            
            if (p == 1) {
                var = parameterName
            } else {
                var = paste(parameterName, ".", p, sep = "")
            }
            
            if (!var %in% rownames(data)){
                    stop(paste("MICROPOP ERROR: parameter", var, "is not in", gname,
                               "data frame (note code is case sensitive)"))
            }
             
            for (r in 1:Lr) {
                rname = resNames[r]
                if (rname %in% colnames(data)) {
                  # rname is a resource for group g
                  
                  if (var == "Rtype" | var == paste("Rtype.", p, sep = "")) {
                    
                    Mat[g, r, p] = data[var, rname]
                  } else {
                    Mat[g, r, p] = as.numeric(data[var, rname])
                  }
                  
                  if (var == "stoichiom" | var == paste("stoichiom.", p, sep = "")) {
                    Mat[g, r, p] = as.numeric(molarMassData[[rname]]) * Mat[g, r, p]
                  }
                  
                } else {
                  # rname is not a resource for group g
                  
                  if (var == "Rtype" | var == paste("Rtype.", p, sep = "")) {
                    Mat[g, r, p] = "X"
                  } else {
                    Mat[g, r, p] = NA
                  }
                }
            }  #r
        }  #p
    }  #g
    
    
    
    return(Mat)
    
}



