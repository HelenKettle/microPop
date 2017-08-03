# Read in the molar stoichiom values and put in array
# array has form [group,resource,path]
# reads in data from MFG dataframes
# @param microbeNames Vector of strings which contains the names of the microbial groups in the system e.g. c('Bacteroides','Acetogens')
# @param numPaths Named vector. Number of paths for each microbial group
# @param resNames Vector of resource names
# @return Array [gname,rname,pathname]
# @keywords internal
# 
getMolarStoichiom = function(microbeNames, numPaths, resNames) {
    
    parameterName = "stoichiom"
    alt.parameterName = "Stoichiom"
    
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
                alt.var = alt.parameterName
            } else {
                var = paste(parameterName, ".", p, sep = "")
                alt.var = paste(alt.parameterName, ".", p, sep = "")
            }
            
            if (!var %in% rownames(data)) {
                # print(data)
                stop(paste("MICROPOP ERROR: parameter", var, "is not in", gname))
            }
            
            for (r in 1:Lr) {
                rname = resNames[r]
                if (rname %in% colnames(data)) {
                  # rname is a resource for group g
                  
                  Mat[g, r, p] = as.numeric(data[var, rname])
                  
                } else {
                  # rname is not a resource for group g
                  
                  Mat[g, r, p] = NA
                  
                }
            }  #r
        }  #p
    }  #g
    
    return(Mat)
    
}



