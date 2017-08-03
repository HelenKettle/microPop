#' Differential Equations called by ODE solver
#' @param t time
#' @param y vector of state variables
#' @param parms list of parameters
#' @export

derivsDefault = function(t, y, parms) {
    
    halfSat = parms$Pmats$halfSat
    yield = parms$Pmats$yield
    maxGrowthRate = parms$Pmats$maxGrowthRate
    Rtype = parms$Pmats$Rtype
    stoichiom = parms$Pmats$stoichiom
    washOut = parms$Smats$washOut
    
    numR = length(parms$resourceNames)
    
    # y is named and contains all state variables
    X = y[parms$allStrainNames]
    R = y[parms$resourceNames, drop = FALSE]
    
    dX = NA * X
    names(dX) = parms$allStrainNames
    dX.growth = NA * X
    names(dX.growth) = parms$allStrainNames
    dR = NA * R
    names(dR) = parms$resourceNames
    
    dR.g = array(0, dim = c(length(parms$microbeNames), numR), dimnames = list(parms$microbeNames, 
        parms$resourceNames))
    
    for (gname in parms$microbeNames) {
        # group loop
        
        # print(gname)
        
        if (parms$numStrains == 1) {
            strainNames = gname
        } else {
            strainNames = paste(gname, ".", seq(1, parms$numStrains), sep = "")
        }
        
        pathNames = paste("path", seq(1, parms$numPaths[gname]), sep = "")
        
        dR.s = matrix(0, ncol = numR, nrow = parms$numStrains, dimnames = list(strainNames, 
            parms$resourceNames))
        
        for (str.name in strainNames) {
            # strain loop
            
            # print(str.name)
            if (X[str.name] > parms$bacCutOff) {
                
                # storage
                growthRate.p = NA * seq(1, parms$numPaths[gname])  #total growth rate (over all substrates) of strain s on each path 
                names(growthRate.p) = pathNames
                growthLim = matrix(NA, ncol = numR, nrow = parms$numPaths[gname], 
                  dimnames = list(pathNames, parms$resourceNames))
                uptake.p = matrix(0, ncol = numR, nrow = parms$numPaths[gname], dimnames = list(pathNames, 
                  parms$resourceNames))
                production.p = matrix(0, ncol = numR, nrow = parms$numPaths[gname], 
                  dimnames = list(pathNames, parms$resourceNames))
                
                # find pH limitation on growth for given strain (not path or resource dependent)
                if (parms$pHLimit) {
                  pHlim = parms$pHLimFunc(str.name, gname, parms$pHFunc(t, parms), 
                    parms)
                  # print(pHlim)
                } else {
                  pHlim = 1
                }
                
                
                for (path in pathNames) {
                  # path loop
                  
                  # compute extra growth lim function (default value is 1 ie no limitation)
                  extraGrowthLim = parms$extraGrowthLimFunc(str.name, gname, path, 
                    c(X, R), c(parms$allStrainNames, parms$resourceNames), t, parms)
                  
                  # print(path)
                  resnames = colnames(Rtype[gname, , path, drop = FALSE])
                  
                  subst = resnames[Rtype[gname, , path] == "S" | Rtype[gname, , path] == 
                    "Sm"]  #substitutable resources
                  ess = resnames[Rtype[gname, , path] == "Se"]  #essential resources
                  boost = resnames[Rtype[gname, , path] == "Sb"]  #booster resources
                  bio.sub = resnames[Rtype[gname, , path] == "Sm"]  #substrate is a microbe
                  products = resnames[Rtype[gname, , path] == "P"]  #products
                  bio.products = resnames[Rtype[gname, , path] == "Pb"]  #biomass product
                  water = resnames[Rtype[gname, , path] == "Sw"]  #biomass product
                  
                  all.substrates = c(subst, ess, boost)
                  
                  # modify max growth rate by pH limitation and extraGrowthLim
                  maxGrowthRate.g = pHlim * extraGrowthLim * maxGrowthRate[[str.name]]
                  
                  # need to do this to retain row and col names
                  rtype.sub = subsetFunc(Rtype, gname, path)
                  halfsat.sub = subsetFunc(halfSat[[str.name]], gname, path)
                  yield.sub = subsetFunc(yield[[str.name]], gname, path)
                  
                  # specific growth limitation for each resource for this strain on this path
                  for (rname in all.substrates) {
                    growthLim[path, rname] = parms$growthLimFunc(str.name, gname, 
                      path, rname, R, rtype.sub, halfsat.sub, y)
                    if (is.na(growthLim[path, rname])) {
                      stop(paste("MICROPOP ERROR: growthLimFunc is returning NA for ", 
                        str.name, " growing on ", rname, ". (FYI halfSat value is ", 
                        halfSat[[str.name]][path, rname], ")", sep = ""))
                    }
                  }
                  
                  # need to do this to retain row and col names
                  growthLim.sub = subsetFunc(growthLim, gname, path)
                  maxGrowthRate.g.sub = subsetFunc(maxGrowthRate.g, gname, path)
                  stoichiom.sub = subsetFunc(stoichiom, gname, path)
                  
                  # combine the growth lims to get bac growth on this path
                  growthRate.p[path] = X[str.name] * parms$combineGrowthLimFunc(str.name, 
                    gname, path, subst, ess, boost, bio.sub, maxGrowthRate.g.sub, 
                    growthLim.sub, parms$keyRes[[gname]][path], parms$nonBoostFrac[gname, 
                      , path])
                  
                  # compute uptake of each resource on this path--------------------
                  for (rname in c(all.substrates)) {
                    uptake.p[path, rname] = X[str.name] * parms$uptakeFunc(str.name, 
                      gname, path, rname, parms$keyRes[[gname]][path], subst, ess, 
                      boost, maxGrowthRate.g.sub, growthLim.sub, yield.sub, parms$nonBoostFrac[gname, 
                        , path], stoichiom.sub, parms)
                  }
                  
                  # compute uptake of water if it is a resource on this path
                  if (any(rtype.sub == "Sw")) {
                    uptake.p[path, rtype.sub == "Sw"] = parms$Pmats$waterRatio[gname, 
                      path] * sum(uptake.p[path, ], na.rm = TRUE)
                  }
                  
                  # compute production on this path (metabolites only - not
                  # biomass)-----------------------
                  for (rname in products) {
                    production.p[path, rname] = parms$productionFunc(str.name, gname, 
                      path, rname, all.substrates, parms$keyRes[[gname]][path], stoichiom.sub, 
                      products, bio.products, uptake.p[path, ], growthRate.p[path], 
                      yield.sub, parms, water)
                  }
                  
                }  #path loop
                
                
                # scale growth, production and uptake according to path frac
                path.frac = parms$combinePathsFunc(str.name, gname, growthRate.p, 
                  parms$numPaths[gname], pathNames)
                growthRate.p = path.frac * growthRate.p
                uptake.p = path.frac * uptake.p
                production.p = path.frac * production.p
                
                # check mass balance for changes due to biological growth
                if (parms$checkMassConv) {
                  parms$massBalanceFunc(uptake.p, production.p, growthRate.p, parms$balanceTol, 
                    str.name)
                }
                
                for (rname in parms$resourceNames) {
                  dR.s[str.name, rname] = sum(production.p[, rname] - uptake.p[, 
                    rname], na.rm = TRUE)
                }
                
                
                dX.growth[str.name] = sum(growthRate.p)
                
            } else {
                dR.s[str.name, ] = 0
                dX.growth[str.name] = 0
            }
            
        }  #strain loop
        
        dR.g[gname, ] = colSums(dR.s)  #production-uptake from each group for path p
        
    }  #group loop
    
    
    for (rname in parms$resourceNames) {
        
        # add in changes to resource due to microbial growth
        if (rname %in% parms$microbeNames) {
            dR.m = dX.growth[rname]  #TODO need to sort out for strain names if more than 1 strain
        } else {
            dR.m = 0
        }
        
        resRate.in = parms$entryRateFunc(rname, R[rname], y, t, parms$Smats$inflowRate, 
            parms)
        if (is.na(resRate.in)) {
            stop(paste("MICROPOP ERROR: entry rate of", rname, "is not defined at time", 
                t))
        }
        
        resRate.out = parms$removalRateFunc(rname, R[rname], y, t, washOut, parms)
        if (is.na(resRate.out)) {
            stop(paste("MICROPOP ERROR: removal rate of", rname, "is not defined at time", 
                t))
        }
        
        dR[rname] = sum(dR.g[, rname], na.rm = TRUE) + resRate.in - resRate.out + 
            dR.m
    }
    
    # washout etc
    for (str.name in parms$allStrainNames) {
        
        # add changes to X due to uptake of it as a resource
        if (str.name %in% parms$resourceNames) {
            dX.res = sum(dR.g[, str.name], na.rm = TRUE)
        } else {
            dX.res = 0
        }
        
        XRate.in = parms$entryRateFunc(str.name, X[str.name], y, t, parms$Smats$inflowRate, 
            parms)
        
        if (is.na(XRate.in)) {
            stop(paste("MICROPOP ERROR: entry rate of", str.name, "is not defined at time", 
                t))
        }
        XRate.out = parms$removalRateFunc(getGroupName(str.name, parms$microbeNames), 
            X[str.name], y, t, washOut, parms)
        if (is.na(XRate.out)) {
            stop(paste("MICROPOP ERROR: removal rate of", str.name, "is not defined at time", 
                t))
        }
        
        
        dX[str.name] = XRate.in + dX.res + dX.growth[str.name] - XRate.out
        
    }
    
    list(c(dX, dR))
    
}

