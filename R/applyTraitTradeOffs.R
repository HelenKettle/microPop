#' Internal function to trade off one trait against another (used when
#' assigning randomly generated strain traits)
#'
#' works by finding the values for each strain for par1 and par2 and
#' then sorting them in opposite orders.  This means the parameter
#' values don't change number but they are assigned to different
#' strains.
#' 
#' @param microbeNames Vector of strings which contains the names of
#'     the microbial groups in the system e.g. c('Bacteroides','Acetogens')
#' @param tradeOffParams (vector of two strings) - parameters to trade
#'     off against each other
#' @param numPaths Named vector. Number of paths for each microbial
#'     group
#' @param numStrains Scalar. Number of strains per group
#' @param Pmats List containing lists and matrices:
#'     [[param]][[strainName]][path,rname]
#' @param resourceNames Vector of strings which contains the names of
#'     the resources in the system
#' @return new version of Pmats where parameter values are traded off
#' 
#' @export
applyTraitTradeOffs = function(microbeNames, tradeOffParams, numPaths, numStrains, 
    Pmats, resourceNames) {
    
    # sort par 1 in the direction of bad to good and par2 in the direction of good to
    # bad
    par1 = tradeOffParams[1]
    par2 = tradeOffParams[2]
    
    # determine which direction to sort in
    decreasing.logical = logical(length = 2)
    ct = 1
    for (par in tradeOffParams) {
        if (par == "halfSat") {
            # smaller half sat means less limitation on growth so direction=decreasing
            decreasing.logical[ct] = TRUE
        } else if (par == "yield" | par == "maxGrowthRate") {
            # larger yield means more efficient growth on resources large maxGrowthRate means
            # faster growth so direction=increasing
            decreasing.logical[ct] = FALSE
        }
        ct = ct + 1
    }
    decreasing.logical[2] = !decreasing.logical[2]
    
    nPmats = Pmats
    
    for (gname in microbeNames) {
        for (path in 1:numPaths[gname]) {
            for (rname in resourceNames) {
                mat = matrix(NA, ncol = numStrains, nrow = 2)
                for (i in 1:numStrains) {
                  strainName = paste(gname, ".", i, sep = "")
                  mat[1, i] = Pmats[[par1]][[strainName]][path, rname]
                  mat[2, i] = Pmats[[par2]][[strainName]][path, rname]
                }
                if (sum(is.finite(mat[1, ])) == numStrains & sum(is.finite(mat[2, 
                  ])) == numStrains) {
                  nmat = rbind(sort(mat[1, ], decreasing = decreasing.logical[1]), 
                    sort(mat[2, ], decreasing = decreasing.logical[2]))
                  for (i in 1:numStrains) {
                    strainName = paste(gname, ".", i, sep = "")
                    nPmats[[par1]][[strainName]][path, rname] = nmat[1, i]
                    nPmats[[par2]][[strainName]][path, rname] = nmat[2, i]
                  }
                }
            }
        }
    }
    
    return(nPmats)
}
