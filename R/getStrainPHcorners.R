#' get stochastically generated pH corners for each strain
#' 
#' Returns the values of the pH values of the limit function i.e. where the limit is c(0,1,1,0)
#' Reads these in from the microbe group dataframes
#' 
#' @param microbeNames (vector of strings). Names of microbes in the system
#' @param allStrainNames (vector of strings)
#' @param numStrains Integer
#' @param pHcorners vector of 4 scalars definining the pH lim func
#' @param pHLimit (logical) Is microbial growth affected by pH?
#' @param strainOptions list from microPopModel inputs
#'
#' @return (matrix) values of the pH values of the limit function i.e.
#' where the limit is c(0,1,1,0) for each strain
#' @export

getStrainPHcorners = function(microbeNames, allStrainNames, numStrains, pHcorners, 
    pHLimit, strainOptions) {

    mat = matrix(NA, ncol = 4, nrow = length(allStrainNames),
        dimnames = list(allStrainNames))

    if (pHLimit & "pHtrait" %in% strainOptions$randomParams) {
        # assign strain traits
        for (g in 1:length(microbeNames)) {
            shifts = assignStrainTraits(numStrains, 1, strainOptions,
                parName = "pHtrait", 
                pHtrait = TRUE) - 1
            for (i in 1:numStrains) {
                mat[((g - 1) * numStrains + i), ] = pHcorners[microbeNames[g], ] + 
                  shifts[i]
            }
        }
    }
    
    return(mat)
}


