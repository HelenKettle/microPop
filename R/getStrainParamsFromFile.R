#' get strain parameter values from a csv file
#' 
#' @param Pmats List of parameter matrices
#' @param strainPHcorners Matrix of pH corners for each strain
#' @param strainOptions List which is input to microPopModel
#' @return (list) - first entry is new version of Pmats, second is new version of strainPHcorners
#' @export
#' 
getStrainParamsFromFile = function(Pmats, strainPHcorners, strainOptions) {
    
    nPmats = Pmats
    nCorners = strainPHcorners
    
    if (is.character(strainOptions$paramDataName)) {
        sdata = read.csv(strainOptions$paramDataName, header = TRUE, stringsAsFactors = FALSE)
    } else {
        sdata = strainOptions$paramDataName
    }
    
    for (line in 1:length(sdata[, 1])) {
        strainName = trimws(sdata[line, "strainName"], which = "both")
        path = as.numeric(trimws(sdata[line, "path"], which = "both"))
        resName = trimws(sdata[line, "resource"], which = "both")
        parVal = as.numeric(trimws(sdata[line, "paramVal"], which = "both"))
        parName = trimws(sdata[line, "paramName"], which = "both")
        if (parName == "pHcorners") {
            if (strainName%in%rownames(nCorners)){
                nCorners[strainName, ] = as.numeric(sdata[line, 3:6])
            }else{
                warning(paste('MICROPOP WARNING:',strainName,'is not in strainPHcorners'))
            }
        }else{
            if (!parName%in%names(Pmats)){
                warning(paste('MICROPOP WARNING:',parName,'is not a valid parameter name')) 
            }else if (!strainName%in%names(Pmats[[parName]])){
                warning(paste('MICROPOP WARNING:',strainName,'is not a valid strain name')) 
            }else if (!resName%in%colnames(Pmats[[parName]][[strainName]])){
                warning(paste('MICROPOP WARNING:',resName,'is not a valid resource name'))
            }else{
                nPmats[[parName]][[strainName]][path, resName] = parVal
            }
        }
    }

    return(list(nPmats, nCorners))
}

