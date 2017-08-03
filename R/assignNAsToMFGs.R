#' Assigns NA to the MFG data frames for parameter values that are not used
#' Even though these values are not used it is important that they are NAs when assigning random strain values
#' It alters the global variables that are the MFG dataframes
#' @param microbeNames Vector of strings which contains the names of the microbial groups in the system e.g. c('Bacteroides','Acetogens')
#' @param numPaths Named vector. Number of paths for each microbial group
#' @param keyRes Name of the key resource 
#' @param resourceNames Vector of strings which contains the names of the resources in the system 
#' @return nothing
#' @keywords internal
#' 
assignNAsToMFGs = function(microbeNames, numPaths, keyRes, resourceNames) {
    
    causeErrors = TRUE
    paramNames = c("yield", "maxGrowthRate", "halfSat")
    
    for (gname in microbeNames) {
        
        data = get(gname)
        
        for (rname in resourceNames) {
            if (rname %in% colnames(data)) {
                for (path in 1:numPaths[gname]) {
                  
                  pname = paste("path", path, sep = "")
                  if (path == 1) {
                    Rvar = "Rtype"
                  } else {
                    Rvar = paste("Rtype.", path, sep = "")
                  }
                  
                  Rtype = data[Rvar, rname]
                  if (is.na(Rtype)) {
                    stop(paste("MICROPOP ERROR:", Rvar, "is not specified for", gname, 
                      "on path", path))
                  }
                  if (!"S" %in% data[Rvar, ]) 
                    {
                      # if there are no substitutable resources on the path: essential and biomass
                      # resources
                      if (Rtype == "Se" | Rtype == "Sb") 
                        {
                          if (rname != keyRes[[gname]][pname]) {
                            if (path == 1) {
                              for (param in paramNames[1:2]) {
                                if (!is.na(data[param, rname])) {
                                  data[param, rname] = NA
                                  if (causeErrors) {
                                    stop(paste("MICROPOP ERROR: the", param, "value for", 
                                      gname, "on", rname, "on", pname, "should be NA as it will not be used as", 
                                      rname, "is not the key resource"))
                                  }
                                }
                              }
                            } else {
                              # path>1
                              for (param in paramNames[1:2]) {
                                nparam = paste(param, ".", path, sep = "")
                                if (!is.na(data[nparam, rname])) {
                                  data[nparam, rname] = NA
                                  if (causeErrors) {
                                    stop(paste("MICROPOP ERROR: the", param, "value for", 
                                      gname, "on", rname, "on", pname, "should be NA as it will not be used as", 
                                      rname, "is not the key resource"))
                                  }
                                }
                              }
                            }
                          }
                        }  # if Rtype=='Se' | Rtype=='Sb'
                    }  #subst res if
                  
                  if (Rtype == "X" | Rtype == "P") {
                    # unused resources and products
                    if (path == 1) {
                      for (param in paramNames) {
                        if (!is.na(data[param, rname])) {
                          data[param, rname] = NA
                          if (causeErrors) {
                            stop(paste("MICROPOP ERROR: the", param, "value for", 
                              gname, "on", rname, "on", pname, "should be NA as it will not be used"))
                          }
                        }
                      }
                    } else {
                      for (param in paramNames) {
                        if (!is.na(data[paste(param, ".", path, sep = ""), rname])) {
                          print(!is.na(data[paste(param, ".", path, sep = ""), rname]))
                          print(is.na(data[paste(param, ".", path, sep = ""), rname]))
                          print(data[paste(param, ".", path, sep = ""), rname])
                          nparam = paste(param, ".", path, sep = "")
                          data[nparam, rname] = NA
                          
                          
                          if (causeErrors) {
                            stop(paste("MICROPOP ERROR: the", param, "value for", 
                              gname, "on", rname, "on", pname, "should be NA as it will not be used."))
                          }
                        }
                      }
                    }
                  }
                }
            }
        }
        # assign(gname,data,envir=.GlobalEnv)
    }
    
}
