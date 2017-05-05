#' Internal function to assign stochastic strain traits
#' 
#' Produces a random distribution of trait values where the mean is 1 and the range is determined by percentRange e.g. 10% gives values between 0.9 and 1.1
#' @param numStrains Scalar. Number of strains per group
#' @param groupVal Scalar. Group parameter value 
#' @param strainOptions List containing 'distribution' i.e. the shape of the distribution ('normal' or 'uniform' - if distribution is 'normal' then its std dev is percentRange/200, if distribution is 'uniform' then its range is groupVal +/- percentRange); ; 'maxPHshift' max shift in pH units
#' @param gname Name of microbial group 
#' @param parName Name of parameter. This is only used to help with error catching
#' @param pHtrait TRUE/FALSE whether or not trait is the pH trait. 
#' @importFrom stats rnorm runif
#' @return vector of trait values for each strain for one parameter value

assignStrainTraits=function(numStrains,groupVal,strainOptions,gname,parName='unspecified param',pHtrait=FALSE){

    
    if (pHtrait){
        percentRange=100*strainOptions$maxPHshift
    }else{
        percentRange=strainOptions$percentTraitRange
    }
    
    
    if (!is.na(groupVal)){
        
              #choose strain values randomly given the mean value and a std dev or a range
            #all parameter values must be zero or above
        if (strainOptions$distribution!='normal' & strainOptions$distribution!='uniform'){stop(paste('MICROPOP ERROR: please specify the type of distribution (uniform or normal) from which to choose strain traits from for',parName))}
        
        if (strainOptions$distribution=='normal'){
                #draw parameter values from a distribution where 2*sigma=percentRange/100
            traits=rnorm(numStrains,mean=1,sd=percentRange/200)
            vals=groupVal*traits
                                        #vals=rnorm(numStrains,mean=groupVal,sd=percentRange/200)
            if (any(vals<0)){stop(paste('MICROPOP ERROR: The values of the mean and std deviation for choosing strain trait values for',parName,'will result in values that are less than zero and therefore not valid'))}
        }

        if (strainOptions$distribution=='uniform'){
            traits=runif(numStrains,min=(1-percentRange/100),max=(1+percentRange/100))
            vals=groupVal*traits
                #vals=runif(numStrains,min=groupVal*(1-percentRange/100),max=groupVal*(1+percentRange/100))
            if (any(vals<0)){stop(paste('MICROPOP ERROR: The values of the mean and percent Range for choosing strain trait values for',parName,'will result in values that are less than zero and therefore not valid'))}
        }
            
        if (numStrains>1 & (is.na(percentRange) | percentRange==1)){
            print(paste('MICROPOP WARNING: There are',numStrains,'but they will all have the same value for',parName,'unless percentRange is specified (and is above zero)'))
        }
            


    }else{
        vals=NA*seq(1,numStrains)
    }


    
    return(vals)
}
