#' List of functions that are used by the ODE solver
#' these functions can be changed by the user but all must be listed.
#' 
#' rateFuncsDefault=list(pHFunc=pHFuncDefault, pHLimFunc=pHLimFuncDefault, extraGrowthLimFunc=extraGrowthLimFuncDefault, growthLimFunc=growthLimFuncDefault, combineGrowthLimFunc=combineGrowthLimFuncDefault, uptakeFunc=uptakeFuncDefault, productionFunc=productionFuncDefault, combinePathsFunc=combinePathsFuncDefault, massBalanceFunc=massBalanceFuncDefault, entryRateFunc=entryRateFuncDefault, removalRateFunc=removalRateFuncDefault)

#' @aliases rateFuncs
#' @include pHFuncDefault.R
#' @include pHLimFuncDefault.R
#' @include extraGrowthLimFuncDefault.R
#' @include growthLimFuncDefault.R
#' @include combineGrowthLimFuncDefault.R
#' @include uptakeFuncDefault.R
#' @include productionFuncDefault.R
#' @include combinePathsFuncDefault.R
#' @include massBalanceFuncDefault.R
#' @include entryRateFuncDefault.R
#' @include removalRateFuncDefault.R
#'
#' @export
#' 
rateFuncsDefault<-list(

    pHFunc=pHFuncDefault,
    
    pHLimFunc=pHLimFuncDefault,

    extraGrowthLimFunc=extraGrowthLimFuncDefault,

    growthLimFunc=growthLimFuncDefault,
    
    combineGrowthLimFunc=combineGrowthLimFuncDefault,
    
    uptakeFunc=uptakeFuncDefault,
  
    productionFunc=productionFuncDefault,

    combinePathsFunc=combinePathsFuncDefault,
    
    massBalanceFunc=massBalanceFuncDefault,
    
    entryRateFunc=entryRateFuncDefault,
    
    removalRateFunc=removalRateFuncDefault

)



