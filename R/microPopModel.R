#' microPopModel
#'
#' Runs the microbial population model
#'
#' @param microbeNames Vector of strings which contains the names of the microbial groups in the system e.g. c('Bacteroides','Acetogens'). A dataframe for each of the same name must also exist in the workspace.
#' @param times Vector of times at which the solution is required, e.g. seq(0,10,0.1)
#' @param resourceSysInfo String giving the name of a csv file or a dataframe object, which describes the initial conditions, inflow and outflow (if constant) and molar mass of each resource. See help(resourceSysInfo) for more info.
#' @param microbeSysInfo String giving the name of a csv file (e.g. 'systemInfoMicrobes.csv') or a dataframe object, which describes the initial conditions, inflow and outflow (if constant) of each microbial group. See help(microbeSysInfo) for more info. 
#' @param rateFuncs A list of functions which are used to solve the ODEs in odeFunc. Default is rateFuncsDefault.R (provided in the package). See ?rateFuncs
#' @param odeFunc The function the ODE solver will use - the default is derivsDefault provided by the package but if the user wants to make significant changes a new ODE function file can be used. See ?derivsDefault
#' @param numStrains Integer stating the number of strains in each microbial group (same for all groups). Default is 1.
#' @param oneStrainRandomParams Logical to allow randomization of parameters even if there is only one strain. The default is FALSE which means that if numStrains=1 then the group params are used; if numStrains>1 then the parameters are automatically randomised according to info given in strainOptions. If oneStrainRandomParams=TRUE then even if there is only one strain its parameters will be randomised according to info given in strainOptions. 
#' @param pHLimit TRUE if pH limits microbial growth rates. Default is FALSE. If TRUE then rateFuncs$pHLimFunc is called.
#' @param pHVal Scalar. If the pH value is fixed it can be specified here and this is then used in the default rateFuncs$pHFunc function.
#' @param plotOptions List containing instructions for plotting: Default is list(plotFig=TRUE, sumOverStrains=FALSE, saveFig=FALSE, figType='eps', figName='microPopFig', yLabel='Concentration (g/L)', xLabel='Time').\cr
#' To turn off plot generation set plotFig=FALSE. If there are multiple strains these are all plotted if sumOverStrains=FALSE, otherwise they will be summed over each group. To save plot, saveFig=TRUE, figType (format) can be 'eps','png', 'pdf' or 'tiff' and is specified in figType (string), the name is figName (string) to which the string 'Microbes' or 'Resources' will be added for the respective plots.
#' @param odeOptions List containing instructions for the ODE solver ('deSolve'). Default: list('atol'=1e-6,'rtol'=1e-6,'method'='lsoda'). See ?ode for more details.
#' @param strainOptions List containing instructions for specifying strain parameters. Default: list(randomParams=c('halfSat', 'yield', 'maxGrowthRate', 'pHtrait'), seed=1, distribution='uniform', percentTraitRange=10, maxPHshift=0.2, applyTradeOffs=FALSE, tradeOffParams=NULL, paramsSpecified=FALSE, paramDataName=NULL). 
#' \itemize{
#' \item randomParams (vector) specifying which parameters need to be stochastically generated.
#' \item seed (number) seed for random number generator.
#' \item distribution (string) - either 'uniform' or 'normal' specifying the shape of the distribution from which to draw the random strain parameters. 
#'\item percentTraitRange (number) this is the percentage either side of the group parameter value which the strain parameter may range e.g. if percentTraitRange=10 then range is 0.9x to 1.1x for group mean x. 
#'\item maxPHshift (number) pH units to range over.
#' \item applyTradeOffs (logical) to trade off `good' and `bad' parameter values. 
#'\item tradeOffParams (vector of two strings) - parameters to trade off against each other. Note that pHtrait can not be traded off as whether this trait is good or bad depends on the environmental pH.
#'\item paramsSpecified (logical) TRUE if strain parameters are read in from a file (whose name is specified in paramDataName). The file must have colnames c(strainName, paramName, paramVal, paramUnit, resource,path) and where strainName is in format 'groupName.i' where i is the strain number.
#' }
#' @param checkingOptions (List) Default is list(checkMassConv=FALSE, balanceTol=1e-2, reBalanceStoichiom=FALSE, stoiTol=0.1, checkForNegs=TRUE, negTol=-1e-2). 
#' \itemize{
#' \item checkMassConv=TRUE checks for mass conservation in the ODE solver with a tolerance of 'balanceTol' (default is FALSE).
#' \item reBalanceStoichiom will check the mass balance of the stoichiometries on every metabolic path and rebalance if these are not conserving mass within a tolerance of stoiTol (a warning message will be issued). Rebalancing will only affect the final solution if the pathway contains only essential resources (Rtype 'Se') and microbial biomass is a product (Rtype 'Pb').  
#' \item checkForNegs If TRUE the function checkSolution is called and the solution for each variable, x, is checked for negative values that are greater in magnitude than negTol*max(x). If negative values occur then the solution is incorect and either the problem is incorrectly specified or the tolerances in the ODE solver need to be smaller.
#' }
#' @param microbeMolarMass Scalar. Mass of 1 mole of microbes - default is 113g/mol (Batstone et al., 2002)
#' @param bacCutOff Scalar. Amount of bacteria below which the bacteria are considered to have left the system and can't grow, default =1e-14. If this is set to zero then bacteria will always be able to grow again as zero is never reached.
#' @return The output is a list containing a matrix called 'solution' where rows are points in time and the columns are the state variables, and another list called parms which contains all the information needed to run the model. Within parms there are a number of other lists (e.g. Pmats for parameter values and Smats for system settings etc - try names(out$parms)).

#' @examples
#' #simplest example - define one microbial group (Archea) with 4 resources and
#' #simulate growth over 50 days
#' #make microbial group data frame:
#' MFG=matrix(NA,ncol=4,nrow=6,dimnames=list(c('Rtype','halfSat','yield',
#' 'maxGrowthRate','stoichiom','keyResource'),c('H2','CO2','CH4','H2O')))
#' MFG['Rtype',]=c('Se','Se','P','P')
#' MFG['halfSat',c('H2','CO2')]=1e-6
#' MFG['yield','H2']=0.2
#' MFG['maxGrowthRate','H2']=2
#' MFG['keyResource',1]='H2'
#' MFG['stoichiom',]=c(4,1,1,2)
#' Archea=data.frame(MFG,stringsAsFactors=FALSE)
#' 
#' #make resourceSysInfo data frame
#' Rmat=matrix(NA,ncol=4,nrow=4,dimnames=list(c('startValue','inflowRate',
#' 'washOut','molarMass'),c('H2','CO2','CH4','H2O')))
#' Rmat['startValue',]=c(1,1,0,0)
#' Rmat['inflowRate',]=c(1,5,0,0)
#' Rmat['washOut',]=c(0.1,0.1,0.1,0.1)
#' Rmat['molarMass',]=c(2,44,16,18)
#' 
#' #make microbeSysInfo data frame
#' Mmat=matrix(NA,ncol=1,nrow=3,dimnames=list(c('startValue','inflowRate',
#' 'washOut'),c('Archea')))
#' Mmat['startValue',]=1
#' Mmat['inflowRate',]=0
#' Mmat['washOut',]=0.1
#' 
#' out=microPopModel(
#'     microbeNames='Archea',
#'     times=seq(0,50,0.1),
#'     resourceSysInfo=data.frame(Rmat,stringsAsFactors=FALSE),
#'     microbeSysInfo=data.frame(Mmat,stringsAsFactors=FALSE)
#' )

#' @export

microPopModel = function(microbeNames, times, resourceSysInfo, microbeSysInfo, rateFuncs = rateFuncsDefault, 
    odeFunc = derivsDefault, numStrains = 1, oneStrainRandomParams = FALSE, pHLimit = FALSE, 
    pHVal = NA, plotOptions = list(), odeOptions = list(), strainOptions = list(), 
    checkingOptions = list(), microbeMolarMass = 113, bacCutOff = 1e-14) {
    
    
    # check Input Args: need to do some checks on the rateFuncs need a check so that
    # all substrates have halfsat, yield and max growth rate above zero
    #----------------
    # replace items in list specified by user
    plotOptions.default = list(yLabel = "Concentration (g/L)", xLabel = "Time", plotFig = TRUE, 
        sumOverStrains = FALSE, saveFig = FALSE, figType = "eps", figName = "microPopFig")
    
    odeOptions.default = list(atol = 1e-06, rtol = 1e-06, method = "lsoda")
    
    strainOptions.default = list(randomParams = c("halfSat", "yield", "maxGrowthRate", 
        "pHtrait"), seed = 1, distribution = "uniform", percentTraitRange = 10, maxPHshift = 0.2, 
        applyTradeOffs = FALSE, tradeOffParams = NULL, paramsSpecified = FALSE, paramDataName = NULL)
    
    checkingOptions.default = list(checkMassConv = FALSE, balanceTol = 0.01, reBalanceStoichiom = FALSE, 
        stoiTol = 0.1, checkForNegs = TRUE, negTol = -0.01, checkStoichiomBalance = TRUE)
    
    plotOptions = replaceListItems(plotOptions, plotOptions.default)
    odeOptions = replaceListItems(odeOptions, odeOptions.default)
    strainOptions = replaceListItems(strainOptions, strainOptions.default)
    checkingOptions = replaceListItems(checkingOptions, checkingOptions.default)
    
    if (length(microbeNames) > 1) {
        if (anyDuplicated(microbeNames)) {
            stop(paste("ERROR INFO: duplication of", microbeNames[duplicated(microbeNames)], 
                "in microbeNames"))
        }
    }
    
    set.seed(strainOptions$seed)
    
    # read in sys info data
    if (is.character(resourceSysInfo)) {
        sysInfoRes = createDF(resourceSysInfo)
    } else {
        sysInfoRes = resourceSysInfo
    }
    if (is.character(microbeSysInfo)) {
        sysInfoMicrobes = createDF(microbeSysInfo)
    } else {
        sysInfoMicrobes = microbeSysInfo
    }
    
    if (any(colnames(sysInfoRes) == "units") | any(colnames(sysInfoRes) == "Units")) {
        molarMass = c(as.numeric(sysInfoRes["molarMass", -1]), microbeMolarMass)
        names(molarMass) = c(colnames(sysInfoRes[, -1]), "X")
    } else {
        molarMass = c(as.numeric(sysInfoRes["molarMass", ]), microbeMolarMass)
        names(molarMass) = c(colnames(sysInfoRes), "X")
    }
    
    parameterNames.s = c("halfSat", "yield", "maxGrowthRate")  #microbial group params for each resource (use makeParamMatrix to put these values into a matrix with the same name)
    halfSat = 1
    yield = 1
    maxGrowthRate = 1
    
    for (p in strainOptions$randomParams) {
        if (!p %in% c(parameterNames.s, "pHtrait")) {
            stop("MICROPOP ERROR: Parameter names in strainOptions$randomParams must be one or more of halfSat, yield or maxGrowthRate")
        }
    }
    
    if (strainOptions$applyTradeOffs) {
        if (length(strainOptions$tradeOffParams) != 2) {
            stop("MICROPOP ERROR: If applyTradeOffs is TRUE, two (and only two) parameters must be specifed for trading off")
        }
        for (p in strainOptions$tradeOffParams) {
            if (!p %in% strainOptions$randomParams) {
                stop("MICROPOP ERROR: Trade-off parameters must be included in strainOptions$randomParams")
            }
            if (p == "pHtrait") {
                stop("MICROPOP ERROR: pH trait can not be traded-off")
            }
        }
    }
    #--------------
    
    # assign strain names
    allStrainNames = NULL
    for (gname in microbeNames) {
        if (numStrains == 1) {
            allStrainNames = c(allStrainNames, gname)
        } else {
            allStrainNames = c(allStrainNames, paste(gname, ".", seq(1, numStrains), 
                sep = ""))
        }
    }
    
    resourceNames = getAllResources(microbeNames)
    checkResInfo(resourceNames, sysInfoRes)  #check resources are in SysInfo
    numPaths = getNumPaths(microbeNames)
    keyRes = getKeyRes(microbeNames, numPaths)
    
    parameterNames.g = c("Rtype", "stoichiom")  #microbial group params for each resource
    Rtype = 1
    stoichiom = 1
    sysNames = c("startValue", "inflowRate", "washOut")
    startValue = 1
    inflowRate = 1
    washOut = 1
    
    # Force user to put NAs in the group dataframes where they are needed so that
    # random assignment of strain traits is correct.
    assignNAsToMFGs(microbeNames, numPaths, keyRes, resourceNames)
    
    # Make matrices of the parameters in parameterNames
    for (i in 1:length(parameterNames.s)) {
        assign(parameterNames.s[i], makeParamMatrixS(resourceNames, microbeNames, 
            parameterNames.s[i], numPaths, numStrains, strainOptions, oneStrainRandomParams))
    }
    
    for (i in 1:length(parameterNames.g)) {
        assign(parameterNames.g[i], makeParamMatrixG(microbeNames, parameterNames.g[i], 
            numPaths, sysInfoRes, microbeMolarMass, resourceNames))
    }
    
    if (numStrains > 1 & any(Rtype == "Sm", na.rm = TRUE)) {
        stop("MICROPOP ERROR: can not have multiple strains when there are microbial substrates - sorry!")
    }
    
    stateVarNames.g = c(microbeNames, resourceNames)
    stateVarNames.s = c(allStrainNames, resourceNames)
    for (i in 1:length(sysNames)) {
        assign(sysNames[i], getValues(sysInfoMicrobes, sysInfoRes, stateVarNames.g, 
            sysNames[i], allStrainNames, microbeNames, resourceNames, numStrains))
    }
    
    
    if (checkingOptions$checkStoichiomBalance) {
        stoichiom = checkStoichiom(stoichiom, Rtype, microbeNames, numPaths, as.numeric(checkingOptions$stoiTol), 
            checkingOptions$reBalanceStoichiom)
    }
    
    water.uptake.ratio = waterUptakeRatio(microbeNames, stoichiom, Rtype, numPaths)  #ratio of water mass/other uptake mass [gname,path]
    
    
    # molStoi=getMolarStoichiom(microbeNames,numPaths,resData,resourceNames)
    # molYield=getMolarYields(microbeNames,numPaths,resData,resourceNames,molStoi,keyRes,Rtype)
    # print(molYield) print(stoichiom)
    
    # put into lists
    Pmats = list(halfSat = halfSat, yield = yield, maxGrowthRate = maxGrowthRate, 
        Rtype = Rtype, stoichiom = stoichiom, waterRatio = water.uptake.ratio)  #,molYield=molYield)
    Smats = list(startValue = startValue, inflowRate = inflowRate, washOut = washOut)
    
    # once params halfSat, maxGrowthRate, yield have been assigned then can implement
    # trade offs.
    if (strainOptions$applyTradeOffs & numStrains > 1) {
        Pmats = applyTraitTradeOffs(microbeNames, strainOptions$tradeOffParams, numPaths, 
            numStrains, Pmats, resourceNames)
    }
    
    pHcorners = getPHcorners(microbeNames, pHLimit)
    
    strainPHcorners = getStrainPHcorners(microbeNames, allStrainNames, numStrains, 
        pHcorners, pHLimit, strainOptions)
    
    nonBoostFrac = getNonBoostFrac(microbeNames, resourceNames, numPaths)
    # print(nonBoostFrac)
    
    # now need to read in parameter values from file if required overwrite param vals
    # with those from file if required
    if (strainOptions$paramsSpecified) {
        nps = getStrainParamsFromFile(Pmats, strainPHcorners, strainOptions)
        Pmats = nps[[1]]
        strainPHcorners = nps[[2]]
    }
    
    
    # add in the parameter list that is passed into the ODE solver
    paramList = append(list(keyRes = keyRes, numPaths = numPaths, allStrainNames = allStrainNames, 
        resourceNames = resourceNames, microbeNames = microbeNames, numStrains = numStrains, 
        Pmats = Pmats, Smats = Smats, strainPHcorners = strainPHcorners, nonBoostFrac = nonBoostFrac, 
        pHLimit = pHLimit, pHVal = pHVal, molarMass = molarMass, bacCutOff = bacCutOff, 
        balanceTol = checkingOptions$balanceTol, checkMassConv = checkingOptions$checkMassConv), 
        rateFuncs)
    
    print("Set up completed, ODE solver called...")
    
    # run model - atm just using one ODE solver ptm=proc.time()
    out.ode = ode(y = startValue, times = times, func = odeFunc, parms = paramList, rtol = odeOptions$rtol, 
        atol = odeOptions$atol, method = odeOptions$method)
    # print(proc.time()-ptm)
    
    # checksolution does not contain zeros
    if (checkingOptions$checkForNegs) {
        checkSolution(out.ode, checkingOptions$negTol)
    }
    
    # plot figs
    if (plotOptions$plotFig) {
        quickPlot(out.ode, length(resourceNames), numStrains, microbeNames, plotOptions$yLabel, 
            plotOptions$xLabel, plotOptions$sumOverStrains, plotOptions$saveFig, 
            plotOptions$figType, plotOptions$figName)
    }
    
    return(list(solution = out.ode, parms = paramList))
}

