#' Microbial Functional Group (MFG) dataframes
#'
#' This is a generic description of the dataframes describing the pathways and parameters of each microbial functional group.
#' Each resource (substrate, metabolic product or biomass (if microbial production is included in the chemical stoichiometry)) has a column. The first column can be used for describing the units of each parameter. This is optional and just for clarity - it is not used within microPop (note, the units column must be labelled 'units' and it can not contain NAs). 
#' The row names and their details are given below:
#' \itemize{
#' \item Rtype Describes the type of resource. Can be S (substitutable substrate), Se (essential substrate), Sb (boosting substrate), Sm (microbial substrate), Sw (water as a substrate), P (product), Pb (biomass product) or X (not used)
#' \item halfSat Half-saturation constant for Monod Equation growth. Units must match the units of the resources. Resources that aren't used for growth will have entry NA.
#' \item yield This is the biomass yield i.e. mass of microbes/mass of substrate consumed. Note this is NOT a mol/mol yield! Resources that aren't used for growth will have entry NA.
#' \item maxGrowthRate Maximum growth rate of the group. Units are per unit time where time has the same units as those used for the microPopModel input arguments 'times'. Resources that aren't used for growth must have entry NA.
#' \item stoichiom The chemical stoichiometry in moles of each resource (note that this may also include biomass (see Xsu)).
#' \item keyResource If the stoichiometry is specified and all resources are essential then stoichiom will be used to determine rates of production and uptake and now 'yield' is the biomass produced per gram of the key resource specified here.
#' \item pHcorners Specified using 4 values in the first 4 columns. The pH limitation on growth is described by a trapezium. For increasing pH values the limitation goes from 0,1,1,0 at the points specified by the pHcorners.
#' \item numPathways The number of metabolic pathways the group has. If this is greater than 1 see details below for naming conventions.
#' }
#'
#' If there is more than one pathway the row names are as above but followed by .2 for second pathway, .3 for third pathway and so on. E.g. halfSat.2, yield.2
#'
#' Note, when constructing new dataframes for new microbial functional groups (MFGs), the order of the rows does not matter but the names of the rows must be the same as those above.
#' Also, the order of the resources columns does not matter (although if there is a 'units' column it must be the first column).
#' The resources may be different for each MFG (e.g. See Bacteroides and Xsu).
#'
#' When the user tells microPop which groups to use via the microbeNames input argument, the package will determine the names of all the resources and MFGs in the system and then check they are also in the system information files.
#'
#'
#'Note that the optional units column can not contain NAs. For entries without units put 'none'.
#'
#' @docType data
#' @keywords datasets data
#' @name MFG
#' @usage MFG
#' @format A dataframe with the row names in the itemised list below and a column for units (optional) and for each resource required by the microbial group. 
NULL
#'
#' resourceSysInfo
#' 
#' Data frame describing the system information for the state variables that are resources (i.e. substrates or metabolic products). 
#'
#' Each resource (substrate, metabolic product or biomass if microbes are a resource e.g. in the case of viruses) has a column. The first column can be used for describing the units of each variable. This is optional and just for clarity - it is not used within microPop (note, the units column must be labelled 'units').
#' The data frame must contain the following rows:
#' \itemize{
#' \item startValue The value of each resource at the start time of the simulation (e.g. units are g/l)
#' \item inflowRate The value of the rate of inflow of each resource (e.g. units are g/l/d)
#' \item washOut The specific washout rate of each resource (e.g. units are /d)
#' \item molarMass The mass in grams of one mole of the resource (units are g/mol) 
#' }
#' 
#' @docType data
#' @keywords datasets data
#' @name resourceSysInfo
#' @usage resourceSysInfo
#' @format A dataframe with the row names in the itemised list below and a column for units (optional) and for each resource in the system to be simulated.
NULL
#' microbeSysInfo
#' 
#' Data frame describing the system information for the microbial state variables
#'
#' Each MFG has a column. The first column can be used for describing the units of each variable. This is optional and just for clarity - it is not used within microPop (note, the units column must be labelled 'units').
#' The data frame must contain the following rows:
#' \itemize{
#' \item startValue The value of each MFG at the start time of the simulation (e.g. units are g/l)
#' \item inflowRate The value of the rate of inflow of each MFG (e.g. units are g/l/d)
#' \item washOut The specific washout rate of each MFG (e.g. units are /d)
#' }
#' 
#' @docType data
#' @keywords datasets data
#' @name microbeSysInfo
#' @usage microbeSysInfo
#' @format A dataframe with the row names in the itemised list below and a column for units (optional) and for each microbial functional group (MFG) in the system to be simulated.
NULL
#' Bacteroides dataframe
#'
#' Table of information describing the behaviour of the microbial functional group.
#' See help(MFG) or ?MFG for explanation of the contents of the microbial functional groups dataframes
#'
#' @docType data
#' @keywords datasets data
#' @name Bacteroides
#' @usage Bacteroides
#' @format  dataframe
#' @seealso MFG
NULL
#' Acetogens dataframe
#'
#' Table of information describing the behaviour of the microbial functional group.
#' See help(MFG) or ?MFG for explanation of the contents of the microbial functional groups dataframes
#'
#' @docType data
#' @keywords datasets data
#' @name Acetogens
#' @usage Acetogens
#' @format  dataframe
#' @seealso MFG
NULL
#' ButyrateProducers1 dataframe
#'
#' Table of information describing the behaviour of the microbial functional group.
#' See help(MFG) or ?MFG for explanation of the contents of the microbial functional groups dataframes
#'
#' @docType data
#' @keywords datasets data
#' @name ButyrateProducers1
#' @usage ButyrateProducers1
#' @format  dataframe
#' @seealso MFG
NULL
#' ButyrateProducers2 dataframe
#'
#' Table of information describing the behaviour of the microbial functional group.
#' See help(MFG) or ?MFG for explanation of the contents of the microbial functional groups dataframes
#'
#' @docType data
#' @keywords datasets data
#' @name ButyrateProducers2
#' @usage ButyrateProducers2
#' @format  dataframe
#' @seealso MFG
NULL
#' ButyrateProducers3 dataframe
#'
#' Table of information describing the behaviour of the microbial functional group.
#' See help(MFG) or ?MFG for explanation of the contents of the microbial functional groups dataframes
#'
#' @docType data
#' @keywords datasets data
#' @name ButyrateProducers3
#' @usage ButyrateProducers3
#' @format  dataframe
#' @seealso MFG
NULL
#' LactateProducers dataframe
#'
#' Table of information describing the behaviour of the microbial functional group.
#' See help(MFG) or ?MFG for explanation of the contents of the microbial functional groups dataframes
#'
#' @docType data
#' @keywords datasets data
#' @name LactateProducers
#' @usage LactateProducers
#' @format  dataframe
#' @seealso MFG
NULL
#' Methanogens dataframe
#'
#' Table of information describing the behaviour of the microbial functional group.
#' See help(MFG) or ?MFG for explanation of the contents of the microbial functional groups dataframes
#'
#' @docType data
#' @keywords datasets data
#' @name Methanogens
#' @usage Methanogens
#' @format  dataframe
#' @seealso MFG
NULL
#' NoButyFibreDeg dataframe
#'
#' Table of information describing the behaviour of the microbial functional group.
#' See help(MFG) or ?MFG for explanation of the contents of the microbial functional groups dataframes
#'
#' @docType data
#' @keywords datasets data
#' @name NoButyFibreDeg
#' @usage NoButyFibreDeg
#' @format  dataframe
#' @seealso MFG
NULL
#' NoButyStarchDeg dataframe
#'
#' Table of information describing the behaviour of the microbial functional group.
#' See help(MFG) or ?MFG for explanation of the contents of the microbial functional groups dataframes
#'
#' @docType data
#' @keywords datasets data
#' @name NoButyStarchDeg
#' @usage NoButyStarchDeg
#' @format  dataframe
#' @seealso MFG
NULL
#' PropionateProducers dataframe
#'
#' Table of information describing the behaviour of the microbial functional group.
#' See help(MFG) or ?MFG for explanation of the contents of the microbial functional groups dataframes
#'
#' @docType data
#' @keywords datasets data
#' @name PropionateProducers
#' @usage PropionateProducers
#' @format  dataframe
#' @seealso MFG
NULL
#' Xsu dataframe
#'
#' Table of information describing the behaviour of the microbial functional group.
#' See help(MFG) or ?MFG for explanation of the contents of the microbial functional groups dataframes
#'
#' @docType data
#' @keywords datasets data
#' @name Xsu
#' @usage Xsu
#' @format  dataframe
#' @seealso MFG
NULL
#' Xaa dataframe
#'
#' Table of information describing the behaviour of the microbial functional group.
#' See help(MFG) or ?MFG for explanation of the contents of the microbial functional groups dataframes
#'
#' @docType data
#' @keywords datasets data
#' @name Xaa
#' @usage Xaa
#' @format  dataframe
#' @seealso MFG
NULL
#' Xh2 dataframe
#'
#' Table of information describing the behaviour of the microbial functional group.
#' See help(MFG) or ?MFG for explanation of the contents of the microbial functional groups dataframes
#'
#' @docType data
#' @keywords datasets data
#' @name Xh2
#' @usage Xh2
#' @format  dataframe
#' @seealso MFG
NULL
#' resourceSysInfoHuman dataframe
#'
#' Table of information describing the inflows, outflows, start values and molar masses of each resource for the R script microPop/inst/DemoFiles/human*.R
#' See help(resourceSysInfo) or for an explanation of the contents 
#'
#' @docType data
#' @keywords datasets data
#' @name resourceSysInfoHuman
#' @usage resourceSysInfoHuman
#' @format  dataframe
#' @seealso resourceSysInfo
NULL
#' resourceSysInfoRumen dataframe
#'
#' Table of information describing the inflows, outflows, start values and molar masses of each resource for the R script microPop/inst/DemoFiles/rumen*.R
#' See help(resourceSysInfo) or for an explanation of the contents 
#'
#' @docType data
#' @keywords datasets data
#' @name resourceSysInfoRumen
#' @usage resourceSysInfoRumen
#' @format  dataframe
#' @seealso resourceSysInfo
NULL
#' systemInfoResourcesPhyto dataframe
#'
#' Table of information describing the inflows, outflows, start values and molar masses of each resource for the R script microPop/inst/DemoFiles/phyto.R
#' See help(resourceSysInfo) or for an explanation of the contents 
#'
#' @docType data
#' @keywords datasets data
#' @name systemInfoResourcesPhyto
#' @usage systemInfoResourcesPhyto
#' @format  dataframe
#' @seealso resourceSysInfo
NULL
#' systemInfoResourcesVirus dataframe
#'
#' Table of information describing the inflows, outflows, start values and molar masses of each resource for the R script microPop/inst/DemoFiles/phages.R
#' See help(resourceSysInfo) or for an explanation of the contents 
#'
#' @docType data
#' @keywords datasets data
#' @name systemInfoResourcesVirus
#' @usage systemInfoResourcesVirus
#' @format  dataframe
#' @seealso resourceSysInfo
NULL
#' microbeSysInfoHuman dataframe
#'
#' Table of information describing the inflows, outflows, start values of each microbial group for the R script microPop/inst/DemoFiles/human*.R
#' See help(microbeSysInfo) or for an explanation of the contents 
#'
#' @docType data
#' @keywords datasets data
#' @name microbeSysInfoHuman
#' @usage microbeSysInfoHuman
#' @format  dataframe
#' @seealso microbeSysInfo
NULL
#' microbeSysInfoRumen dataframe
#'
#' Table of information describing the inflows, outflows, start values of each microbial group for the R script microPop/inst/DemoFiles/rumen.R
#' See help(microbeSysInfo) or for an explanation of the contents 
#'
#' @docType data
#' @keywords datasets data
#' @name microbeSysInfoRumen
#' @usage microbeSysInfoRumen
#' @format  dataframe
#' @seealso microbeSysInfo
NULL
#' systemInfoMicrobesPhyto dataframe
#'
#' Table of information describing the inflows, outflows, start values of each microbial group for the R script microPop/inst/DemoFiles/phyto.R
#' See help(microbeSysInfo) or for an explanation of the contents 
#'
#' @docType data
#' @keywords datasets data
#' @name systemInfoMicrobesPhyto
#' @usage systemInfoMicrobesPhyto
#' @format  dataframe
#' @seealso microbeSysInfo
NULL
#' systemInfoMicrobesVirus dataframe
#'
#' Table of information describing the inflows, outflows, start values of each microbial group for the R script microPop/inst/DemoFiles/phages.R
#' See help(microbeSysInfo) or for an explanation of the contents 
#'
#' @docType data
#' @keywords datasets data
#' @name systemInfoMicrobesVirus
#' @usage systemInfoMicrobesVirus
#' @format  dataframe
#' @seealso microbeSysInfo
NULL
#' strainParams dataframe
#'
#' Table containing some parameter values for specific strains for the R script microPop/inst/DemoFiles/human4.R
#' The file must have colnames c(strainName, paramName, paramVal, paramUnit, resource,path) where strainName is in format 'groupName.i' where i is the strain number.
#'
#' @docType data
#' @keywords datasets data
#' @name strainParams
#' @usage strainParams
#' @format  dataframe
NULL
