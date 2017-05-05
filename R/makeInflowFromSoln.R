#' Used for running microPop with multiple compartments

#' Takes the solution (state of system) from the previous compartment (out$solution)
#' and then finds the washout rate of each state variable using removalRateFunc
#' to find the inflow rate to the next downstream compartment 

#' @param out output from microPopModel()
#' @return matrix of flow rates (conc/time) with named columns (the same as out$solution) 
#' @export

makeInflowFromSoln=function(out){

  soln=out$solution
  all.names=colnames(soln)
  washOutRate=out$parms$Smats$washOut
  resNames=out$parms$resourceNames
  
  mat=soln*NA
  mat[,1]=soln[,'time']
  
  for (i in 2:length(soln[1,])){
    xname=all.names[i]
    x=getGroupName(xname,out$parms$microbeNames)
    for (t in 1:length(soln[,'time'])){
      mat[t,i]=out$parms$removalRateFunc(x,soln[t,xname],soln[t,],soln[t,'time'],washOutRate,out$parms)
    }
  }
 
  return(mat)
}


