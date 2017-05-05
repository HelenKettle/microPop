#' Create a dataframe from a CSV file
#' @param filename A string containing the path to the csv file
#' @return A dataframe
#' @importFrom utils read.csv
#' @export
#' 
createDF=function(filename){

  #this function loads in a csv file and creates an R object (global)
  data=read.csv(filename,header=TRUE,stringsAsFactors=FALSE,row.names=1)

  return(data)

}
