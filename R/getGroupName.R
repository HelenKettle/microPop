#' Convert strain name to its group name
#' e.g. 'Bacteroides.1' becomes 'Bacteroides'
#' @param xname a string (may be strain name or something else)
#' @param microbeNames vector of strings of microbial group names
#' @return group name (string) if xname is a strain name. If xname is not a the name of a strain it will simply return xname unchanged. 
#' @export


getGroupName=function(xname,microbeNames){

  g=as.numeric(gregexpr(pattern ='\\.',xname))-1 #find where dot is
  #gsub("^([^\\.]+)\\.?.*$","\\1", xname) (alternative method)
  if (g<=0){
    #not a strain name, therefore do not alter xname
    x=xname
  }else{
    gname=substring(xname,1,g)
    if (gname%in%microbeNames){x=gname}else{x=xname}
  }

  
  return(x)
  
}
