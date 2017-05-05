#' this function is needed due to R dropping the names when it subsets
#' @param array Original array
#' @param gname Group name (string)
#' @param path Path name (string)
subsetFunc=function(array,gname,path){
    
    if (length(dim(array))==3){
        v=array[gname,,path]
        resnames=colnames(array[gname,,path,drop=FALSE])
        names(v)=resnames
    }
    
    if (length(dim(array))==2){#already subsetted by gname or strain name
        v=as.vector(array[path,])
        resnames=colnames(array)
      names(v)=resnames
    }
    
  return(v)
}
