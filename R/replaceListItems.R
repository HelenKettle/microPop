#' used to replace items in list.in in list.default
#' needed for processing microPop input args like plotOptions 
#' @param list.in input List
#' @param list.default Default List
#' @return list.default updated with entries from list.in
#' @export
replaceListItems = function(list.in, list.default) {
    list.out = replace(list.default, names(list.in), list.in)
    return(list.out)
}
