#' runMicroPopExample
#'
#' This function is similar to the demo() function but requires less interaction
#' It is used to run the canned examples from the microPop package.
#'
#' @param name Name of the example to run. If Name is NULL the list of examples will be printed. 
#'
#' @export
runMicroPopExample <- function(name = NULL) {
    
    if (is.null(name)) {
        all_examples <- grep("functions", sapply(Sys.glob(paste(system.file("DemoFiles", 
            package = "microPop"), "/*.R", sep = "")), function(filename) {
            return(sub("^([^.]*).*", "\\1", basename(filename)))
        }), invert = TRUE, value = TRUE)
        
        cat("List of microPop examples:\n\n")
        for (example in all_examples) {
            cat(paste(example, "\n"))
        }
        
    } else {
        example_file = system.file("DemoFiles", paste(name, ".R", sep = ""), package = "microPop")
        
        if (file.exists(example_file)) {
            print(paste("*** RUNNING EXAMPLE FILE", example_file))
            source(example_file,echo=TRUE,max.deparse.length=1e3)
        } else {
            stop(paste("No example named '", name, "' exists"))
        }
    }
}
