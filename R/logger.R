#' Get a logger function
#' 
#' @param func_name This value is inferred from the call stack. However,
#'   it is inferred wrong if the function is memoised.
#' 
#' @export
get_logger <- function(func_name = NULL, shiny = FALSE) {
    # Get the name of the calling function if it is not given. 
    if (is.null(func_name)) {
        func_name <- deparse(sys.call(-1)[[1]])
    }

    # Check if function name is namespaced
    if (stringr::str_detect(func_name, "::")) {
        # In that case, we don't want the module
        func_name <- stringr::str_extract(func_name, "(?<=::).+")
    }
    
    func_name_log <- 
        glue("{ifelse(shiny, '(s)', '(b)')} `{func_name}()`") %>% 
        sprintf("% -27s", .)

    # Remaining logs will be printed with the format below.
    function(msg) {
        message(glue("{func_name_log} | {msg}"))
        invisible(msg)
    }
}