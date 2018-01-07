now_debug <- function() paste0(format(Sys.time(), format = "%M:%OS1"))

#' Simple logging function
#' 
#' @export
logger <- function(...) {
    msg <- list(...) %>% 
        purrr::discard(~length(.) == 0) %>% 
        paste(collapse = ", ")
    
    # get_fn_name <- function() {
    #     call_stack <- function(n) deparse(sys.call(n)[[1]])
    #     if (call_stack(-3) == "_f") call_stack(-4)
    #     else call_stack(-3)
    # }
    # 
    # if (prepend_call == TRUE) {
    #     fn_name <- get_fn_name()
    #     msg <- paste(fn_name, msg, sep = " - ")
    # }
    # 
    message(now(), msg)
}

#' Get a logger function
#' 
#' @export
get_logger <- function(calling_fn_name = NULL, exclude_vars = character()) {
    if (is.null(calling_fn_name)) {
        calling_fn_name <- deparse(sys.call(-1)[[1]])
    }
    calling_fn_args <- as.list(parent.frame())
    calling_fn_args <- calling_fn_args %>% 
        purrr::discard(~ is.null(.) || is.na(.)) %>% # Discarding NULLs
        purrr::map_chr(function(arg_val) {
            if (length(arg_val) > 1) glue("<vec({length(arg_val)})>")
            else as.character(arg_val)
        }) %>% 
        purrr::map_chr(function(arg_val) {
            if (nchar(arg_val) > 15) glue("<{strtrim(arg_val, 13)}>")
            else arg_val
        })
    
    # Removing unwanted variables
    calling_fn_args <- calling_fn_args[!(names(calling_fn_args) %in% exclude_vars)]
    
    if (length(calling_fn_args) > 0) {
        calling_fn_args_str <- paste(
            names(calling_fn_args), 
            unlist(calling_fn_args), 
            sep = ":", collapse = ", ")
    } else {
        calling_fn_args_str <- "<empty>"
    }

    first_call_log_template <- "[fn] {calling_fn_name} [env] {calling_fn_args_str}"
    message(glue(first_call_log_template))
    
    log_template <- "[fn] {calling_fn_name} [msg] {msg}"
    function(msg) message(glue(log_template))
}