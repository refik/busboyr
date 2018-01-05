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
get_logger <- function() {
    calling_fn_name <- deparse(sys.call(-1)[[1]])
    calling_fn_args <- as.list(parent.frame())
    calling_fn_args <- calling_fn_args %>% 
        purrr::discard(is.null) %>% # Discarding NULLs
        purrr::map_chr(function(arg_val) {
            if (length(arg_val) > 1) glue("<len{length(arg_val)}vec>")
            else as.character(arg_val)
        }) %>% 
        purrr::map_chr(function(arg_val) {
            if (nchar(arg_val) > 11) paste0(strtrim(arg_val, 7), "...")
            else arg_val
        })
    
    if (length(calling_fn_args) > 0) {
        calling_fn_args_str <- paste(
            names(calling_fn_args), 
            unlist(calling_fn_args), 
            sep = ":", collapse = ", ")
    } else {
        calling_fn_args_str <- ""
    }

    first_call_log_template <- "{now_debug()} - {calling_fn_name} - {calling_fn_args_str}"
    message(glue(first_call_log_template))
    
    log_template <- "{now_debug()} - {calling_fn_name} - {msg}"
    function(msg) message(glue(log_template))
}