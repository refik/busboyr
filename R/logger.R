now <- function() paste0(format(Sys.time(), format = "%H:%M:%OS1"), " - ")

#' Simple logging function
#' 
#' @export
logger <- function(..., prepend_call = TRUE) {
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