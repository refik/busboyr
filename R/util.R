#' Converts integer64 to list before pmap
#' 
#' @export
pre_pmap_int64 <- function(tbl) {
    int64_vec_to_list <- function(int64) purrr::map(int64, ~.)
    dplyr::mutate_if(tbl, bit64::is.integer64, int64_vec_to_list)
}

#' Get episode from torrent name
#' 
#' @export
extract_episode <- function(name, season) {
    episode_regex <- glue("(?<=[Ss]{sprintf('%02i', season)}[Ee])[0-9]{{2}}")
    as.integer(stringr::str_extract(name, episode_regex))
}

#' Evaluate an expression and print result
#' 
#' @export
eval_expression <- function(expression) {
    logger <- get_logger()
    
    expression %>% 
        parse(text = .) %>% 
        eval() %>% 
        utils::capture.output() %>% 
        purrr::map(logger) %>% 
        invisible()
}

#' Number to imdb id text
#' 
#' @export
to_imdb <- function(title_id) {
    sprintf("tt%07i", title_id)
}


#' Imdb text id to number
#' 
#' @export
from_imdb <- function(title_id) {
    as.integer(substr(title_id, 3, 9))
}

#' Converting a named list to character
#' 
#' @export
log_named_list <- function(lst) {
    name <- names(lst)
    value <- purrr::map_chr(lst, as.character)
    paste(name, value, sep = ":", collapse = ", ")
}
