#' Converts integer64 to list before pmap
#' 
#' @export
pre_pmap_int64 <- function(tbl) {
    int64_vec_to_list <- function(int64) purrr::map(int64, ~.)
    dplyr::mutate_if(tbl, bit64::is.integer64, int64_vec_to_list)
}

#' Minute second of current time
#' 
#' @export
now_short <- function() paste0(format(Sys.time(), format = "%M:%OS1"))

#' Get episode from torrent
#' 
#' @export
extract_episode <- function(name, season) {
    if (is.null(season)) return(rep(NA_integer_, length(name)))
    episode_regex <- glue("(?<=[Ss]{sprintf('%02i', season)}[Ee])[0-9]{{2}}")
    as.integer(stringr::str_extract(name, episode_regex))
}