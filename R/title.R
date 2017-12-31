#' Get title information
#' 
#' @export
get_title <- function(imdb_id) {
    cached <- dplyr::tbl(db_pool(), "api_response") %>% 
        dplyr::filter(hostname == "www.omdbapi.com", sql("query ->> 'i'") == imdb_id) %>% 
        dplyr::arrange(desc(id)) %>% head(1) %>% 
        dplyr::select("response") %>% dplyr::pull()
    
    if (length(cached) == 0) {
        omdb_title(imdb_id)
    } else {
        jsonlite::fromJSON(cached)
    }
}