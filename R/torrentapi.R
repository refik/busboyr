torrentapi_url <- "https://torrentapi.org"

#' Get torrentapi token
#' 
#' @export
torrentapi_token <- memoise::memoise(function() {
    response <- httr::GET(
        torrentapi_url, 
        path = "pubapi_v2.php",
        query = list(get_token = "get_token")
    )
    
    # There is a 1req/2sec limit and the first time this function is called,
    # the next call definitly fails without sleep.
    Sys.sleep(2)
    
    httr::content(response, "parsed")$token
}, ~memoise::timeout(15 * 60)) # tokens expire in 15 minutes

#' Search torrents with imdb id
#' 
#' @export
torrentapi_imdb_search <- function(imdb_id, season = NULL, episode = NULL, 
                                   season_pack_only = FALSE) {
    logger <- get_logger()
    
    query <- list(
        token = torrentapi_token(),
        mode = "search",
        search_imdb = imdb_id,
        sort = "seeders",
        limit = 100
    )
    
    if (!is.null(season)) {
        search_string <- sprintf("S%02d", season)
        if (!is.null(episode)) {
            search_string <- sprintf("%sE%02d", search_string, episode)
        }
        query$search_string <- search_string
    }
    
    response <- httr::RETRY(
        "GET",
        torrentapi_url,
        path = "pubapi_v2.php",
        query = query
    )
    
    cached_response <- cache_api_response(response)
    
    if (!is.null(cached_response$json$error)) {
        stop(cached_response$json$error)
    } else {
        torrents <- cached_response$json$torrent_results %>% 
            dplyr::mutate(hash = stringr::str_extract(download, 
                                                      "(?<=urn:btih:)[0-9a-z]+"))
        
        logger(glue("Search returned {nrow(torrents)} results"))
        
        if (is.null(episode) && !is.null(season) && season_pack_only == TRUE) {
            season_pack_regex <- "[^A-Za-z0-9]S[0-9]{2}[^eE]"
            torrents <- dplyr::filter(torrents, 
                                      stringr::str_detect(filename, season_pack_regex))
        }
    }
    
    torrents
}