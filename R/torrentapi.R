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
search_torrent <- function(title_id, season = NULL, ...) {
    logger <- get_logger()
    
    query <- list(
        token = torrentapi_token(),
        mode = "search",
        search_imdb = title_id,
        sort = "seeders",
        limit = 100
    )
    
    if (!is.null(season))  query$search_string <- sprintf("S%02i", season)
    
    response <- httr::RETRY(
        "GET",
        torrentapi_url,
        path = "pubapi_v2.php",
        query = query
    )
    
    api <- save_api(response, ...)
    
    if (!is.null(api$error)) {
        logger(glue("Torrentapi error: {api$error}"))
        torrents <- NULL
    } else {
        torrents <- api$torrent_results %>% 
            dplyr::rename(name = "filename", source = "download") %>% 
            dplyr::mutate(hash = stringr::str_extract(source, 
                                                      "(?<=urn:btih:)[0-9a-z]+"))
        
        if (!is.null(season)) {
            logger("Adding season_pack and episode information.")
            torrents <- dplyr::mutate(
                torrents, 
                season_pack = stringr::str_detect(name, "\\.S[0-9]{2}\\."), 
                episode = extract_episode(name, !!season)
            )
        }
        
        logger(glue("Search returned {nrow(torrents)} results"))
    }
    
    dplyr::as_tibble(torrents) %>% 
        dplyr::mutate(seeder_rank = row_number()) %>% 
        bind_putio_download() %>% 
        dplyr::mutate(full_hd = stringr::str_detect(name, "1080p"),
                      hd = stringr::str_detect(name, "720p|HDTV"))
}