torrentapi_url <- "https://torrentapi.org"

#' Get torrentapi token
#' 
#' @export
torrentapi_token <- memoise::memoise(function() {
    logger <- get_logger("torrentapi_token")
    
    response <- httr::RETRY(
        "GET",
        torrentapi_url, 
        path = "pubapi_v2.php",
        query = list(get_token = "get_token"),
        httr::timeout(2),
        times = 5
    )
    
    token <- httr::content(response, "parsed")$token
    assert_that(!is.null(token))
    
    logger(glue("Got token:{token}."))
    
    # There is a 1req/2sec limit and the first time this function is called,
    # the next call definitly fails without sleep.
    Sys.sleep(2)

    token
}, ~memoise::timeout(15 * 60)) # tokens expire in 15 minutes

#' Search torrents with imdb id
#' 
#' @export
search_torrent <- function(title_id, season = NULL, ...) {
    logger <- get_logger()
    
    logger(paste0(
        glue("Searching torrents for title:{title_id}"),
        glue(" season:{season}"),
        "."
    ))
    
    query <- list(
        token = torrentapi_token(),
        mode = "search",
        search_imdb = to_imdb(title_id),
        sort = "seeders",
        limit = 100
    )
    
    if (!is.null(season))  query$search_string <- sprintf("S%02i", season)
        
    response <- httr::RETRY(
        "GET",
        torrentapi_url,
        path = "pubapi_v2.php",
        query = query,
        times = 5
    )
    
    api <- save_api(response, ...)
    
    if (!is.null(api$error)) {
        logger(glue("Torrentapi error: {api$error}"))
        return()
    }
    
    logger(glue("Search returned {nrow(api$torrent_results)} result(s)."))
    
    if (!is.null(season)) {
        name <- api$torrent_results$filename
        episode <- extract_episode(name, season)
        season_pack <- stringr::str_detect(name, "\\.S[0-9]{2}\\.")
    } else {
        episode <- NA_character_
        season_pack <- NA
    }
    
    hash_regex <- "(?<=urn:btih:)[0-9a-z]+"
    
    api$torrent_results %>% 
        dplyr::as_tibble() %>% 
        dplyr::rename(name = "filename", source = "download") %>% 
        dplyr::mutate(
            hash = stringr::str_extract(source, hash_regex), 
            episode = !!episode, 
            season_pack = !!season_pack,
            seeder_rank = row_number(),
            full_hd = stringr::str_detect(name, "1080p"),
            hd = stringr::str_detect(name, "720p|HDTV")) %>% 
        bind_putio_download() 
}