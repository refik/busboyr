omdb_api_url <- "https://www.omdbapi.com"

#' Search OMDB for a title
#' 
#' @export
omdb_search <- function(query, ...) {
    response <- httr::RETRY(
        "GET",
        omdb_api_url,
        query = list(
            apikey = Sys.getenv("OMDB_APIKEY"),
            s = query
        ),
        httr::timeout(2), 
        times = 5
    )
    
    save_api(response, ...)
}

#' Get detailed title information from OMDB
#' 
#' @export
omdb_title <- function(title_id, ...) {
    response <- httr::GET(
        omdb_api_url,
        query = list(
            apikey = Sys.getenv("OMDB_APIKEY"),
            i = to_imdb(title_id)
        ),
        httr::timeout(2)
    )
    
    save_api(response, ...)
}

#' Get detailed season information from OMDB
#' 
#' @export
omdb_season <- function(title_id, season, ...) {
    response <- httr::GET(
        omdb_api_url,
        query = list(
            apikey = Sys.getenv("OMDB_APIKEY"),
            i = to_imdb(title_id),
            Season = season
        )
    )
    
    save_api(response, ...)
}