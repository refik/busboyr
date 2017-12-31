omdb_api_url <- "https://www.omdbapi.com"

#' Search OMDB for a title
#' 
#' @export
omdb_search <- function(query) {
    response <- httr::GET(
        omdb_api_url,
        query = list(
            apikey = Sys.getenv("OMDB_APIKEY"),
            s = query
        )
    )
    
    cache_api_response(response)
}

#' Get detailed title information from OMDB
#' 
#' @export
omdb_title <- function(imdb_id) {
    response <- httr::GET(
        omdb_api_url,
        query = list(
            apikey = Sys.getenv("OMDB_APIKEY"),
            i = imdb_id
        )
    )
    
    cache_api_response(response)
}
