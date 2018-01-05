#' Cache a response to json api
#' 
#' @export
cache_api_response <- function(response) {
    logger <- get_logger()
    parsed_url <- httr::parse_url(response$request$url)
    response_json_text <- httr::content(response, "text", encoding = "UTF-8")
    to_json_text <- function(obj) as.character(jsonlite::toJSON(obj, auto_unbox = TRUE))
    
    id <- pool::poolWithTransaction(db_pool(), function(con) {
        insert_row(con, "api_response", list(
            hostname = parsed_url$hostname,
            path = parsed_url$path, 
            query = to_json_text(parsed_url$query),
            fields = to_json_text(response$request$fields),
            response = response_json_text,
            method = response$request$method,
            status_code = response$status_code,
            duration = as.numeric(response$times["total"])
        ), returning = "id")
    })
    
    json <- jsonlite::fromJSON(response_json_text)
    
    structure(list(
        response = response,
        json = json,
        id = id
    ), class = "cached_api_response")
}