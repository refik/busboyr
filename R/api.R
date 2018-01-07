#' Cache a response to json api
#' 
#' @export
cache_api_response <- function(response, user_id = NULL) {
    parsed_url <- httr::parse_url(response$request$url)
    response_json_text <- httr::content(response, "text", encoding = "UTF-8")
    to_json_text <- function(obj) as.character(jsonlite::toJSON(obj, auto_unbox = TRUE))
    
    insert_row("api", list(
        hostname = parsed_url$hostname,
        path = parsed_url$path, 
        query = to_json_text(parsed_url$query),
        fields = to_json_text(response$request$fields),
        response = response_json_text,
        method = response$request$method,
        status_code = response$status_code,
        duration_second = as.numeric(response$times["total"]),
        user_id = user_id
    ))

    jsonlite::fromJSON(response_json_text)
}