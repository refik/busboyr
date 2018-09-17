#' Cache a response to json api
#' 
#' @export
save_api <- function(response, user_id = NULL, ...) {
    logger <- get_logger()
    
    parsed_url <- httr::parse_url(response$request$url)
    response_json_text <- httr::content(response, "text", encoding = "UTF-8")
    to_json_text <- function(obj) as.character(jsonlite::toJSON(obj, auto_unbox = TRUE))
    
    logger(glue(
        "Response status:{response$status_code} for ", 
        "host:{parsed_url$hostname}, path:{parsed_url$path}."
    ))
    
    insert_row("api", list(
        hostname = parsed_url$hostname,
        path = parsed_url$path, 
        query = to_json_text(parsed_url$query),
        fields = to_json_text(response$request$fields),
        json = response_json_text,
        method = response$request$method,
        status_code = response$status_code,
        duration_second = as.numeric(response$times["total"]),
        user_id = user_id
    ))

    jsonlite::fromJSON(response_json_text)
}

#' Shortcut function for getting the last response json
#' 
#' @export
last_saved_json <- function(tbl) {
    json <- 
        tbl %>% 
        filter_last() %>%
        dplyr::pull(json)
    
    if (length(json) == 0) NULL
    else jsonlite::fromJSON(json)
}