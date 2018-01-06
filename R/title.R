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

#' Run function in a loop
#' 
#' @export
consume_loop <- function(queue = c("task", "callback", "both")) {
    logger <- get_logger("consume_loop")
    
    fn <- list(
        task = task_consume,
        callback = transfer_complete_consume,
        both = function() {
            logger("Consuming both queues shortly for interative use.")
            task_consume(wait = 1)
            transfer_complete_consume(wait = 1)
            logger("Sleeping for 2 seconds.")
            Sys.sleep(2)
        }
    )[[match.arg(queue)]]

    repeat fn()
}