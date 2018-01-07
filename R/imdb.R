#' Read imdb dataset file
#' 
#' Couldn't solve some weird quoting error and wrote this custom tsv
#' reading function instead.
#' 
#' @export
imdb_read <- function(type = c("episode", "basics"), file_name) {
    logger <- get_logger()
    
    type <- match.arg(type)
    col_count = c(episode = 4, basics = 9)[type]
    
    data_matrix <- readr::read_lines(file_name) %>% 
        stringr::str_split_fixed("\t", col_count)
    
    header <- data_matrix[1,]
    body <- tail(data_matrix, -1)
    body <- replace(body, body == "\\N", NA)
    
    colnames(body) <- header
    dplyr::as_tibble(body)
}

#' Download imdb dataset
#' 
#' @export
imdb_download <- function(type = c("episode", "basics")) {
    logger <- get_logger()
    
    type <- match.arg(type)
    output_file <- tempfile(fileext = ".tsv.gz")

    url <- glue("http://datasets.imdbws.com/title.{type}.tsv.gz")
    logger(glue("Downloading to file'{output_file}'"))
    download.file(url, output_file, method = "wget")
    
    output_file
}

#' Write imdb dataset to db
#' 
#' @export
imdb_write <- function(episode_file = NULL, basics_file = NULL) {
    logger <- get_logger("imdb_write")
    
    if (is.null(episode_file)) {
        episode_file <- imdb_download("episode")
    }
    
    if (is.null(basics_file)) {
        basics_file <- imdb_download("basics")
    }
    
    logger("Tidying imdb data")

    episode_data <- imdb_read("episode", episode_file) %>% 
        dplyr::select(
            id = "tconst", parent_id = "parentTconst", 
            season = "seasonNumber", episode = "episodeNumber") %>% 
        dplyr::mutate_at(c("season", "episode"), as.integer)

    title_data <- imdb_read("basics", basics_file) %>% 
        dplyr::select(
            id = "tconst", type = "titleType", name = "primaryTitle",
            year = "startYear", duration_minute = "runtimeMinutes") %>% 
        dplyr::mutate_at(c("year", "duration_minute"), as.integer)
    
    combined_data <- dplyr::left_join(title_data, episode_data, by = "id")

    logger("Writing to db")
    
    pool::poolWithTransaction(db_pool(), function(con) {
        DBI::dbSendStatement(con, "TRUNCATE title")
        DBI::dbWriteTable(con, "title", combined_data, append = TRUE)
    })
    
    logger("Complete")
}