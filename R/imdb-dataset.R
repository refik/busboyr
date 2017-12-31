#' Read imdb dataset file
#' 
#' Couldn't solve some weird quoting error and wrote this custom tsv
#' reading function instead.
#' 
#' @export
imdb_read_tsv <- function(type = c("episode", "basics"), file_name) {
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
imdb_download_dataset <- function(type = c("episode", "basics")) {
    type <- match.arg(type)
    output_file <- tempfile(fileext = ".tsv.gz")

    command <- paste(
        Sys.getenv("AWS_CLI_EXEC"),
        "s3api",
        "get-object",
        "--request-payer requester",
        "--bucket imdb-datasets",
        glue("--key documents/v1/current/title.{type}.tsv.gz"),
        output_file
    )
    
    message(glue("Downloading imdb-dataset '{type}' to file '{output_file}'..."))
    system(command)
    output_file
}

#' Write imdb dataset to db
#' 
#' @export
imdb_write_dataset <- function(episode_file = NULL, basics_file = NULL) {
    if (is.null(episode_file)) {
        episode_file <- imdb_download_dataset("episode")
    }
    
    if (is.null(basics_file)) {
        basics_file <- imdb_download_dataset("basics")
    }
    
    message("Tidying imdb-datasets...")

    episode_data <- imdb_read_tsv("episode", episode_file) %>% 
        dplyr::select(
            imdb_id = "tconst", parent_imdb_id = "parentTconst", 
            season = "seasonNumber", episode = "episodeNumber") %>% 
        dplyr::mutate_at(c("season", "episode"), as.integer)

    title_data <- imdb_read_tsv("basics", basics_file) %>% 
        dplyr::select(
            imdb_id = "tconst", type = "titleType", name = "primaryTitle",
            start_year = "startYear", end_year = "endYear", 
            duration = "runtimeMinutes") %>% 
        dplyr::mutate_at(c("start_year", "end_year", "duration"), as.integer)
    
    joined_title_episode <- dplyr::left_join(title_data, episode_data, by = "imdb_id")

    message("Writing imdb-dataset to db...")
    
    pool::poolWithTransaction(db_pool(), function(con) {
        DBI::dbWriteTable(con, "imdb_dataset", joined_title_episode, append = TRUE)
    })
}