#' Organize the downloaded files
#' 
#' @export
organize_download <- function(putio_user_id, putio_user_download_id) {
    logger <- get_logger()
    con <- pool::poolCheckout(db_pool())
    on.exit(pool::poolReturn(con))

    transfer <- putio_get_transfer(putio_user_id, putio_user_download_id)$json$transfer
    
    assert_that(transfer$status == "COMPLETED")
    
    
    get_episode_helper <- function(name, type = c("episode", "season")) {
        episode_regex <- "[Ss][0-9]{2}[Ee][0-9]{2}"
        episode_string <- stringr::str_extract(name, episode_regex)
        if (type == "season") as.integer(substr(episode_string, 2, 3))
        else as.integer(substr(episode_string, 5, 6))
    } 
    
    transfered_files <- putio_files_list(putio_user_id, 
                                         parent_id = transfer$file_id)$json$files %>% 
        dplyr::filter(file_type == "VIDEO") %>% 
        dplyr::mutate(
            season = get_episode_helper(name, "season"),
            episode = get_episode_helper(name, "episode")) %>% 
        dplyr::select(season, episode, original_name = "name", user_file_id = "id")
    
    logger(glue("Found {nrow(transfered_files)} video files in download."))
    
    title_requests <- dplyr::tbl(con, "title_request") %>% 
        dplyr::select(title_request_id = "id", "imdb_id")
    
    imdb_dataset <- dplyr::tbl(con, "imdb_dataset") %>% 
        dplyr::select("parent_imdb_id", "imdb_id", "type", "name", "season", "episode")
    
    expected_titles <- dplyr::tbl(con, "title_request_download") %>% 
        dplyr::filter(putio_user_download_id == !!putio_user_download_id) %>% 
        dplyr::select("title_request_id") %>% 
        dplyr::left_join(title_requests, by = "title_request_id") %>% 
        dplyr::left_join(imdb_dataset, by = "imdb_id") %>% 
        dplyr::collect()
    
    expected_title_count <- nrow(expected_titles)
    logger(glue("{expected_title_count} titles were expected."))
    
    title_with_folder <- expected_titles %>% 
        dplyr::mutate(name = ifelse(
            type == "tvEpisode", 
            sprintf("%02i - %s", episode, name), name)) %>% 
        dplyr::mutate(putio_folder_id = purrr::pmap(., function(type, parent_imdb_id, season, ...) {
            if (type == "tvEpisode") {
                folder_season(putio_user_id, parent_imdb_id, season)
            } else {
                folder_movies(putio_user_id)
            }
        }))
    
    transfer_expectation_match <- dplyr::inner_join(title_with_folder, transfered_files, 
                                                    by = c("season", "episode"))
    
    assert_that(nrow(transfer_expectation_match) == expected_title_count)
    
    logger("Renaming and moving files")
    
    DBI::dbBegin(con)
    
    purrr::pmap(transfer_expectation_match, function(title_request_id, putio_folder_id, 
                                                     user_file_id, original_name, 
                                                     name, imdb_id,...) {
        putio_files_rename(putio_user_id, user_file_id, name)
        putio_files_move(putio_user_id, user_file_id, putio_folder_id)
        insert_row(con, "title_request_file", list(
            name = name,
            original_name = original_name,
            putio_user_file_id = user_file_id,
            title_request_id = title_request_id,
            imdb_id = imdb_id
        ))
    })
    
    putio_files_delete(putio_user_id, transfer$file_id)
    
    DBI::dbCommit(con)
}