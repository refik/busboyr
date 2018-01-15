#' Organize the downloaded files
#' 
#' @export
create_file <- function(download_id) {
    logger <- get_logger("create_file")
    
    download <- get_record("download", download_id)
    user_id <- download$user_id
    logger(glue("Creating files for user_id:{user_id}, ", 
                "title_id:{download$title_id}"))

    api_transfer <- putio_get_transfer(user_id, download_id)$transfer
    api_file <- putio_files_list(user_id, parent_id = api_transfer$file_id)$files %>% 
        dplyr::filter(file_type == "VIDEO") 
    
    # Since we only call this function after a complete callback from put.io
    # download has to be completed.
    assert_that(api_transfer$status %in% c("SEEDING", "COMPLETED"), msg = glue(
        "Transfer status is ", "{api_transfer$status} ", 
        "rather then SEEDING or COMPLETE"
    ))

    if (!is.null(download$season)) {
        logger("Download is for a series season:{season}")
        parent_id <- get_folder(user_id, "season", download$title_id, 
                                season = download$season)
        
        logger("Joining download files with episodes for proper names.")
        
        episode <- tbl_episode(download$title_id, download$season) %>% 
            dplyr::select("episode", "name") %>% 
            dplyr::collect()
        
        download_file <- api_file %>% 
            dplyr::mutate(episode = extract_episode(name, download$season)) %>% 
            dplyr::filter(!is.na(episode)) %>% 
            dplyr::rename(original_name = "name") %>% 
            dplyr::inner_join(episode, by = "episode")
        
        if (!is.null(download$episode)) {
            logger(glue("Download is for the episode {download$episode}. Filtering it."))
            download_file <- dplyr::filter(download_file, episode == download$episode)
        }
        
        download_file <- download_file %>% 
            dplyr::mutate(name = glue("{sprintf('%02i', episode)} - {name}")) %>% 
            dplyr::select("id", "episode", "original_name", "name")
    } else {
        parent_id <- get_folder(user_id, "movies_root")
        title <- get_record("title", download$title_id)
        download_file <- api_file %>% 
            dplyr::arrange(desc(size)) %>% 
            dplyr::rename(original_name = "name") %>% 
            head(1) %>% 
            dplyr::mutate(name = title$name) %>% 
            dplyr::select("id", "original_name", "name")
    }
    
    logger(glue("Found {nrow(download_file)} related video file(s) in download. ",
                "Renaming them."))
    
    pool::poolWithTransaction(db_pool(), function(con) {
        purrr::pmap(download_file, function(id, original_name, name, episode = NULL) {
            putio_files_rename(user_id, id, name)
            insert_row("file", list(
                id = id,
                original_name = original_name,
                user_id = user_id,
                name = name,
                title_id = download$title_id,
                download_id = download$id,
                season = download$season,
                episode = episode
            ), con = con)
        })
        
        logger("Moving files and deleting unnecessary ones.")
        putio_files_move(user_id, download_file$id, parent_id)
        putio_files_delete(user_id, api_transfer$file_id)
    })
    
    refresh_title(download$user_id, download$title_id, download$season)
}

#' Get all files of the user
#' 
#' @export
get_user_title <- function(user_id) {
    title_id <- get_table("file") %>% 
        dplyr::filter(user_id == !!user_id) %>% 
        dplyr::distinct(title_id) %>% 
        dplyr::pull()
    
    if (length(title_id) != 0) {
        purrr::map_dfr(title_id, get_title)
    }
}