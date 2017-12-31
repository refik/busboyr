#' Request a title's season
#' 
#' @export
request_season <- function(putio_user_id, imdb_id, season) {
    logger(glue("{putio_user_id}, {imdb_id}, {season}"))
    
    missing_title <- title_status(putio_user_id, imdb_id, season) %>% 
        dplyr::filter(is.na(putio_user_file_id)) %>% 
        dplyr::pull(imdb_id)
    
    if (length(missing_title) != 0) {
        pool::poolWithTransaction(db_pool(), function(con) {
            season_request_id <- insert_row(con, "season_request", list(
                putio_user_id = putio_user_id,
                imdb_id = imdb_id,
                season = season
            ), returning = "id")
            
            request_title(putio_user_id, missing_title, 
                          season_request_id = season_request_id)
            
            season_request_id
        })
    }
}

#' Add titles to download
#' 
#' @export
request_title <- function(putio_user_id, imdb_id, prefer_hd = FALSE, 
                          season_request_id = NULL) {
    logger(glue("{putio_user_id}, {length(imdb_id)} titles, hd {prefer_hd}"),
           glue("season request: {season_request_id}"))
    
    title_request_df <- dplyr::tibble(
        imdb_id = imdb_id,
        prefer_hd = prefer_hd,
        putio_user_id = putio_user_id,
        season_request_id = season_request_id
    )
    
    DBI::dbWriteTable(db_pool(), "title_request", title_request_df, append = TRUE)
}

#' Download a torrent for a single title request
#' 
#' @export
process_title_request <- function(title_request_id) {
    logger(title_request_id)
    
    title_request <- get_record("title_request", title_request_id)
    
    title <- dplyr::tbl(db_pool(), "imdb_dataset") %>% 
        dplyr::filter(imdb_id == title_request$imdb_id) %>% 
        dplyr::collect() %>% 
        as.list()
    
    if (title$type == "tvEpisode") {
        season <- title$season
        episode <- title$episode
    } else {
        season <- NULL
        episode <- NULL
    }
    
    torrent <- torrentapi_imdb_search(title$parent_imdb_id, season = season, 
                                      episode = episode) %>% 
        putio_bind_download_count() %>% 
        dplyr::arrange(desc(download_count)) %>% 
        head(1) %>% 
        as.list()
    
    title_request_start_download(putio_user_id, title_request_id, 
                                 torrent$filename, torrent$download)
}


#' Add a title's season to downloads for the user
#' 
#' @export
process_season_request <- function(season_request_id) {
    logger(season_request_id)
    
    season_request <- get_record("season_request", season_request_id)

    title_request_id <- dplyr::tbl(db_pool(), "title_request") %>% 
        dplyr::filter(season_request_id == !!season_request_id) %>% 
        dplyr::pull(id)
    
    if (length(title_request_id) > 2) {
        # If there is more than one title request for the season,
        # look for a season pack. Otherwise, check for a torrent for each
        # of them individually.
        torrent <- torrentapi_imdb_search(season_request$imdb_id, season_request$season, 
                                          season_pack_only = TRUE) %>% 
            putio_bind_download_count() %>% 
            dplyr::arrange(desc(download_count)) %>% 
            head(1) %>% 
            as.list()
            
        if (length(torrent) != 0) {
            title_request_start_download(season_request$putio_user_id, title_request_id, 
                                         torrent$filename, torrent$download)
            return(torrent)
        }
    } else {
        purrr::map(title_request_id, process_title_request)
    }
}

#' Create title request download
#' 
#' title_request_id can be a vector.
#' 
#' @export
title_request_start_download <- function(putio_user_id, title_request_id, 
                                         torrent_name, torrent_source) {
    logger(glue("user: {putio_user_id} for {length(title_request_id)} titles."))
    
    pool::poolWithTransaction(db_pool(), function(con) {
        parent_id <- folder_buffer(putio_user_id)
        callback_uuid <- uuid::UUIDgenerate()
        api_response <- putio_add_transfer(putio_user_id, source = torrent_source,
                                           save_parent_id = parent_id, 
                                           callback_uuid = callback_uuid)
        user_download_id <- api_response$json$transfer$id
        
        insert_row(con, "putio_transfer_complete_callback", list(
            putio_user_download_id = user_download_id,
            putio_user_id = putio_user_id,
            callback_uuid = callback_uuid
        ))
        
        DBI::dbWriteTable(con, "title_request_download", dplyr::tibble(
            title_request_id = title_request_id,
            original_file_name = torrent_name,
            source = torrent_source,
            putio_user_download_id = user_download_id
        ), append = TRUE)
        
        logger(glue("Created user download {user_download_id}"))
        
        user_download_id
    })
}


#' Get status of the title
#' 
#' @export
title_status <- function(putio_user_id, imdb_id, season = NULL) {
    con <- pool::poolCheckout(db_pool())
    on.exit(pool::poolReturn(con))
    
    if (!is.null(season)) {
        logger(glue("user: {putio_user_id}, imdb: {imdb_id}"), 
               glue("season: {season}"))
        episodes <- imdb_dataset_episode(imdb_id, season = season, con = con)
        
        title_file <- dplyr::tbl(con, "title_request_file") %>% 
            dplyr::filter(putio_user_id == !!putio_user_id) %>% 
            dplyr::select("imdb_id", "putio_user_file_id")
        
        title_download <- dplyr::tbl(con, "title_request_download") %>% 
            dplyr::select("title_request_id", "putio_user_download_id")
                
        title_request <- dplyr::tbl(con, "title_request") %>% 
            dplyr::filter(is.na(completed_at), putio_user_id == !!putio_user_id) %>%
            dplyr::select("imdb_id", title_request_id = "id")
        
        episodes %>%
            dplyr::left_join(title_file, by = "imdb_id") %>% 
            dplyr::left_join(title_request, by = "imdb_id") %>% 
            dplyr::left_join(title_download, by = "title_request_id") %>% 
            dplyr::mutate(status = ifelse(
                !is.na(putio_user_file_id),
                "ready", ifelse(
                    !is.na(putio_user_download_id),
                    "downloading", ifelse(
                        !is.na(title_request_id),
                        "requested", NA
                    )
                )
            )) %>% 
            dplyr::arrange(season, episode)
    }
}