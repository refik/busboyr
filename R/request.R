#' Request a title to download
#' 
#' @export
create_request <- function(user_id, title_id, prefer_hd = FALSE, 
                           season = NULL) {
    logger <- get_logger()
    
    request_id <- insert_row(request, list(
        user_id = user_id,
        title_id = title_id,
        prefer_hd = prefer_hd,
        season = season
    ), returning = "id")
    
    create_task("process_request", list(request_id = request_id))
}

#' Download a torrent for a single title request
#' 
#' @export
process_request <- function(request_id) {
    logger <- get_logger()
    
    request <- get_record("request", request_id)
    logger(glue("Request from user:{request$user_id} for title:{request$title_id}"))
    torrents <- search_torrent(request$title_id, season = request$season)

    if (is.null(torrents)) {
        logger("Couldn't find suitable torrents to download.")
        stop()
    }
    
    if (!is.null(request$season)) {
        logger(glue("Request is for season:{request$season}. Checking season_pack."))
        
        if (any(torrents$season_pack)) {
            season_pack_preferable <- dplyr::filter(torrents, season_pack == TRUE) %>% 
                dplyr::summarise(min(seeder_rank)) %>% 
                # Arbitrary number. If its too far down at seeders, its probably
                # something else.
                dplyr::pull() < 5 
        } else {
            season_pack_preferable <- FALSE
        }
        
        if (season_pack_preferable == TRUE) {
            logger("There are good quality season_pack torrents. Prefering them.")
            torrents <- dplyr::filter(torrents, season_pack == TRUE)
        } else {
            logger("Picking episode torrents that user doesn't have")
            
            missing_episode <- season_status(request$user_id, request$title_id, 
                                             request$season) %>% 
                dplyr::filter(is.na(file_id)) %>% 
                dplyr::pull(episode)
            
            logger(glue("User has {length(missing_episode)} missing episodes."))
            torrent_episode <- unique(torrents$episode)
            torrent_episode <- torrent_episode[!is.na(torrent_episode)]
            episode_not_on_torrent <- setdiff(missing_episode, torrent_episode)
            logger(glue("Torrent not found for episodes: ", 
                        "{paste(episode_not_on_torrent, collapse = ', ')}"))
            
            torrents <- dplyr::filter(torrents, episode %in% !!missing_episode) %>% 
                dplyr::group_by(episode)
        }
    }
    
    torrents %>% 
        dplyr::do(select_torrent(., request$prefer_full_hd)) %>% 
        dplyr::ungroup() %>% 
        purrr::pmap(function(name, source, episode = NULL, ...) {
            start_download(request$user_id, request_id, name, source, 
                           request$title_id, season = request$season, 
                           episode = episode)
        })

}

#' Torrent selection algorithm
#' 
#' @export
select_torrent <- function(torrents, prefer_full_hd = FALSE) {
    logger <- get_logger()
    
    if (prefer_full_hd == TRUE && any(torrents$full_hd)) {
        logger("There are some full_hd torrents, prefering them.")
        torrents <- dplyr::filter(torrents, full_hd == TRUE)
    } else if (prefer_full_hd == FALSE && !all(torrents$full_hd)) {
        logger("There are some non full_hd torrents, discarding full_hd.")
        torrents <- dplyr::filter(torrents, full_hd == FALSE)
    }
    
    torrent <- dplyr::arrange(torrents, desc(on_putio), desc(hd), 
                              desc(download_count), seeder_rank) %>% 
        head(1)
}