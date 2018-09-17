#' Request a title to download
#' 
#' @export
create_request <- function(user_id, title_id, season = NULL) {
    logger <- get_logger()
    
    pool::poolWithTransaction(db_pool(), function(con) {
        request_id <- insert_row("request", list(
            user_id = user_id,
            title_id = title_id,
            season = season
        ), returning = "id", con = con)
        
        create_task("process_request", list(request_id = request_id))
    })
    
    invisible()
}

#' Download a torrent for a single title request
#' 
#' @export
process_request <- function(request_id) {
    logger <- get_logger()
    
    request <- get_record("request", request_id)
    
    logger(glue("Processing request from user:{request$user_id} ", 
                "for title:{request$title_id}."))
    
    torrents <- search_torrent(request$title_id, season = request$season)

    if (is.null(torrents)) {
        "Couldn't find suitable torrents to download." %>% 
            logger() %>% 
            stop(call. = FALSE)
    }
    
    if (!is.null(request$season)) {
        logger(glue("Request is for season:{request$season}. ", 
                    "Checking season_pack's."))

        if (any(torrents$season_pack)) {
            logger("Season packs found.")
            
            # Only take good quality ones into consideration. Low seeder rank could
            # mean it is something else
            good_season_pack <- 
                torrents %>% 
                dplyr::filter(season_pack == TRUE, 
                              seeder_rank < 5)
            
            pack_on_putio <- any(good_season_pack$on_putio)
            non_pack_on_putio <- 
                torrents %>% 
                dplyr::filter(season_pack == FALSE, on_putio == TRUE) %>% 
                nrow() > 0
            
            logger(glue(
                "pack_on_putio:{pack_on_putio}, ",
                "non_pack_on_putio:{non_pack_on_putio}, ",
                "good_season_pack_count:{nrow(good_season_pack)}."
            ))
            
            season_pack_preferable <- 
                (pack_on_putio) ||
                (non_pack_on_putio == FALSE && nrow(good_season_pack) > 0)
        } else {
            season_pack_preferable <- FALSE
        }
        
        if (season_pack_preferable == TRUE) {
            logger("Prefering season_pack's.")
            torrents <- dplyr::filter(torrents, season_pack == TRUE)
        } else {
            logger("Prefering separate episode torrents.")
            missing_episode <- 
                season_status(request$user_id, request$title_id, 
                              request$season) %>% 
                dplyr::filter(is.na(file_id)) %>% 
                dplyr::pull(episode)
            
            logger(glue("User has {length(missing_episode)} missing episodes."))
            torrent_episode <- unique(torrents$episode)
            torrent_episode <- torrent_episode[!is.na(torrent_episode)]
            episode_not_on_torrent <- setdiff(missing_episode, torrent_episode)
            
            if (length(episode_not_on_torrent) != 0) {
                logger(glue("episode_not_on_torrent:", 
                            "{paste(episode_not_on_torrent, collapse = ',')}."))
            }
            
            torrents <- 
                torrents %>% 
                dplyr::filter(episode %in% !!missing_episode) %>% 
                dplyr::group_by(episode)
        }
    }
    
    prefer_full_hd <- get_full_hd(request$user_id, request$title_id)
    
    torrents %>% 
        dplyr::do(select_torrent(., prefer_full_hd)) %>% 
        dplyr::ungroup() %>% 
        purrr::pmap(function(name, source, episode, ...) {
            if (is.na(episode)) episode <- NULL
            start_download(request$user_id, request_id, name, source, 
                           request$title_id, season = request$season, 
                           episode = episode)
        })
}

#' Torrent selection algorithm
#' 
#' @export
select_torrent <- function(torrents, prefer_full_hd = FALSE) {

    if (prefer_full_hd == TRUE && any(torrents$full_hd)) {
        torrents <- dplyr::filter(torrents, full_hd == TRUE)
    } else if (prefer_full_hd == FALSE && !all(torrents$full_hd)) {
        torrents <- dplyr::filter(torrents, full_hd == FALSE)
    }
    
    torrents %>% 
        dplyr::arrange(
            desc(on_putio), 
            desc(hd), 
            desc(download_count), 
            seeder_rank) %>% 
        head(1)
}