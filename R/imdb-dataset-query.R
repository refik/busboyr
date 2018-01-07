#' Get episodes of a series
#' 
#' @export
tbl_episode <- function(title_id, season = NULL, ...) {
    episode <- get_table("title", ...) %>% 
        dplyr::filter(parent_id == !!title_id, !is.na(duration_minute)) %>%
        dplyr::arrange(season, episode)
    
    if (!is.null(season)) {
        dplyr::filter(episode, season == !!season)
    } else {
        episode
    }
}

#' Get seasons of a title
#' 
#' @export
title_season <- memoise::memoise(function(title_id) {
    tbl_episode(title_id) %>% 
        dplyr::distinct(season) %>% 
        dplyr::pull()
})