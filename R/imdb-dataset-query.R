#' Get episodes of a series
#' 
#' @export
imdb_dataset_episode <- function(imdb_id, season = NULL, con = db_pool()) {
    episodes <- dplyr::tbl(con, "imdb_dataset") %>% 
        dplyr::filter(parent_imdb_id == !!imdb_id, !is.na(duration)) %>%
        dplyr::select("imdb_id", "season", "episode", "name", "duration") %>% 
        dplyr::arrange(season, episode)
    
    if (!is.null(season)) {
        dplyr::filter(episodes, season == !!season)
    } else {
        episodes
    }
}

#' Get seasons of a title
#' 
#' @export
imdb_dataset_seasons <- memoise::memoise(function(imdb_id) {
    imdb_dataset_episode(imdb_id) %>% 
        dplyr::distinct(season) %>% 
        dplyr::pull()
})