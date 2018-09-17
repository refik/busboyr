#' Get episodes of a series
#' 
#' @export
tbl_episode <- function(title_id, season = NULL, ...) {
    episode <- get_table("title", ...) %>% 
        # dplyr::filter(parent_id == !!title_id, !is.na(duration_minute)) %>%
        dplyr::filter(parent_id == !!title_id) %>%
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

#' Get detailed title information
#' 
#' @export
get_title <- function(title_id) {
    api <- get_table("api") %>% 
        dplyr::filter(hostname == "www.omdbapi.com", 
                      sql("query ->> 'i'") == !!to_imdb(title_id)) %>% 
        last_saved_json()
    
    if (is.null(api)) {
        api <- omdb_title(title_id)
    }
    
    list(
        id = title_id,
        name = api$Title,
        type = api$Type,
        year = as.integer(substr(api$Year, 0, 4)),
        plot = api$Plot,
        poster = ifelse(api$Poster == "N/A", NA_character_, api$Poster)
    )
}

#' Get titles in a data frame
#' 
#' @export
tbl_get_title <- function(title_id) {
    if (!is.null(title_id) && length(title_id) > 0) {
        purrr::map_dfr(title_id, get_title)
    }
}

#' Search for an imdb title
#' 
#' @export
search_title <- function(name) {
    logger <- get_logger()
    
    name <- trimws(name)
    
    api <- get_table("api") %>% 
        dplyr::filter(hostname == "www.omdbapi.com", sql("query ->> 's'") == name) %>% 
        last_saved_json()
    
    if (is.null(api)) {
        logger("Not cached. Querying API.")
        api <- omdb_search(name)
    }
    
    if (!is.null(api$Error)) {
        logger(glue("OMDB error: {api$Error}"))
        NULL
    } else {
        logger(glue("Found {nrow(api$Search)} results."))
        api$Search %>% 
            dplyr::select(id = "imdbID", name = "Title",  type = "Type", 
                          year = "Year", poster = "Poster") %>%
            dplyr::mutate(id = from_imdb(id),
                          year = as.integer(substr(year, 0, 4)),
                          poster = ifelse(poster == "N/A", NA_character_, poster)) %>% 
            dplyr::filter(type %in% c("movie", "series")) %>% 
            dplyr::as_tibble()
    }

}

#' Get status of a title
#' 
#' @export
title_status <- function(user_id, title_id) {
    logger <- get_logger()

    file_id <- get_table("file") %>% 
        dplyr::filter(user_id == !!user_id, title_id == !!title_id,
                      is.na(missing_at)) %>% 
        filter_last() %>% 
        dplyr::pull(id)
    
    if (length(file_id) != 1) {
        file_id <- NULL
    }
    
    has_download <- get_table("download") %>% 
        dplyr::filter(user_id == !!user_id, title_id == !!title_id,
                      is.na(completed_at)) %>%
        pull_count() > 0 
    
    has_request <- get_table("request") %>% 
        dplyr::filter(user_id == !!user_id, title_id == !!title_id,
                      is.na(completed_at)) %>% 
        pull_count() > 0
    
    if (!is.null(file_id)) {
        status <- "has_file"
    } else if (has_download == TRUE) {
        status <- "has_download"
    } else if (has_request == TRUE) {
        status <- "has_request"
    } else {
        status <- NA_character_
    }
    
    list(
        file_id = file_id,
        status = status
    )
}

#' Get status of the title's season
#' 
#' @export
season_status <- function(user_id, title_id, season) {
    logger <- get_logger()

    pool::poolWithTransaction(db_pool(), function(con) {
        episodes <- tbl_episode(title_id, season = season, con = con) %>% 
            dplyr::select("episode", "name", "year", "duration_minute")

        file <- get_table("file", con = con) %>% 
            dplyr::filter(user_id == !!user_id, title_id == !!title_id, 
                          season == !!season, is.na(missing_at)) %>% 
            dplyr::select(file_id = "id", "episode")

        has_download <- get_table("download", con = con) %>% 
            dplyr::filter(user_id == !!user_id, title_id == !!title_id,
                          season == !!season, is.na(completed_at)) %>%
            pull_count() > 0 

        has_request <- get_table("request", con = con) %>% 
            dplyr::filter(user_id == !!user_id, title_id == !!title_id,
                          season == !!season, is.na(completed_at)) %>% 
            pull_count() > 0
        
        logger("Joining all tables")
        
        episodes %>%
            dplyr::left_join(file, by = "episode") %>% 
            dplyr::mutate(has_download = !!has_download, 
                          has_request = !!has_request) %>% 
            dplyr::mutate(status = ifelse(
                !is.na(file_id),
                "has_file", ifelse(
                    has_download == TRUE,
                    "has_download", ifelse(
                        has_request == TRUE,
                        "has_request", NA
                    )
                )
            )) %>% 
            dplyr::select("episode", "name", "year", "duration_minute", 
                          "file_id", "status") %>% 
            dplyr::arrange(episode)
    })

}

#' Get all files of the user
#' 
#' @export
get_user_title <- function(user_id) {
    user_id %>% 
        get_user_title_id() %>% 
        tbl_get_title()
}

#' Get title_id user has
#' 
#' @export
get_user_title_id <- function(user_id) {
    get_table("file") %>% 
        dplyr::filter(user_id == !!user_id, is.na(missing_at)) %>% 
        dplyr::arrange(desc(id)) %>% 
        dplyr::distinct(title_id) %>% 
        dplyr::pull()
}