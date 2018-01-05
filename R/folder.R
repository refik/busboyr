#' Get a seasons folder on put.io for a user
#' 
#' @export
folder_season <- memoise::memoise(function(putio_user_id, imdb_id, season) {
    logger <- get_logger()
    
    user_file_id <- dplyr::tbl(db_pool(), "putio_busboy_folder") %>% 
        dplyr::filter(putio_user_id == !!putio_user_id, type == "season",
                      sql("meta ->> 'imdb_id'") == !!imdb_id,
                      sql("meta ->> 'season'") == as.character(season)) %>% 
        dplyr::arrange(desc(id)) %>% 
        head(1) %>% 
        dplyr::pull(putio_user_file_id)
    
    if (length(user_file_id) == 0) {
        logger("Folder not found, creating it")
        series_user_file_id <- folder_series_imdb(putio_user_id, imdb_id)
        api_response <- putio_create_folder(putio_user_id, name = glue("Season {season}"), 
                                            parent_id = series_user_file_id)
        user_file_id <- api_response$json$file$id
        folder_save(putio_user_id, "season", user_file_id, meta = list(
            imdb_id = imdb_id,
            season = season
        ))
    }
    
    user_file_id
})

#' Get the last created folder type id
#' 
#' @export
folder_generic <- function(putio_user_id, folder_type) {
    dplyr::tbl(db_pool(), "putio_busboy_folder") %>% 
        dplyr::filter(putio_user_id == !!putio_user_id, type == !!folder_type) %>% 
        dplyr::arrange(desc(id)) %>% 
        head(1) %>% 
        dplyr::pull(putio_user_file_id)
}

#' Saves folder user_file_id
#' 
#' @export
folder_save <- function(putio_user_id, folder_type, user_file_id, meta = NULL) {
    logger <- get_logger()
    con <- pool::poolCheckout(db_pool())
    on.exit(pool::poolReturn(con))
    
    if (!is.null(meta)) {
        meta <- jsonlite::toJSON(meta, auto_unbox = TRUE) %>% 
            as.character()
    } else {
        meta <- NA_character_
    }
    
    insert_row(con, "putio_busboy_folder", list(
        putio_user_id = putio_user_id,
        type = folder_type,
        putio_user_file_id = user_file_id,
        meta = meta
    ))
}

#' Get the buffer download folder
#' 
#' @export
folder_buffer <- function(putio_user_id) {
    user_file_id <- folder_generic(putio_user_id, "buffer")
    
    if (length(user_file_id) == 0) {
        root_user_file_id <- folder_root(putio_user_id)
        api_response <- putio_create_folder(putio_user_id, name = "Files to Organize", 
                                            parent_id = root_user_file_id)
        user_file_id <- api_response$json$file$id
        folder_save(putio_user_id, "buffer", user_file_id)
    }
    
    user_file_id
}

#' Get the movies folder
#' 
#' @export
folder_movies <- function(putio_user_id) {
    user_file_id <- folder_generic(putio_user_id, "movies")
    
    if (length(user_file_id) == 0) {
        root_user_file_id <- folder_root(putio_user_id)
        api_response <- putio_create_folder(putio_user_id, name = "Movies", 
                                            parent_id = root_user_file_id)
        user_file_id <- api_response$json$file$id
        folder_save(putio_user_id, "movies", user_file_id)
    }
    
    user_file_id
}

#' Get the series folder
#' 
#' @export
folder_series <- function(putio_user_id) {
    user_file_id <- folder_generic(putio_user_id, "series")
    
    if (length(user_file_id) == 0) {
        root_user_file_id <- folder_root(putio_user_id)
        api_response <- putio_create_folder(putio_user_id, name = "Series", 
                                            parent_id = root_user_file_id)
        user_file_id <- api_response$json$file$id
        folder_save(putio_user_id, "series", user_file_id)
    }
    
    user_file_id
}

#' Get a specific series folder
#' 
#' @export
folder_series_imdb <- function(putio_user_id, imdb_id) {
    logger <- get_logger()
    
    user_file_id <- dplyr::tbl(db_pool(), "putio_busboy_folder") %>% 
        dplyr::filter(putio_user_id == !!putio_user_id, 
                      sql("meta ->> 'imdb_id'") == !!imdb_id) %>% 
        dplyr::pull(putio_user_file_id)
    
    if (length(user_file_id) == 0) {
        logger("Folder not found. Creating it.")
        series_user_file_id <- folder_series(putio_user_id)
        title <- get_record("imdb_dataset", imdb_id, column = "imdb_id")
        api_response <- putio_create_folder(putio_user_id, name = title$name, 
                                            parent_id = series_user_file_id)
        user_file_id <- api_response$json$file$id
        folder_save(putio_user_id, "series_imdb", user_file_id, 
                    meta = list(imdb_id = imdb_id))
    }
    
    user_file_id
}

#' Get the root busboy folder
#' 
#' @export
folder_root <- function(putio_user_id) {
    user_file_id <- folder_generic(putio_user_id, "root")
    
    if (length(user_file_id) == 0) {
        api_response <- putio_create_folder(putio_user_id, name = "Busboy", parent_id = 0)
        user_file_id <- api_response$json$file$id
        folder_save(putio_user_id, "root", user_file_id)
    }
    
    user_file_id
}