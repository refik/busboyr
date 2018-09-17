#' Get busboy folders file_id on put.io
#' 
#' @export
get_folder <- function(user_id, type, title_id = NULL, season = NULL, 
                       refresh_shiny = FALSE) {
    logger <- get_logger()
    
    folder <- get_table("folder") %>% 
        dplyr::filter(user_id == !!user_id, type == !!type)
    
    if (!is.null(title_id)) {
        folder <- dplyr::filter(folder, title_id == !!title_id)
        if (!is.null(season)) {
            folder <- dplyr::filter(folder, season == !!season)
        }
    }
    
    file_id <- folder %>% 
        filter_last() %>% 
        dplyr::pull(id)
    
    if (length(file_id) == 1) {
        logger("There is a file. Checking if its still on put.io")
        
        api <- putio_files_get(user_id, file_id)
        
        if (api$status == "ERROR" && api$status_code == 404) {
            logger("Got 404 from putio. File is gone.")
            must_create_file <- TRUE
        } else {
            logger("There is a file and it is on put.io")
            must_create_file <- FALSE
        }
    } else {
        must_create_file <- TRUE
        logger("There is no file.")
    }
    
    if (must_create_file) {
        logger("Folder not found. Creating it.")
        
        folder_def <- list(
            root = list(name = Sys.getenv("ROOT_FOLDER")),
            season = list(parent = "series", type = "season"),
            series = list(parent = "series_root", type = "series"),
            buffer = list(parent = "root", name = "Files to Organize", type = "buffer"),
            series_root = list(parent = "root", name = "Series", type = "series_root"),
            movies_root = list(parent = "root", name = "Movies", type = "movies_root")
        )[[type]]

        parent_type <- folder_def$parent
        
        if (is.null(parent_type)) {
            parent_id <- 0
        } else if (parent_type == "series") {
            parent_id <- get_folder(user_id, parent_type, title_id)
        } else {
            parent_id <- get_folder(user_id, parent_type)
        }
        
        if (type == "series") {
            folder_def$name <- get_record("title", title_id)$name
        } else if (type == "season") {
            folder_def$name <- glue("Season {season}")
        }
        
        api <- putio_create_folder(user_id, name = folder_def$name, 
                                   parent_id = parent_id)
        
        file_id <- api$file$id
        
        insert_row("folder", list(
            id = file_id,
            user_id = user_id,
            type = type,
            title_id = title_id,
            season = season
        ))
    }
    
    file_id
}

