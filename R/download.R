#' Start a download for a title
#' 
#' @export
start_download <- function(user_id, request_id, name, source, 
                           title_id, season = NULL, episode = NULL) {
    logger <- get_logger()
    
    id <- pool::poolWithTransaction(db_pool(), function(con) {
        parent_id <- get_folder(user_id, "buffer")
        uuid <- uuid::UUIDgenerate()
        api <- putio_add_transfer(user_id, source = source, save_parent_id = parent_id, 
                                  callback_url = callback_url(uuid))
        id <- api$transfer$id
        insert_row("download", list(
            id = id,
            request_id = request_id,
            user_id = user_id,
            title_id = title_id,
            name = name,
            source = source,
            season = season,
            episode = episode,
            uuid = uuid
        ), con = con)
        
        logger(glue("Created download {id}"))
        id
    })
    
    refresh_title(user_id, title_id, season)
}

