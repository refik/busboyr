#' Putio download table
#' 
#' @export
tbl_putio_download <- function(hash = NULL) {
    hash <- unique(hash)
    query <- "SELECT id, name, hash, download_count FROM downloads"
    tbl <- dplyr::tbl(putio_db_pool(), from = dplyr::sql(query))
    
    if (!is.null(hash)) {
        tbl <- dplyr::filter(tbl, hash %in% !!hash)
    }
    
    tbl
}

#' Bind putio download_count to a table with hash
#' 
#' @export
bind_putio_download <- function(tbl) {
    logger <- get_logger()
    
    download_count <- tbl_putio_download(hash = tbl$hash) %>% 
        dplyr::group_by(hash) %>% 
        dplyr::summarise(download_count = max(download_count, na.rm = TRUE)) %>% 
        dplyr::collect() %>% 
        dplyr::mutate_at("download_count", as.integer)
    
    logger(glue(
        "Binding {nrow(download_count)} download_count(s) from put.io", 
        " to {nrow(tbl)} row table."
    ))
    
    dplyr::left_join(tbl, download_count, by = "hash") %>% 
        dplyr::mutate(on_putio = !is.na(download_count))
}