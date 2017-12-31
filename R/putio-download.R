#' Putio download table
#' 
#' @export
putio_download <- function(hash = NULL) {
    query <- "SELECT id, name, hash, download_count FROM downloads"
    src <- dplyr::tbl(putio_db_pool(), from = dplyr::sql(query))
    if (!is.null(hash)) {
        dplyr::filter(src, hash %in% !!hash) %>% 
            dplyr::collect() %>% 
            dplyr::mutate_at("download_count", as.integer)
    } else {
        src
    }
}

#' Bind putio download_count to hash
#' 
#' @export
putio_bind_download_count <- function(df_with_hash) {
    download_count <- putio_download(hash = df_with_hash$hash) %>% 
        dplyr::select(hash, download_count)
    
    dplyr::left_join(df_with_hash, download_count, by = "hash")
}