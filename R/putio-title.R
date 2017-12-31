#' List imdb titles in a folder
#' 
#' @export
putio_imdb_list <- function(putio_parent_id, con = NULL) {
    if (is.null(con)) {
        con <- pool::poolCheckout(db_pool())
        on.exit(pool::poolReturn(con))
    }
    
    user_files <- putio_cached_files_list(putio_parent_id, con = con) %>% 
        dplyr::select(putio_user_file_id = "id", name, size)
    
    title_files <- dplyr::tbl(con, "title_request_file") %>% 
        dplyr::select("imdb_id", putio_user_file_id = "putio_file_id")
    
    dplyr::inner_join(user_files, title_files, by = "putio_user_file_id")
}
