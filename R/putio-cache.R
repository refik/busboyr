#' Cached files list response from put.io
#' 
#' @export
putio_cached_files_list <- function(putio_parent_id, con = db_pool()) {
    putio_parent_id <- as.character(putio_parent_id)

    populate_files <- 
        "(json_populate_recordset(null::putio_file, response::json -> 'files')).*"
    
    dplyr::tbl(con, "api_response") %>% 
        dplyr::filter(hostname == "api.put.io", path == "v2/files/list", 
                      sql("query->>'parent_id'") == !!putio_parent_id) %>%
        dplyr::arrange(desc(id)) %>% 
        head(1) %>% 
        lax_select(populate_files)
}

#' Get user information from previous api response
#' 
#' @export
putio_cached_user <- function(putio_user_id) {
    user_id <- as.character(putio_user_id) # postgres cast problem
    dplyr::tbl(db_pool(), "api_response") %>% 
        dplyr::filter(hostname == "api.put.io", path == "v2/account/info", 
                      sql("response#>'{info,user_id}'") == !!user_id,
                      status_code == 200) %>% 
        dplyr::arrange(desc(id)) %>% 
        head(1) %>% 
        dplyr::pull(response) %>% 
        jsonlite::fromJSON() %>% 
        .$info
}

