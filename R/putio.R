#' Get put.io db pool
#' 
#' @export
putio_db_pool <- memoise::memoise(function() {
    pool::dbPool(
        drv = RMariaDB::MariaDB(),
        user = Sys.getenv("PUTIO_DB_USER"),
        password = Sys.getenv("PUTIO_DB_PASSWORD"),
        host = Sys.getenv("PUTIO_DB_HOST"),
        dbname = "putio"
    )
})


#' Get user information from previous api response
#' 
#' @export
get_user <- function(user_id) {
    user_id <- as.character(user_id) # postgres cast problem
    get_table("api") %>% 
        dplyr::filter(hostname == "api.put.io", path == "v2/account/info", 
                      sql("json #> '{info,user_id}'") == !!user_id,
                      status_code == 200) %>% 
        last_saved_json() %>% 
        .$info
}

