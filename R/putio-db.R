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
