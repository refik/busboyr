#' @importFrom dplyr %>% if_else tibble
#' @importFrom rlang .data quo enquo quo_name
#' @importFrom glue glue
#' @importFrom assertthat assert_that
NULL

#' Get a db pool
#' 
#' @export
db_pool <- memoise::memoise(function() {
  pool::dbPool(
    drv = RPostgres::Postgres(),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_PASSWORD"),
    host = Sys.getenv("DB_HOST"),
    port = as.integer(Sys.getenv("DB_PORT")),
    dbname = Sys.getenv("DB_NAME")
  )
})

#' Insert a row to db
#' 
#' Does interpolation to prevent sql injection.
#' 
#' @export
insert_row <- function(table, values, returning = NULL, con = NULL) {
    if (is.null(con)) {
        con <- pool::poolCheckout(db_pool())
        on.exit(pool::poolReturn(con))
    }
    
    # Discarding NULL values. They can't be inserted.
    values <- purrr::discard(values, is.null)

    if (!is.null(returning)) {
        returning_statement <- glue("RETURNING {returning}")
    } else {
        returning_statement <- ""
    }
    
    value_names <- paste0(names(values), collapse = ", ")
    value_identifiers <- paste0(paste0("?", names(values)), collapse = ", ")
    
    sql_template <- glue(
        "INSERT INTO {table} ({value_names}) 
         VALUES ({value_identifiers}) {returning_statement}"
    )
    
    sql_safe <- DBI::sqlInterpolate(con, sql_template, .dots = values)
    result <- DBI::dbSendStatement(con, sql_safe)

    if (!is.null(returning)) {
        id <- DBI::dbFetch(result, n = 1)
        DBI::dbClearResult(result)
        id[[1]]
    } else {
        DBI::dbClearResult(result)
        result
    }
}

#' Generate random string
#' 
#' @export
random_string <- function(n = 10) {
    paste0(sample(letters, n, replace = TRUE), collapse = "")
}

#' Select statement with no column name control
#' 
#' @export
lax_select <- function(tbl, select_expr) {
    alias <- random_string()
    original_query <- dbplyr::sql_render(tbl)
    con <- tbl$src$con
    query <- glue("SELECT {select_expr} FROM ({original_query}) \"{alias}\"")
    dplyr::tbl(con, from = dplyr::sql(query))
}

#' Generic function for getting record by id
#' 
#' @export
get_record <- memoise::memoise(function(table, record_id, ...) {
    get_table(table, ...) %>% 
        dplyr::filter(id == !!record_id) %>% 
        dplyr::collect() %>% 
        purrr::modify_if(is.na, ~NULL) %>% 
        as.list()
})

#' Shortcut for getting a table
#' 
#' @export
get_table <- function(table, con = db_pool(), ...) {
    # Suppressing warning for getting rid of
    # unknown column type warnings
    suppressWarnings(dplyr::tbl(con, table))
}

#' Shortcut for filtering the last record
#' 
#' @export
filter_last <- function(tbl) {
    dplyr::arrange(tbl, desc(id)) %>% head(1)
}

#' Shortcut for getting the count from db
#' 
#' @export
pull_count <- function(tbl) {
    dplyr::count(tbl) %>% dplyr::pull()
}
