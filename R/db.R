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
insert_row <- function(con, table, values, returning = NULL) {
    assert_that(
        all(!purrr::map_lgl(values, is.null)), 
        msg = glue("Can't have NULL value(s) while inserting to '{table}' table.")
    )

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
        result
    }
}

#' #' Escaping integer64 for sql queries
#' #'
#' #' @export
#' escape.integer64 <- function(x, parens = NA, collapse = ", ", con = NULL) {
#'     x[is.na(x)] <- "NULL"
#'     dbplyr::sql_vector(x, parens, collapse)
#' }

#' Generate random string
#' 
#' @export
random_string <- function(n = 10) {
    paste0(sample(letters, n, replace = TRUE), collapse = "")
}

#' Select statement with no column name control
#' 
#' @export
lax_select <- function(.data, select_expr) {
    alias <- random_string()
    original_query <- dbplyr::sql_render(.data)
    con <- .data$src$con
    query <- glue("SELECT {select_expr} FROM ({original_query}) \"{alias}\"")
    dplyr::tbl(con, from = dplyr::sql(query))
}

#' Generic function for getting record by id
#' 
#' @export
get_record <- memoise::memoise(function(table, record_id, column = "id") {
    dplyr::tbl(db_pool(), table) %>% 
        dplyr::filter(sql(column) == !!record_id) %>% 
        dplyr::collect() %>% 
        as.list()
})
