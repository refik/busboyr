#' Get the hd preference of user
#' 
#' @export
get_full_hd <- function(user_id, title_id) {
    logger <- get_logger()
    
    get_table("full_hd") %>% 
        dplyr::filter(user_id == !!user_id, 
                      title_id == !!title_id) %>% 
        pull_count() > 0
}

#' Set the full hd preference
#' 
#' @export
set_full_hd <- function(user_id, title_id) {
    logger <- get_logger()

    insert_row("full_hd", list(
        user_id = user_id,
        title_id = title_id
    ))
}

#' Unser the full hd preference
#' 
#' @export
unset_full_hd <- function(user_id, title_id) {
    logger <- get_logger()
    
    DBI::dbExecute(db_pool(), glue(
        "DELETE FROM full_hd ",
        "WHERE user_id = {user_id} AND title_id = {title_id}"
    ))
}