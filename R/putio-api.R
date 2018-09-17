putio_api_url <- "https://api.put.io"

#' Get oauth token from putio user id
#' 
#' @export
putio_get_token <- memoise::memoise(function(user_id) {
    user_id <- as.character(user_id)
    get_table("api") %>% 
        dplyr::filter(hostname == "api.put.io", path == "v2/account/info", 
                      sql("json #> '{info,user_id}'") == !!user_id) %>% 
        filter_last() %>% 
        lax_select("query->>'oauth_token'") %>% 
        dplyr::pull()
})

#' put.io auth url for busboy
#' 
#' @export
putio_oauth_redirect_url <- function() {
    httr::modify_url(
        putio_api_url,
        path = "v2/oauth2/authenticate",
        query = list(
            client_id = Sys.getenv("PUTIO_CLIENT_ID"),
            response_type = "token",
            redirect_uri = Sys.getenv("PUTIO_REDIRECT_URL")
        )
    )
}

#' put.io account information
#' 
#' @export
putio_account_info <- function(user_id = NULL, 
                               oauth_token = putio_get_token(user_id)) {
    response <- httr::GET(
        putio_api_url,
        path = "v2/account/info",
        query = list(oauth_token = oauth_token)
    )
    
    save_api(response)
}

#' Get information about a transfer
#' 
#' @export
putio_get_transfer <- function(user_id, putio_user_download_id,
                               oauth_token = putio_get_token(user_id)) {
    response <- httr::GET(
        putio_api_url,
        path = glue("v2/transfers/{putio_user_download_id}"),
        query = list(oauth_token = oauth_token)
    )
    
    save_api(response, user_id)
}

#' Add transfer to put.io
#' 
#' @export
putio_add_transfer <- function(user_id = NULL, source, save_parent_id = 0, 
                               callback_url = NULL,
                               oauth_token = putio_get_token(user_id)) {
    response <- httr::POST(
        putio_api_url,
        path = "v2/transfers/add",
        query = list(oauth_token = oauth_token),
        body = list(
            url = source,
            save_parent_id = as.character(save_parent_id),
            callback_url = callback_url
        )
    )
    
    save_api(response, user_id)
}

#' Get users files
#' 
#' @export
putio_files_list <- function(user_id = NULL, parent_id = 0,
                             oauth_token = putio_get_token(user_id)) {
    response <- httr::GET(
        putio_api_url,
        path = "v2/files/list",
        query = list(
            parent_id = parent_id,
            oauth_token = oauth_token
        )
    )
    
    save_api(response, user_id)
}

#' Get users files
#' 
#' @export
putio_files_get <- function(user_id = NULL, file_id,
                             oauth_token = putio_get_token(user_id)) {
    response <- httr::GET(
        putio_api_url,
        path = glue("v2/files/{file_id}"),
        query = list(
            oauth_token = oauth_token
        )
    )

    save_api(response, user_id)
}

#' Create folder on put.io
#' 
#' @export
putio_create_folder <- function(user_id = NULL, name, parent_id = 0,
                                oauth_token = putio_get_token(user_id)) {
    response <- httr::POST(
        putio_api_url,
        path = "v2/files/create-folder",
        query = list(oauth_token = oauth_token),
        body = list(
            parent_id = as.character(parent_id),
            name = name
        )
    )
    
    save_api(response, user_id)
    
}

#' Rename a file on put.io
#' 
#' @export
putio_files_rename <- function(user_id = NULL, file_id, new_name, 
                               oauth_token = putio_get_token(user_id)) {
    response <- httr::POST(
        putio_api_url,
        path = "v2/files/rename",
        query = list(oauth_token = oauth_token),
        body = list(
            file_id = as.character(file_id),
            name = new_name
        )
    )
    
    save_api(response, user_id)
}

#' Rename a file on put.io
#' 
#' @export
putio_files_move <- function(user_id = NULL, file_id, parent_id,
                             oauth_token = putio_get_token(user_id)) {
    response <- httr::POST(
        putio_api_url,
        path = "v2/files/move",
        query = list(oauth_token = oauth_token),
        body = list(
            file_ids = paste(file_id, collapse = ","),
            parent_id = as.character(parent_id)
        )
    )
    
    save_api(response, user_id)
}

#' Delete  a file on put.io
#' 
#' @export
putio_files_delete <- function(user_id = NULL, file_id, 
                               oauth_token = putio_get_token(user_id)) {
    response <- httr::POST(
        putio_api_url,
        path = "v2/files/delete",
        query = list(oauth_token = oauth_token),
        body = list(
            file_ids = paste(file_id, collapse = ",")
        )
    )
    
    save_api(response, user_id)
}

#' Search put.io with from:admin
#' 
#' @export
putio_search_admin <- function(query) {
    response <- httr::GET(
        putio_api_url,
        path = "v2/files/search",
        query = list(
            query = paste(query, "from:admin"),
            oauth_token = Sys.getenv("PUTIO_ADMIN_TOKEN")
        )
    )

    save_api(response)
}

#' Link to users putio file
#' 
#' @export
putio_file_link <- function(file_id) {
    glue("https://app.put.io/files/{file_id}")
}