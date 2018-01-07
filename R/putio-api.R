putio_api_url <- "https://api.put.io"

#' Get oauth token from putio user id
#' 
#' @export
putio_get_token <- memoise::memoise(function(putio_user_id) {
    user_id <- as.character(putio_user_id)
    get_table("api") %>% 
        dplyr::filter(hostname == "api.put.io", path == "v2/account/info", 
                      sql("response#>'{info,user_id}'") == !!user_id) %>% 
        filter_last() %>% 
        lax_select("query->>'oauth_token'") %>% 
        dplyr::pull()
})

#' put.io auth url for busboy
#' 
#' @export
putio_oauth_redirect_url <- function(redirect_url = "http://busboy.io") {
    httr::modify_url(
        putio_api_url,
        path = "v2/oauth2/authenticate",
        query = list(
            client_id = "3140",
            response_type = "token",
            redirect_uri = redirect_url
        )
    )
}

#' put.io account information
#' 
#' @export
putio_account_info <- function(putio_user_id = NULL, 
                               oauth_token = putio_get_token(putio_user_id)) {
    response <- httr::GET(
        putio_api_url,
        path = "v2/account/info",
        query = list(oauth_token = oauth_token)
    )
    
    cache_api_response(response)
}

#' Get information about a transfer
#' 
#' @export
putio_get_transfer <- function(putio_user_id, putio_user_download_id,
                               oauth_token = putio_get_token(putio_user_id)) {
    response <- httr::GET(
        putio_api_url,
        path = glue("v2/transfers/{putio_user_download_id}"),
        query = list(oauth_token = oauth_token)
    )
    
    cache_api_response(response)
}

#' Add transfer to put.io
#' 
#' @export
putio_add_transfer <- function(putio_user_id = NULL, source, save_parent_id = 0, 
                               callback_uuid = NULL,
                               oauth_token = putio_get_token(putio_user_id)) {
    if (!is.null(callback_uuid)) {
        callback_url <- paste0(
            "https://sqs.us-east-1.amazonaws.com/", 
            "575677476286/putio_transfer_complete",
            "?Action=SendMessage&MessageBody=", callback_uuid
        )
    } else {
        callback_url <- "http://example.com/placeholder"
    }
    
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
    
    cache_api_response(response)
}

#' Get users files
#' 
#' @export
putio_files_list <- function(putio_user_id = NULL, parent_id = 0,
                             oauth_token = putio_get_token(putio_user_id)) {
    response <- httr::GET(
        putio_api_url,
        path = "v2/files/list",
        query = list(
            parent_id = parent_id,
            oauth_token = oauth_token
        )
    )
    
    cache_api_response(response)
}

#' Create folder on put.io
#' 
#' @export
putio_create_folder <- function(putio_user_id = NULL, name, parent_id = 0,
                                oauth_token = putio_get_token(putio_user_id)) {
    response <- httr::POST(
        putio_api_url,
        path = "v2/files/create-folder",
        query = list(oauth_token = oauth_token),
        body = list(
            parent_id = as.character(parent_id),
            name = name
        )
    )
    
    cache_api_response(response)
    
}

#' Rename a file on put.io
#' 
#' @export
putio_files_rename <- function(putio_user_id = NULL, user_file_id, new_name, 
                               oauth_token = putio_get_token(putio_user_id)) {
    response <- httr::POST(
        putio_api_url,
        path = "v2/files/rename",
        query = list(oauth_token = oauth_token),
        body = list(
            file_id = as.character(user_file_id),
            name = new_name
        )
    )
    
    cache_api_response(response)
}

#' Rename a file on put.io
#' 
#' @export
putio_files_move <- function(putio_user_id = NULL, user_file_id, parent_id,
                             oauth_token = putio_get_token(putio_user_id)) {
    response <- httr::POST(
        putio_api_url,
        path = "v2/files/move",
        query = list(oauth_token = oauth_token),
        body = list(
            file_ids = paste(user_file_id, collapse = ","),
            parent_id = as.character(parent_id)
        )
    )
    
    cache_api_response(response)
}

#' Delete  a file on put.io
#' 
#' @export
putio_files_delete <- function(putio_user_id = NULL, user_file_id, 
                               oauth_token = putio_get_token(putio_user_id)) {
    response <- httr::POST(
        putio_api_url,
        path = "v2/files/delete",
        query = list(oauth_token = oauth_token),
        body = list(
            file_ids = paste(user_file_id, collapse = ",")
        )
    )
    
    cache_api_response(response)
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

    cache_api_response(response)
}

#' Link to users putio file
#' 
#' @export
putio_file_link <- function(putio_user_file_id) {
    glue("https://app.put.io/files/{putio_user_file_id}")
}