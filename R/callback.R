callback_queue_name <- "putio_transfer_complete"

#' Consumes transfer complete tasks from putio
#' 
#' @export
consume_callback <- function(wait = NULL) {
    logger <- get_logger("consume_callback")
    sqs_message <- aws.sqs::receive_msg(callback_queue_name, wait = wait)
    
    if (nrow(sqs_message) == 0) {
        logger("No message in callback queue.")
        return()
    }
    
    uuid <- sqs_message$Body
    
    # Get intended download
    download_id <- get_table("download") %>% 
        dplyr::filter(uuid == !!uuid) %>% 
        dplyr::pull(id)
    
    if (length(download_id) == 0) {
        logger("download_id for uuid:{uuid} not found, skipping callback.")
    } else {
        create_task("create_file", list(download_id = download_id))
    }

    logger("Deleting callback message.")
    aws.sqs::delete_msg(callback_queue_name, sqs_message$ReceiptHandle)
}

#' Notify user about updates
#' 
#' @export
refresh_shiny <- function(user_id) {
    logger <- get_logger()
    
    session_uuid <- get_table("session") %>% 
        dplyr::filter(user_id == !!user_id, is.na(ended_at)) %>%
        filter_last() %>% 
        dplyr::pull(uuid)
    
    if (length(session_uuid) == 1) {
        queue_name <- paste("session", session_uuid, sep = "-")
        logger("Sending refresh signal to users shiny session.")
        aws.sqs::send_msg(queue_name, "title-season-refresh")
    }
}

#' Transfer complete callback url for put.io
#' 
#' @export
callback_url <- function(uuid) {
    httr::modify_url(
        "https://sqs.us-east-1.amazonaws.com",
         path = glue("575677476286/{callback_queue_name}"),
         query = list(
             Action = "SendMessage",
             MessageBody = uuid
         )
    )
}