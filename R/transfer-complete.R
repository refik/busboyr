transfer_complete_queue_name <- "putio_transfer_complete"

#' Consumes transfer complete tasks from putio
#' 
#' @export
transfer_complete_consume <- function(wait = NULL) {
    logger <- get_logger("transfer_complete_consume")
    sqs_message <- aws.sqs::receive_msg(transfer_complete_queue_name, wait = wait)
    
    if (nrow(sqs_message) == 0) {
        message("No message in transfer complete queue.")
        return()
    }
    
    callback_uuid <- sqs_message$Body
    
    # TODO: Fix this
    expected_callback <- get_record("putio_transfer_complete_callback", callback_uuid)

    organize_download(expected_callback$putio_user_id, 
                      expected_callback$putio_user_download_id)
    
    aws.sqs::delete_msg(transfer_complete_queue_name, sqs_message$ReceiptHandle)
    
    transfer_complete_refresh_shiny(expected_callback$putio_user_id)
}

#' Notify user about updates
#' 
#' @export
transfer_complete_refresh_shiny <- function(putio_user_id) {
    logger <- get_logger()
    
    active_session_uuid <- dplyr::tbl(db_pool(), "session") %>% 
        dplyr::filter(putio_user_id == !!putio_user_id, is.na(ended_at)) %>%
        dplyr::arrange(desc(id)) %>% 
        head(1) %>% dplyr::pull(session_uuid)
    
    assert_that(length(active_session_uuid) == 1)
    
    queue_name <- paste("session", active_session_uuid, sep = "-")
    aws.sqs::send_msg(queue_name, "title-season-refresh")
}