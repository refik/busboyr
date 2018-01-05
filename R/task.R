task_queue_name <- "busboy_task"

#' Creates a task for busboy.io workers
#' 
#' @export
task_create <- function(worker_function, arguments) {
    list(
        function_name = worker_function,
        arguments = arguments
    ) %>% 
        jsonlite::toJSON(auto_unbox = TRUE) %>% 
        as.character() %>% 
        aws.sqs::send_msg(task_queue_name, .)
}

#' Consumes a given task
#' 
#' @export
task_consume <- function(wait = NULL) {
    logger <- get_logger()
    sqs_message <- aws.sqs::receive_msg(task_queue_name, wait = wait)
    
    if (nrow(sqs_message) == 0) {
        logger("No message in task queue.")
        return()
    }
    
    message_json <- jsonlite::fromJSON(sqs_message$Body)
    task_function <- get(message_json$function_name)
    arguments <- message_json$arguments

    do.call(task_function, arguments)
    aws.sqs::delete_msg(task_queue_name, sqs_message$ReceiptHandle)
}