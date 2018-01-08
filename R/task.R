task_queue_name <- "busboy_task"

#' Creates a task for busboy.io workers
#' 
#' @export
create_task <- function(task_function, arguments) {
    logger <- get_logger()
    
    task_definition <- list(
        function_name = task_function,
        arguments = arguments
    )
    
    task_definition %>% 
        jsonlite::toJSON(auto_unbox = TRUE) %>% 
        as.character() %>% 
        aws.sqs::send_msg(task_queue_name, .)
}

#' Consumes a given task
#' 
#' @export
consume_task <- function(wait = NULL) {
    logger <- get_logger("consume_task")
    sqs_message <- aws.sqs::receive_msg(task_queue_name, wait = wait)
    
    if (nrow(sqs_message) == 0) {
        logger("No message in task queue.")
        return()
    }
    
    message_json <- jsonlite::fromJSON(sqs_message$Body)
    task_function <- get(message_json$function_name)
    arguments <- message_json$arguments

    # Calling the task function
    do.call(task_function, arguments)
    
    logger("Task completed successfully. Deleting the handle.")
    aws.sqs::delete_msg(task_queue_name, sqs_message$ReceiptHandle)
}