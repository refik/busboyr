#' Creates a task for busboy.io workers
#' 
#' @export
create_task <- function(task_function, arguments) {
    logger <- get_logger()
    
    task_definition <- list(
        function_name = task_function,
        arguments = arguments
    )

    logger(glue(
        "Sending task for `{task_function}()` ",
        "with argument(s) {log_named_list(arguments)}."
    ))

    task_definition %>% 
        jsonlite::toJSON(auto_unbox = TRUE) %>% 
        as.character() %>% 
        aws.sqs::send_msg(Sys.getenv("TASK_QUEUE"), .) %>% 
        invisible()
}

#' Consumes a given task
#' 
#' @export
consume_task <- function(wait = NULL) {
    logger <- get_logger()
    sqs_message <- aws.sqs::receive_msg(Sys.getenv("TASK_QUEUE"), wait = wait)
    
    if (nrow(sqs_message) == 0) {
        logger("No message in task queue.")
        return()
    }
    
    message <- jsonlite::fromJSON(sqs_message$Body)
    logger(glue("Executing task `{message$function_name}()`."))
    
    # Calling the task function
    try(
        shiny::withLogErrors(
            do.call(message$function_name, 
                    message$arguments)
        )
    )
    
    logger("Task consumed. Deleting the handle.")
    aws.sqs::delete_msg(Sys.getenv("TASK_QUEUE"), sqs_message$ReceiptHandle)
}