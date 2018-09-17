#' Consume queues in a loop
#' 
#' @export
consume_loop <- function(queue = c("task", "callback", "both")) {
    logger <- get_logger()
    
    fn <- list(
        task = consume_task,
        callback = consume_callback,
        both = function() {
            logger("Consuming both queues in turn for interactive testing")
            consume_task(wait = 1)
            consume_callback(wait = 1)
            logger("Sleeping for 2 seconds.")
            Sys.sleep(5)
        }
    )[[match.arg(queue)]]

    repeat fn()
}