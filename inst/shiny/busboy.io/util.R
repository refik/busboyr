user_data <- function(session, var) {
    session$userData$reactive[[var]]
}

trigger_refresh <- function(session, channel) {
    session_id <- user_data(session, "session_id")
    channel <- glue("{session_id} | {channel}")
    session$userData$refresh[[channel]] <- as.integer(Sys.time())
}

bind_refresh <- function(session, channel) {
    session_id <- user_data(session, "session_id")
    channel <- glue("{session_id} | {channel}")
    session$userData$refresh[[channel]]
}

# Concept taken from
# https://github.com/daattali/advanced-shiny/tree/master/reactive-trigger
reactive_trigger <- function() {
    rv <- shiny::reactiveValues(a = 0)
    list(
        depend = function() {
            rv$a
            invisible()
        },
        trigger = function() {
            rv$a <- shiny::isolate(rv$a + 1)
        }
    )
}

