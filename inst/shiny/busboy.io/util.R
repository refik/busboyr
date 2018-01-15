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
    rv <- shiny::reactiveValues()
    list(
        depend = function(name) {
            rv[[name]]
            invisible()
        },
        trigger = function(name) {
            rv[[name]] <- Sys.time()
        }
    )
}

# Build a query string form list ?page=search&query=breaking
build_qs <- function(qs_list) {
    paste0("?", paste(names(qs_list), qs_list, sep = "=", collapse = "&"))
}

# Update query string with a single parameter
update_qs <- function(param, value) {
    qs_list <- shiny::getQueryString()
    qs_param <- qs_list[[param]]
    redundant <- !is.null(qs_param) && qs_param == value
    
    if (!redundant) {
        qs_list[[param]] <- value
        shiny::updateQueryString(build_qs(qs_list))
    }
}
