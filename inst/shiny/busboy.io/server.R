function(input, output, session) {
    session$userData$reactive <- shiny::reactiveValues()
    session$userData$refresh <- shiny::reactiveValues()
    
    event_button_setup(input, output, session, navbar_id = "busboy_navbar")
    
    # Close window after n minutes to prevent resource hogging
    max_session_time <- 10 * 60 * 1000 # 10 minutes
    shinyjs::runjs(glue("limit_session_time({max_session_time})"))

    putio_user_id <- shiny::callModule(account, "account")
    selected_imdb_id <- shiny::callModule(search, "search", putio_user_id)
    shiny::callModule(title, "title", putio_user_id, selected_imdb_id)
}