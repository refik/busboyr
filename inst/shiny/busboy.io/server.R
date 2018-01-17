function(input, output, session) {
    refresh <- reactive_trigger()
    
    shinyjs::runjs("setup_input_button()")
    
    # Limit session time to 10 minutes
    # shiny::reactiveTimer(10 * 60* 1000, )

    # Return values from modules
    user_id <- shiny::callModule(account, "account")
    search <- shiny::callModule(search, "search", user_id)
    title_id <- search$title_id
    search_query <- search$query
    season <- shiny::callModule(title, "title", user_id, title_id, refresh = refresh)
    
    shiny::observe({
        # Updating refresh reactive values
        shiny::validate(shiny::need(input$refresh, message = "Refresh key missing"))
        key <- stringr::str_extract(input$refresh, "^[^|]+(?=|[0-9]+$)")
        refresh$trigger(key)
    })
    
    navigate <- function(tab_value) {
        shiny::updateNavbarPage(
            session, 
            "navbar", 
            tab_value
        )
    }
    
    # If there is a change in title_id, navigate to title page
    shiny::observe({
        title_id()
        navigate("title")
    })
    
    # If there is a change in title_id, navigate to title page
    shiny::observe({
        user_id()
        if (shiny::isolate(input$navbar) == "account") {
            navigate("search")
        }
    })
    
    # If navbar is clicked and the page is changed, push the change to history and
    # add it as a query string
    shiny::observe({
        qs_list <- list(page = input$navbar)

        switch(input$navbar,
            search = {
                try(silent = TRUE, {
                    qs_list$query <- search_query() %>% 
                        stringr::str_replace_all(" ", "+")
                })
            },
            title = {
                try(silent = TRUE, {
                    qs_list$id <- title_id()
                })
                
                try(silent = TRUE, {
                    qs_list$season <- season()
                })
            }
        )

        shiny::updateQueryString(build_qs(qs_list), mode = "replace")
    })

}