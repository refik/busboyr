search <- function(input, output, session, user_id) {
    # Setting up autofocus and search on enter for
    # search query box
    query_input <- session$ns("query")
    button_input <- session$ns("button")
    shinyjs::runjs(glue("setup_search_query('{query_input}', '{button_input}')"))
    
    search_result <- shiny::eventReactive(input$button, {
        # Making sure putio_user_id is obtainable
        user_id()
        
        shiny::validate(
            shiny::need(input$query, 
                        message = "Please enter a title name to search")
        )
        
        result <- busboyr::search_title(input$query)
        
        shiny::validate(
            shiny::need(!is.null(result), message = "Couldn't find any title.")
        )
        
        result %>% 
            # Decreasing the size of posters for search results
            dplyr::mutate(poster = stringr::str_replace(poster, "SX300", "SX150"))
    })
    
    output$result <- shiny::renderUI({
        # Taking the first n, its mostly enough
        titles <- head(search_result(), 8)
        
        title_cards <- purrr::pmap(titles, title_search_card, 
                                   trigger_input = session$ns("selected_imdb_id"))
        
        do.call(shiny::fluidRow, title_cards)
    })
    
    shiny::reactive(input$selected_imdb_id)
}

title_search_card <- function(name, type, year, id, poster, trigger_input, ...) {
    extended_column(
        middle_col = 3, 0, small_col = 6, 0,
        shiny::div(
            class = "thumbnail",
            shiny::div(
                class = "poster-background",
                if (poster != "N/A") {
                    shiny::img(src = poster, class = "poster")
                }
            ),
            shiny::div(
                class = "caption",
                shiny::tags$h4(name, class = "title-info"),
                shiny::fluidRow(
                    shiny::column(
                        7,
                        shiny::tags$p(
                            class = "title-info",
                            paste(tools::toTitleCase(type), year, sep = ", "))
                    ),
                    shiny::column(
                        5,
                        make_event_button(
                            shiny::tags$button(
                                type = "button",
                                class = "btn btn-primary",
                                "See"
                            ),
                            trigger_input_name = trigger_input,
                            trigger_input_value = id,
                            tab_name = "title"
                        )
                    )
                )
            )
        )
    )
}

search_UI <- function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
        shiny::fluidRow(
            extended_column(
                middle_col = 5, 3, small_col = 6, 2,
                shiny::textInput(ns("query"), NULL, width = "100%", 
                                 placeholder = "Search for a title")
            ),
            
            extended_column(
                middle_col = 1, 0, small_col = 2, 0, 
                shiny::actionButton(
                    ns("button"), 
                    shiny::HTML("<span class='glyphicon glyphicon-search' aria-hidden='true'></span>"), 
                    width = "100%",
                    class = "btn-primary"
                )
            )
        ),
        
        shiny::tags$div(
            id = "search",
            shiny::uiOutput(ns("result"))
        )
    )
}