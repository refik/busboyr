search <- function(input, output, session, putio_user_id) {
    # Setting up autofocus and search on enter for
    # search query box
    query_input <- session$ns("search_query")
    button_input <- session$ns("search_button")
    shinyjs::runjs(glue("setup_search_query('{query_input}', '{button_input}')"))
    
    search_result <- shiny::eventReactive(input$search_button, {
        # Making sure putio_user_id is obtainable
        putio_user_id()
        
        shiny::validate(
            shiny::need(input$search_query, 
                        message = "Please enter a title name to search")
        )
        
        result <- busboyr::omdb_search(input$search_query)
        
        shiny::validate(
            shiny::need(result$json$Search, message = "Couldn't find any title.")
        )
        
        result$json$Search %>% 
            # Decreasing the size of posters for search results
            dplyr::mutate(Poster = stringr::str_replace(Poster, "SX300", "SX150"))
    })
    
    output$search_result_ui <- shiny::renderUI({
        # Taking the first n, its mostly enough
        titles <- head(search_result(), 8)
        
        title_cards <- purrr::pmap(titles, title_search_card, 
                                   trigger_input = session$ns("selected_imdb_id"))
        
        do.call(shiny::fluidRow, title_cards)
    })
    
    shiny::reactive(input$selected_imdb_id)
}

title_search_card <- function(Title, Type, Year, imdbID, Poster, trigger_input, ...) {
    extended_column(
        middle_col = 3, 0, small_col = 6, 0,
        shiny::div(
            class = "thumbnail",
            shiny::div(
                class = "poster-background",
                if (Poster != "N/A") {
                    shiny::img(src = Poster, class = "poster")
                }
            ),
            shiny::div(
                class = "caption",
                shiny::tags$h4(Title, class = "title-info"),
                shiny::fluidRow(
                    shiny::column(
                        7,
                        shiny::tags$p(
                            class = "title-info",
                            paste(tools::toTitleCase(Type), Year, sep = ", "))
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
                            trigger_input_value = imdbID,
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
                shiny::textInput(ns("search_query"), NULL, width = "100%", 
                                 placeholder = "Search for a title")
            ),
            
            extended_column(
                middle_col = 1, 0, small_col = 2, 0, 
                shiny::actionButton(
                    ns("search_button"), 
                    shiny::HTML("<span class='glyphicon glyphicon-search' aria-hidden='true'></span>"), 
                    width = "100%",
                    class = "btn-primary"
                )
            )
        ),
        
        shiny::tags$div(
            id = "search",
            shiny::uiOutput(ns("search_result_ui"))
        )
    )
}