search <- function(input, output, session, user_id) {
    # Setting up autofocus and search on enter for
    # search query box
    query_input <- session$ns("query")
    button_input <- session$ns("button")
    shinyjs::runjs(glue("setup_search_bar('{query_input}', '{button_input}')"))
    
    search_result <- shiny::reactive({
        # Attach reactive button event
        input$button
        
        # Making sure putio_user_id is obtainable
        user_id()
        
        # Getting search query
        query <- shiny::isolate(input$query)
        
        # If it is NULL, look for the query string
        if (is.null(query)) {
            query <- shiny::getQueryString()$query
        }
        
        # If nothing is searched yet, display users files if they have any
        if (!is.null(query) && query == "") {
            result <- busboyr::get_user_title(user_id())
            if (!is.null(result)) {
                return(result)
            }
        }
        
        shiny::validate(
            shiny::need(query, 
                        message = "Please enter a title name to search")
        )

        # This is only necessary for query string
        query <- query %>% 
            trimws() %>% 
            stringr::str_replace_all("\\+", " ")
        
        result <- busboyr::search_title(query)
        
        shiny::validate(
            shiny::need(!is.null(result), message = "Couldn't find any title.")
        )
        
        result %>% 
            # Decreasing the size of posters for search results
            dplyr::mutate(poster = stringr::str_replace(poster, "SX300", "SX150")) %>% 
            dplyr::filter(!is.na(poster))
    })
    
    output$result <- shiny::renderUI({
        # Taking the first n, its mostly enough
        titles <- search_result()
        
        title_cards <- purrr::pmap(titles, title_search_card, 
                                   input_name = session$ns("title_id"))
        
        do.call(shiny::fluidRow, title_cards)
    })
    
    title_id <- shiny::reactive({
        title_id <- input$title_id
        
        if (is.null(title_id)) {
            title_id <- shiny::getQueryString()$id
        }
        
        shiny::validate(
            shiny::need(title_id, 
                        message = "Please search and select a title first.")
        )
        
        as.integer(title_id)
    })
    
    list(
        title_id = title_id,
        query = shiny::reactive({
            search_result()
            query <- shiny::isolate(input$query)
            shiny::validate(shiny::need(query, label = "input$query"))
            query
        })
    )
}

title_search_card <- function(name, type, year, id, poster, input_name, ...) {
    extended_column(
        middle_col = 3, 0, small_col = 6, 0,
        shiny::div(
            class = "thumbnail",
            shiny::div(
                class = "poster-background",
                shiny::img(src = poster, class = "poster")
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
                        make_input_button(
                            shiny::tags$button(
                                type = "button",
                                class = "btn btn-primary",
                                "See"
                            ),
                            input_name = input_name,
                            input_value = id
                        )
                    )
                )
            )
        )
    )
}

search_UI <- function(id, query) {
    ns <- shiny::NS(id)
    
    shiny::tags$div(
        id = "search",
        shiny::fluidRow(
            class = "bar",
            extended_column(
                middle_col = 5, 3, small_col = 6, 2,
                shiny::textInput(ns("query"), label = NULL, value = query, width = "100%", 
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
            class = "result",
            shiny::uiOutput(ns("result"))
        )
    )
}