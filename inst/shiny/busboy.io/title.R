title <- function(input, output, session, putio_user_id, selected_imdb_id) {
    imdb_title <- shiny::reactive({
        shiny::validate(
            shiny::need(selected_imdb_id(), 
                        message = "Please search and select a title first.")
        )
        
        busboyr::get_title(selected_imdb_id())
    })

    output$header <- shiny::renderUI({
        page_header_title(imdb_title()$Title)
    })
    
    output$poster <- shiny::renderUI({
        shiny::tags$img(
            src = imdb_title()$Poster,
            class = "poster"
        )
    })

    output$plot <- shiny::renderText(imdb_title()$Plot)
    
    shiny::observe({
        if (imdb_title()$Type == "series") {
            shinyjs::show(selector = "#season")
        }
    })
    
    shiny::callModule(season, "season", putio_user_id, selected_imdb_id)
}

title_UI <- function(id) {
    ns <- shiny::NS(id)
    
    shiny::tags$div(
        id = "title",
        shiny::uiOutput(ns("header")),
        shiny::fluidRow(
            shiny::column(
                3,
                shiny::uiOutput(ns("poster"))
            ),
            shiny::column(
                9,
                shiny::fluidRow(
                    shiny::column(
                        12,
                        shiny::textOutput(ns("plot"))
                    )
                ),
                shiny::fluidRow(
                    shiny::column(
                        12,
                        shinyjs::hidden(
                            shiny::tags$div(id = "season",
                                            season_UI(ns("season")))
                        )
                    )
                )
            )
        )
    )
}
