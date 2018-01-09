title <- function(input, output, session, user_id, title_id) {
    title <- shiny::reactive({
        shiny::validate(
            shiny::need(title_id(), 
                        message = "Please search and select a title first.")
        )
        
        busboyr::get_title(title_id())
    })

    output$header <- shiny::renderUI({
        page_header_title(title()$name)
    })
    
    output$poster <- shiny::renderUI({
        shiny::tags$img(
            src = title()$poster,
            class = "poster"
        )
    })

    output$plot <- shiny::renderText(title()$plot)
    
    shiny::observe({
        if (title()$type == "series") {
            shinyjs::show(selector = "#title_season")
        }
    })
    
    shiny::callModule(season, "season", user_id, title_id)
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
                            shiny::tags$div(id = "title_season",
                                            season_UI(ns("season")))
                        )
                    )
                )
            )
        )
    )
}
