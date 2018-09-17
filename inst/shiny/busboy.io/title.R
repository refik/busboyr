title <- function(input, output, session, user_id, title_id, 
                  refresh) {
    logger <- busboyr::get_logger("title", shiny = TRUE)
    
    title <- shiny::reactive({
        title <- busboyr::get_title(title_id())
        prefer_full_hd <- busboyr::get_full_hd(user_id(), title_id())
        shiny::updateCheckboxInput(session, "prefer_full_hd",
                                   value = prefer_full_hd)
        
        # Checking titles file on put.io
        file_id <- shiny::isolate(file_id())
        if (title$type == "movie" && !is.null(file_id)) {
            busboyr::create_task("check_title", list(
                user_id = user_id(),
                title_id = title_id()
            ))
        }
        
        logger(glue("Rendering title:{title_id()} for user:{user_id()}."))
        
        title
    })
    
    status <- shiny::reactive({
        refresh$depend(glue("title:{title_id()}"))
        busboyr::title_status(user_id(), title_id())
    })
    
    status_text <- shiny::reactive(status()$status)
    file_id <- shiny::reactive(status()$file_id)
    
    shiny::observeEvent(input$prefer_full_hd, {
        if (input$prefer_full_hd == TRUE) {
            busboyr::set_full_hd(user_id(), title_id())
        } else {
            busboyr::unset_full_hd(user_id(), title_id())
        }
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

    output$button <- shiny::renderUI({
        switch(status_text(),
            has_request = shiny::tags$span("Finding", class = "blink"),
            has_download = shiny::tags$span("In Transfers", class = "blink"),
            has_file = shiny::tags$a(
                type = "button",
                target = "_blank",
                class = "btn btn-primary play-movie",
                href = busboyr::putio_file_link(file_id()),
                shiny::tags$span(class = "glyphicon glyphicon-play")
            ),
            
            # The default is the download button
            shiny::actionButton(session$ns("download"), 
                                class = "btn-primary get-movie", 
                                label = "Get Movie")
        )
    })
    
    shiny::observeEvent(input$download, {
        busboyr::create_request(
            shiny::isolate(user_id()), 
            shiny::isolate(title_id())
        )
        
        refresh$trigger(glue("title:{title_id()}"))
    })
    
    output$plot <- shiny::renderText(title()$plot)
    
    shiny::observe({
        if (title()$type == "series") {
            shinyjs::hide("movie")
            shinyjs::show("season")
        } else {
            shinyjs::hide("season")
            shinyjs::show("movie")
        }
    })
    
    output$error <- shiny::renderUI({
        # To check if title is visible.
        tryCatch({
            title()
            shinyjs::hide("error")
            shinyjs::show(selector = "#title")
        }, error = function(e) {
            shinyjs::hide(selector = "#title")
            shinyjs::show("error")
            stop(e)
        })
    })
    
    season <- shiny::callModule(season, "season", user_id, title_id, refresh)
    
    # Return value is the season. Only if it is a series.
    shiny::reactive({
        shiny::validate(
            shiny::need(title()$type == "series", 
                        message = "Title has to be a series to have a season")
        )

        season()
    })
}

title_UI <- function(id) {
    ns <- shiny::NS(id)

    shiny::tagList(
        shiny::uiOutput(ns("error")),
        shinyjs::hidden(
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
                            class = "plot",
                            shiny::column(
                                12,
                                shiny::textOutput(ns("plot"))
                            )
                        ),
                        shiny::fluidRow(
                            class = "checkbox",
                            shiny::column(
                                12,
                                shiny::checkboxInput(ns("prefer_full_hd"), 
                                                     "Prefer Full HD")
                            )
                        ),
                        shiny::fluidRow(
                            shiny::column(
                                12,
                                shinyjs::hidden(
                                    shiny::tags$div(
                                        id = ns("season"),
                                        season_UI(ns("season"))
                                    )
                                ),
                                shinyjs::hidden(
                                    shiny::tags$div(
                                        id = ns("movie"),
                                        shiny::uiOutput(ns("button"))
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )

}
