season_UI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tags$div(
        id = "season",
        shiny::uiOutput(ns("seasons_tabset")),
        shiny::uiOutput(ns("seasons_episode")),
        shinyjs::hidden(
            shiny::actionButton(ns("download_season"), class = "add-season btn-primary", 
                                label = "Get Season")
        )
    )
}

episode_line <- function(episode, name, duration, putio_user_file_id, status, ...) {
    duration_friendly <- glue("{duration} MIN")
    shiny::fluidRow(
        shiny::column(1, shiny::tags$span(class = "number", episode)),
        shiny::column(6, shiny::tags$span(class = "name", name)),
        shiny::column(2, shiny::tags$span(class = "duration pull-right", 
                                          duration_friendly)),
        shiny::column(
            3,
            switch(status,
                ready = {
                    shiny::tags$a(
                        type = "button",
                        target = "_blank",
                        class = "btn btn-primary putio-file-link",
                        href = busboyr::putio_file_link(putio_user_file_id),
                        shiny::tags$span(class = "glyphicon glyphicon-play")
                    )
                },
                requested = {
                    shiny::tags$span("Finding", class = "blink")
                },
                downloading = {
                    shiny::tags$span("In Transfers", class = "blink")
                }
            )
        )
    )
}

season <- function(input, output, session, putio_user_id, selected_imdb_id) {
    refresh_season_episode <- reactive_trigger()
    
    output$seasons_tabset <- shiny::renderUI({
        seasons <- busboyr::imdb_dataset_seasons(selected_imdb_id())
        tabset_id <- session$ns("seasons_tabset")
        tabset_fn <- purrr::partial(shiny::tabsetPanel, id = tabset_id)

        tab_panels <- purrr::map(seasons, function(season) {
            shiny::tabPanel(
                glue("Season {season}"),
                value = as.character(season)
            )
        })
        
        do.call(tabset_fn, tab_panels)
    })
    
    selected_season <- shiny::reactive({
        shiny::validate(
            shiny::need(input$seasons_tabset, message = "Finding seasons...")
        )
        as.integer(input$seasons_tabset)
    })
    
    refresh_channel <- shiny::reactive({
        imdb_id <- imdb_id()
        season <- season()
        glue("{imdb_id} - season {season}")
    })
    
    output$seasons_episode <- shiny::renderUI({
        # Binding refresh
        refresh_season_episode$depend()

        # This will be triggered from sqs when a download
        # is complete
        input$refresh
        
        episodes <- busboyr::title_status(putio_user_id(), selected_imdb_id(), 
                                          selected_season()) %>% 
            dplyr::collect()
        
        if (any(is.na(episodes$status))) {
            shinyjs::show("download_season")
        } else {
            shinyjs::hide("download_season")
        }
        
        episodes %>% 
            busboyr::pre_pmap_int64() %>% 
            purrr::pmap(episode_line)
    })
    
    shiny::observeEvent(input$download_season, {
        season_request_id <- busboyr::request_season(
            shiny::isolate(putio_user_id()), 
            shiny::isolate(selected_imdb_id()), 
            shiny::isolate(selected_season()))
        
        # Refreshing episodes to display the status update
        refresh_season_episode$trigger()
        
        busboyr::task_create("process_season_request", list(
            season_request_id = season_request_id
        ))
    })
}