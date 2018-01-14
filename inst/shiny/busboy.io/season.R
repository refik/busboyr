season_UI <- function(id) {
    ns <- shiny::NS(id)
    shiny::tags$div(
        id = "season",
        shiny::uiOutput(ns("season_tab")),
        shiny::uiOutput(ns("episode")),
        shinyjs::hidden(
            shiny::actionButton(ns("download"), class = "get-season btn-primary", 
                                label = "Get Season")
        )
    )
}

episode_line <- function(episode, name, duration_minute, file_id, status, ...) {
    duration_friendly <- glue("{duration_minute} MIN")
    shiny::fluidRow(
        shiny::column(1, shiny::tags$span(class = "number", episode)),
        shiny::column(6, shiny::tags$span(class = "name", name)),
        shiny::column(2, shiny::tags$span(class = "duration pull-right", 
                                          duration_friendly)),
        shiny::column(
            3,
            switch(status,
                has_file = {
                    shiny::tags$a(
                        type = "button",
                        target = "_blank",
                        class = "btn btn-primary putio-file-link",
                        href = busboyr::putio_file_link(file_id),
                        shiny::tags$span(class = "glyphicon glyphicon-play")
                    )
                },
                has_request = {
                    shiny::tags$span("Finding", class = "blink")
                },
                has_download = {
                    shiny::tags$span("In Transfers", class = "blink")
                }
            )
        )
    )
}

season <- function(input, output, session, user_id, title_id) {
    refresh_episode <- reactive_trigger()
    
    output$season_tab <- shiny::renderUI({
        seasons <- busboyr::title_season(title_id())
        tabset_id <- session$ns("season_tab")
        message(shiny::getQueryString()$season)
        tabset_fn <- purrr::partial(shiny::tabsetPanel, id = tabset_id, 
                                    selected = shiny::getQueryString()$season)

        tab_panels <- purrr::map(seasons, function(season) {
            shiny::tabPanel(
                glue("Season {season}"),
                value = as.character(season)
            )
        })
        
        do.call(tabset_fn, tab_panels)
    })
    
    season <- shiny::reactive({
        season <- input$season_tab
        
        shiny::validate(
            shiny::need(season, message = "Finding seasons...")
        )
        
        as.integer(season)
    })
    
    output$episode <- shiny::renderUI({
        # Binding refresh
        refresh_episode$depend()

        # This will be triggered from sqs when a download
        # is complete
        input$refresh
        
        episodes <- busboyr::season_status(user_id(), title_id(), 
                                           season()) %>% 
            dplyr::collect()
        
        if (any(is.na(episodes$status))) {
            shinyjs::show("download")
        } else {
            shinyjs::hide("download")
        }
        
        episodes %>% 
            busboyr::pre_pmap_int64() %>% 
            purrr::pmap(episode_line)
    })
    
    shiny::observeEvent(input$download, {
        busboyr::create_request(
            shiny::isolate(user_id()), 
            shiny::isolate(title_id()), 
            season = shiny::isolate(season())
        )
        
        # Refreshing episodes to display the status update
        refresh_episode$trigger()
    })
    
    season
}