make_event_button <- function(tag, tab_name = "", trigger_input_name, trigger_input_value) {
    shiny::tagAppendAttributes(
        tag,
        class = "event-button",
        `data-trigger-input-name` = trigger_input_name,
        `data-trigger-input-value` = trigger_input_value,
        `data-open-tab` = tab_name
    )
}

event_button_setup <- function(input, output, session, navbar_id) {
    shinyjs::runjs("setup_event_button()")
    
    shiny::observeEvent(input$open_tab, {
        # Stripping the timestamp
        tab_name <- stringr::str_extract(input$open_tab, ".+(?=/[0-9]+)")
        
        shiny::updateNavbarPage(
            session, 
            navbar_id, 
            tab_name
        )
    })
}