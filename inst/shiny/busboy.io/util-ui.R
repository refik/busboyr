page_header_title <- function(title) {
    shiny::div(class = "page-header", shiny::tags$h1(title))
}

extended_column <- function(middle_col, middle_offset = 0,
                            small_col = middle, small_offset = 0, ...) {
    class = glue("col-md-{middle_col}", "col-sm-{small_col}",  "col-md-offset-{middle_offset}", 
                 "col-sm-offset-{small_offset}", .sep = " ")
    
    shiny::tags$div(class = class, ...)
}

make_input_button <- function(tag, input_name, input_value) {
    shiny::tagAppendAttributes(
        tag,
        class = "input-button",
        `data-input-name` = input_name,
        `data-input-value` = input_value
    )
}
