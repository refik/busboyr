#' busboy.io Account page UI
#'
#' @export
account_UI <- function(id) {
    ns <- shiny::NS(id)
    
    shiny::tagList(
        shinyjs::hidden(
            shiny::tags$div(
                id = "signin_with_putio",
                page_header_title("Sign in"),
                shiny::actionButton(ns("signin"), "Sign in with put.io",
                                    class = "btn-warning btn-lg")
            )
        ),
        shinyjs::hidden(
            shiny::tags$div(
                id = "putio_user_information",
                page_header_title("Connected with put.io"),
                shiny::fluidRow(
                    shiny::column(
                        3,
                        shiny::strong("Username:")
                    ),
                    shiny::column(
                        9,
                        shiny::uiOutput(ns("username"))
                    )
                ),
                shiny::fluidRow(
                    shiny::column(
                        3,
                        shiny::strong("Email:")
                    ),
                    shiny::column(
                        9,
                        shiny::uiOutput(ns("mail"))
                    )
                ),
                shiny::fluidRow(
                    shiny::column(
                        3,
                        shiny::strong("Available space:")
                    ),
                    shiny::column(
                        9,
                        shiny::uiOutput(ns("disk_space"))
                    )
                ),
                shiny::fluidRow(
                    shiny::column(
                        4, 
                        shiny::actionButton(ns("logout"), "Log out",
                                            class = "btn-primary")                            
                    )
                ),
                shiny::fluidRow(
                    shiny::uiOutput(ns("support_email"))
                )
            )
        )
    )
}