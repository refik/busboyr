function(request) {
    query <- shiny::parseQueryString(request$QUERY_STRING)
    page <- query$page
    if (is.null(page)) {
        # Default page
        page <- "account"
    }
    
    shiny::tagList(
        tags$head(
            tags$script(src = "js.cookie.min.js"),
            tags$script(src = "busboy.js"),
            tags$link(rel = "stylesheet", type = "text/css", href = "busboy.css"),
            shiny::HTML('
                <link rel="apple-touch-icon" sizes="180x180" href="/apple-touch-icon.png">
                <link rel="icon" type="image/png" sizes="32x32" href="/favicon-32x32.png">
                <link rel="icon" type="image/png" sizes="16x16" href="/favicon-16x16.png">
                <link rel="manifest" href="/manifest.json">
                <link rel="mask-icon" href="/safari-pinned-tab.svg" color="#b53131">
                <meta name="theme-color" content="#ffffff">
            '),
            shiny::HTML('
                <script async src="https://s.tagove.com/a-69836/init.js"></script>
                <script async src="https://s.tagove.com/main.js"></script>
                <noscript><a href="https://www.tagove.com?welcome" title="live chat software">Website chat software</a></noscript>
            ')
        ),
        shinyjs::useShinyjs(),
        shiny::navbarPage(
            title = shiny::tags$img(src = "logo.png"), 
            theme = "paper_theme.css", selected = page, 
            id = "navbar", windowTitle = "Busboy", fluid = FALSE,
            shiny::tabPanel("Account", value = "account",
                            account_UI("account"),
                            shinyjs::hidden(
                                shiny::tags$pre(
                                    style = "margin-top: 2em",
                                    glue("Busboy Environment: {Sys.getenv('BUSBOY_ENV')}")
                                )
                            ),
                            if (Sys.getenv("BUSBOY_ENV") != "PROD") {
                                shiny::tagList(
                                    page_header_title("UI Request Object"),
                                    shiny::tags$pre(
                                        paste(utils::capture.output(print(as.list(request))), 
                                              collapse = "\n")
                                    )
                                )
                            }),
            shiny::tabPanel("Search", value = "search",
                            search_UI("search", query = query$query)),
            shiny::tabPanel("Title Information", value = "title",
                            title_UI("title"))
        )
    )
}
