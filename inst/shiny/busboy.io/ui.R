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
        ')
    ),
    shinyjs::useShinyjs(),
    shiny::navbarPage(
        title = shiny::tags$img(src = "logo.png"), 
        theme = "paper_theme.css", selected = "search", 
        id = "busboy_navbar", windowTitle = "Busboy", fluid = FALSE,
        shiny::tabPanel("Account", value = "account",
                        account_UI("account")),
        shiny::tabPanel("Search", value = "search",
                        search_UI("search")),
        shiny::tabPanel("Title", value = "title",
                        title_UI("title"))
    )
)