options(shiny.port = 8000, shiny.host = "0.0.0.0")

# Modules
source("account.R")
source("account-ui.R")
source("search.R")
source("title.R")
source("season.R")

# Utility
source("util.R")
source("util-ui.R")

# Shortcuts for most used functions
glue <- glue::glue
`%>%` <- dplyr::`%>%`