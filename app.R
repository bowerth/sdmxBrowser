## install.packages("ISOweek")
## devtools::install_github("rstudio/shinydashboard")
## shiny::runGitHub(repo = "sdmxBrowser", username = "bowerth")
## setwd(file.path(dbpath, "GitHub", "sdmxBrowser"))
## source("app.R")
## shinyApp(ui, server)

## app.R ##
library(shiny)
library(shinydashboard)

source("global.R")

header <- dashboardHeader(title = "sdmxBrowser")

sidebar <- dashboardSidebar(
    ## disable = TRUE,
    sidebarMenu(

        menuItem("Browse Flows", tabName = "browseflows", icon = icon("th"))
        ## ,
        ## menuItem("Compare Flows", tabName = "compareflows", icon = icon("th"))

    )
)

source(file.path("widgets", "browseflows_ui.R"))
## source(file.path("widgets", "compareflows_ui.R"))

body <- dashboardBody(
    tabItems(

        tabItem(tabName = "browseflows",
                fluidRow(
                    browseflows.input,
                    browseflows.output)
                )

        ## ,
        ## tabItem(tabName = "compareflows",
        ##         ## h2("Widgets tab content")
        ##         fluidRow(
        ##           compareflows.input
        ##           ,
        ##           compareflows.output
        ##           )
        ##         )

    )
)

ui <- dashboardPage(
    header,
    sidebar,
    body
)

server <- function(input, output) {

    source(file.path("widgets", "browseflows_server.R"), local = TRUE)
    ## source(file.path("widgets", "compareflows_server.R"), local = TRUE)

}

shinyApp(ui, server)
