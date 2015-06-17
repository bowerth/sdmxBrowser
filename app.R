## install.packages("ISOweek")
## devtools::install_github("rstudio/shinydashboard")
## shiny::runGitHub(repo = "sdmxBrowser", username = "bowerth")
## setwd(file.path(dbpath, "GitHub", "sdmxBrowser"))
## source("app.R")
## shinyApp(ui, server)

## app.R ##
library(shiny)
library(shinydashboard)
library(RJSDMX)
library(reshape2)
library(ISOweek)
library(shinyAce)

library(ggplot2)

library(xts)
library(dygraphs)

input <- NULL
output <- NULL

ui.sdmxBrowser.col <- c("#4F81BD", "#C0504D", "#9BBB59", "#8064A2", "#4BACC6", "#F79646")
ui.sdmxBrowser.maxyear <- as.numeric(format(Sys.time(), "%Y"))

## create list with flows by provider
ui.sdmxbrowser_provider <- getProviders()
## remove providers known to have issues
ui.sdmxbrowser_provider <- ui.sdmxbrowser_provider[!ui.sdmxbrowser_provider%in%c("OECD", "OECD_RESTR", "NBB", "ISTAT")]
ui.sdmxbrowser_provider <- ui.sdmxbrowser_provider[!ui.sdmxbrowser_provider%in%c("ILO", "BIS", "WB")]

.sdmxbrowser_dimensions_all <- reactive({
    sdmxbrowser_dimensions_all<- names(getDimensions(input$sdmxbrowser_provider,
                                                     input$sdmxbrowser_flow))
    return(sdmxbrowser_dimensions_all)
})

## flow list
load(file = file.path("data_init", "sdmxBrowser.rda"))

header <- dashboardHeader(title = "sdmxBrowser")

sidebar <- dashboardSidebar(
  ## disable = TRUE,
  sidebarMenu(

    menuItem("Browse Flows", tabName = "browseflows", icon = icon("th"))
    ## ,
    # menuItem("Compare Flows", tabName = "compareflows", icon = icon("th"))
    )
  )

source(file.path("widgets", "browseflows_ui.R"))
# source(file.path("widgets", "compareflows_ui.R"))

body <- dashboardBody(
  tabItems(

    tabItem(tabName = "browseflows",
            fluidRow(
              browseflows.input,
              browseflows.output
              )
            )
    # ,
    # tabItem(tabName = "compareflows",
    #         ## h2("Widgets tab content")
    #         fluidRow(
    #           compareflows.input
    #           ,
    #           compareflows.output
    #           )
    #         )
    )

  )

ui <- dashboardPage(
  header,
  sidebar,
  body
  )

server <- function(input, output) {

  source(file.path("widgets", "browseflows_server.R"), local = TRUE)
  # source(file.path("widgets", "compareflows_server.R"), local = TRUE)

}

shinyApp(ui, server)
