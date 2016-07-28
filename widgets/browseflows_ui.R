

browseflows.input <- column(width = 4,
                            box(
                              ## width = 4,
                              width = NULL,
                              title = "Controls",
                              wellPanel(
                                uiOutput("uisB_query"),
                                ## actionButton("sdmxbrowser_querySendButton", "Send query"),
                                ## shinysky::actionButton("sdmxbrowser_querySendButton", "Submit Query", styleclass="success",icon = NULL, size = "large", block = TRUE),
                                actionButton("sdmxbrowser_querySendButton", "Submit Query")
                                 ,
                                  downloadButton('sdmxbrowser_download1', 'Download Data (csv)')

                                ## ,
                                ## helpText("Click button to retrieve values")
                                ## ,
                                ## downloadButton('download_sdmxBrowser', 'Download CSV')
                                ),
                              wellPanel(
                                h5("SDMX Query Builder"),
                                uiOutput("uisB_provider"),
                                uiOutput("uisB_flow"),
                                actionButton("sdmxbrowser_flow_updateButton", "Update Flows"),
                                wellPanel(
                                  uiOutput("uisB_dimensions"),
                                  uiOutput("uisB_dimensioncodes")
                                  ),
                                sliderInput(inputId = "sdmxbrowser_yearStartEnd",
                                            label = "Period:",
                                            min = 1970,
                                            max = ui.sdmxBrowser.maxyear,
                                            value = c(2000, ui.sdmxBrowser.maxyear)
                                            ,
                                            sep = ""
                                            ## pre = NULL,
                                            ## post = NULL,
                                            ## format = "#"
                                            )
                                )
                              )
)

browseflows.output <- column(width = 8,
                             box(title = "Parameters", verbatimTextOutput("summary1"), width = NULL, collapsible = TRUE)
                             ,
                             box(title = "Time series plot", plotOutput("plot1", height = 350), width = NULL, collapsible = TRUE)
                             ,
                             ## box(title = "Time series plot", dygraphOutput("plot2"), width = NULL, collapsible = TRUE)
                             ## ,
                             box(title = "Data table", dataTableOutput("table1"), width = NULL, collapsible = TRUE)
                             )
