compareflows.input <- column(width = 4,
                            box(
                              ## width = 4,
                              width = NULL,
                              title = "Controls",
                              wellPanel(
                                h5("SDMX Query List"),
                                aceEditor("compareflows_querylist", mode="r", height="100px", value="ABS.LF.0.1.1.1519.10.M\nABS.LF.0.2.1.1519.10.M")
                                ,
                                actionButton("compareflows_retrieve", "Retrieve flows")
                                ## ,
                                )
                              )
)

compareflows.output <- column(width = 8,
                              ## box(title = "Parameters", verbatimTextOutput("summary2"), width = NULL, collapsible = TRUE)
                              ## ,
                              box(title = "Flow table", dataTableOutput("table2"), width = NULL, collapsible = TRUE)
                             )
