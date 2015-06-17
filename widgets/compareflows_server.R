
compareflows_splitquery <- function(flow) {

  
  X <- strsplit(flow, split = "[.]")
  provider <- sapply(X, "[[", 1)
  flow <- sub(paste0(provider, "."), "", flow)
  result <- NULL
  result[["provider"]] <- provider
  result[["flow"]] <- flow
  return(result)
}

compareflows_maketable <- function(flowlist) {
  X <- strsplit(flowlist, split = "\n")
  X <- unlist(X)
  Y <- lapply(X, compareflows_splitquery)
  table2 <- data.frame(Provider = sapply(Y, "[[", 1),
                       Flow = sapply(Y, "[[", 2))
  return(table2)
}



output$table2 <- renderDataTable({
  compareflows_querylist <- input$compareflows_querylist
  ## compareflows_querylist <- "ABS.LF.0.1.1.1519.10.M\nABS.LF.0.2.1.1519.10.M"

  table2 <- compareflows_maketable(compareflows_querylist)
  return(table2)
})


data <- NULL
for (i in c(1:nrow(table2))) {
  ## i <- 1
  provider <- as.character(table2[["Provider"]][i])
  flow <- as.character(table2[["Flow"]][i])
  temp <- getSDMX(provider = provider,
                  id = flow,
                  start = "2000",
                  end = "2010")
  ## data[[paste0(provider, '.', flow)]] <- temp
  data[[provider]] <- temp
  return(data)
}

    
