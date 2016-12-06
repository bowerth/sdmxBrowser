## shiny::runApp(file.path(dbpath, "GitHub", "sdmxBrowser"), launch.browser = FALSE, port = 3838)
## source(file.path(dbpath, "GitHub", "sdmxBrowser", "master.R"))


path <- file.path(dbpath, "GitHub", "sdmxBrowser")
setwd(path)
source("app.R")

shinyApp(ui, server)

## RJSDMX::getFlows("ABS")
## if (Sys.getenv("JAVA_HOME")!="") Sys.setenv(JAVA_HOME="")
## devtools::install_github("amattioc/SDMX", subdir = "RJSDMX")
## devtools::install("amattioc/SDMX", subdir = "RJSDMX")

## Sample Queries
## ECB:
##   ILM.M.U2.C.LT01.Z5.EUR (monthly)
##   ILM.W.U2.C.A010.Z5.Z0Z (weekly)
## ILO:
##   DF_YI_ALL_EMP_TEMP_ECO_OCU_NB.YI.DEU+ESP+FRA.A.*.EMP_TEMP_NB+EES_TEES_NB.ECO_ISIC4_TOTAL.OCU_ISCO08_TOTAL (annual)
## ABS:
##   LF.0.1.1.1519.10.M (monthly)

## require(RJSDMX)

## queryData <- getSDMX(provider = "ABS",
##                 id = "LF.0.1.1.1519.10.M")

## queryData <- RJSDMX::getSDMX(provider = "ILO",
##                 id = "DF_YI_ALL_EMP_TEMP_ECO_OCU_NB.YI.FRA+BEL.A.*.EMP_TEMP_NB+EES_TEES_NB.ECO_ISIC4_TOTAL.OCU_ISCO08_TOTAL")

## ## queryDataFreq <- attr(queryData[[1]], "FREQ")

## queryDataMelt <- lapply(queryData, changeDates)
## queryDataMelt <- sdmxdf(queryDataMelt)
## names(queryDataMelt) <- tolower(names(queryDataMelt))
## names(queryDataMelt) <- sub("id", "variable", names(queryDataMelt))
## names(queryDataMelt) <- sub("obs", "value", names(queryDataMelt))
## queryDataMelt$time <- as.Date(queryDataMelt$time)

##     data.d <- dcast(queryDataMelt, time ~ variable, value.var = "value", drop = FALSE)
##     ## h(data.d)

##     ## data.d$Year <- as.numeric(as.character(data.d$Year))
##     ## data.d$Year <- paste0(data.d$Year, '-01-01')

##     rownames(data.d) <- data.d$time
##     ## data.d <- data.d[, colnames(data.d)!="time"]
##     subset(data.d, select = names(data.d)[!names(data.d)=="time"])

##     data.d.xts <- as.xts(data.d, dateFormat = 'Date')

##     d1 <- dygraph(data.d.xts)
##     ## d1
