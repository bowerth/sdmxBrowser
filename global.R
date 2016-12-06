library(RJSDMX)
library(reshape2)
library(ISOweek)
library(shinyAce)

library(ggplot2)

ui.sdmxBrowser.col <- c("#4F81BD", "#C0504D", "#9BBB59", "#8064A2", "#4BACC6", "#F79646")

ui.sdmxBrowser.maxyear <- 2015
ui.sdmxbrowser_provider <- getProviders()
ui.sdmxbrowser_provider <- ui.sdmxbrowser_provider[!ui.sdmxbrowser_provider%in%c("OECD_RESTR", "NBB", "ISTAT")]

## flow list
load(file = file.path("data_init", "sdmxBrowser.rda"))

