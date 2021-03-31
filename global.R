list.of.packages <- c("shiny", "dplyr", "tidytransit", "ggplot2", "leaflet", "shinydashboard", "reshape2")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(dplyr)
library(tidytransit)
library(ggplot2)
library(leaflet)
library(shinydashboard)
library(reshape2)