library(prioritizr)
library(shiny)
library(rgdal)
library(shinyjs)

source("to_source.R")


pu <- readOGR(dsn = "D:/Work/R/prioritizrshiny_data", layer = "sim_pu_polygons_add_feat", stringsAsFactors = FALSE)

input <- list()
input$cost_col <- "COST"
input$feat_col <- c("SPEC1", "SPEC2", "SPEC3", "SPEC4", "SPEC5")
input$objective <- "min_set"
input$tar_type <- "rel_tar"
input$glob_tar <- "global"
input$tar_all <- 0.2

p <- problem(pu, features = input$feat_col, cost_column = input$cost_col)
