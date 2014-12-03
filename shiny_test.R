library(ggplot2)
library(reshape2)
library(grid)
library(scales)
library(shiny)

source("shinyAutoCast.R")
source("plot.autocast.R")
load("dat.RData")

shinyAutoCast(autoList[[1]], outfile="~/Desktop/shiny_test.RData")

