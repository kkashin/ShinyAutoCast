### change working directory to downloaded folder
setwd("~/Desktop/Git/ShinyAutoCast/")

### install any libraries you don't have
#install.packages("ggplot2")
#install.packages("reshape2")
#install.packages("shiny")

### load libraries
library(ggplot2)
library(reshape2)
library(grid)
library(scales)
library(shiny)

### load a few functions
source("shinyAutoCast.R")
source("plot.autocast.R")

### load female data & run app
load("female.RData")
shinyAutoCast(autoListFemale, outfile="~/Desktop/shiny_female.RData")

### load male data & run app
load("male.RData")
shinyAutoCast(autoListMale, outfile="~/Desktop/shiny_male.RData")