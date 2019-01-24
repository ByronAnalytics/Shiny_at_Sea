# Global.R is source code for shiny apps, executed before ui.r or server.r
# Here it is used to ensure required libraries are installed and loaded. 
# Libraries with major dependancies are loaded into the namespace, while
# those requiring one/two functions are accessed explicitly with library::function()

options(stringsAsFactors=F, shiny.reactlog=TRUE)

if(!require("shiny")) install.packages("shiny"); library(shiny);
if(!require("shinyjs")) install.packages("shinyjs"); library(shinyjs);
if(!require("shinyBS")) install.packages("shinyBS"); library(shinyBS);
if(!require("ggplot2")) install.packages("ggplot2"); library(ggplot2);
if(!require("DT")) install.packages("DT"); library(DT);

# For readNSmooth.R
if(!require("oce")) install.packages("oce")
if(!require("stringr")) install.packages("stringr")

# For custom point graphics (like boat)
#if(!require("Cairo")) install.packages("Cairo"); library(Cairo);
#if(!require("grid")) install.packages("grid"); library(grid);


#Source scripts in source folder
for (nm in list.files("source", pattern = "\\.[RrSsQq]$")) {
    #if(trace) cat(nm,":")           
    source(file.path("source", nm))
   # if(trace) cat("\n")
}
