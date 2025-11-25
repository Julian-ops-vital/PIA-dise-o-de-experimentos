# app.R
# Archivo principal que arma la aplicación Shiny

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly) 
library(DT)

# Cargar interfaz y servidor
source("ui.R")
source("server.R")

# Lanzar aplicación
shinyApp(ui = ui, server = server)
