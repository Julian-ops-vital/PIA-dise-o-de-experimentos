# app.R
# Archivo principal que arma la aplicación Shiny

library(shiny)
library(ggplot2)
library(dplyr)

# Cargar interfaz y servidor
source("interfaz.R")
source("server.R")

# Lanzar aplicación
shinyApp(ui = ui, server = server)
