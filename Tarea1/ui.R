
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)

shinyUI(fluidPage(

  # Application title
  titlePanel("Modelación de una distribución exponencial \n Con el método de la función inversa"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("lambda",
                  "Parámetro lambda",
                  min = 0,
                  max = 1,
                  value = .5),
      sliderInput("iterations",
                  "Número de muestras",
                  min= 1,
                  max= 1000,
                  value= 800)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotlyOutput("trendPlot"),
      textOutput("chi")
    )
  )
))
