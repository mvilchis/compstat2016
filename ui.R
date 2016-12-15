library(shiny)
library(shinydashboard)

library(plotly)
setwd("~/Documents/compstat2016/")


dashboardPage(
  
  # Dashboard header
  dashboardHeader(title = "Tareas de estadistica Computacional",titleWidth = 250),
  
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Tarea 01", tabName = "tarea_1", icon = icon("unlock")),
      menuItem("Tarea 02", tabName = "tarea_2", icon = icon("unlock")),
      menuItem("Tarea 04, 05 y 06", tabName = "tarea_4_5_6", icon = icon("unlock"))

    )
  ),
  
  #Body
  dashboardBody(
    
    tabItems(
      tabItem("tarea_1", 
              # First Row
              fluidRow(
                
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
                                value= 800),
                    sliderInput("nbins",
                                "Números de particiones",
                                min= 1,
                                max= 100,
                                value= 50),
                    downloadButton('downloadData', 'Descarga muestra')
                  ),
                  column(6, verbatimTextOutput("chi"))
              ),
              # Second Row
              fluidRow(
                
                column(6, plotlyOutput("trendPlot",height = 500)),
                column(6, dataTableOutput("tabla"))
                
              )
      ), # end vis item
      
      # Second menu tab
      tabItem("tarea_2", 
              fluidRow(
                sidebarPanel(
                  numericInput("inf","Limite inferior",value=0),
                  numericInput("sup","Limite Superior",value = 10),
                  numericInput("n_sim","Numero de simulaciones",value = 10),
                  textInput("fun","funcion",value = "function (x) x*3", placeholder = "Funcion a integrar"),
                  numericInput("confianza", "Intervalo de confianza ", min = 0, max = 1,value = 0.20)
                ),
                column(6, plotOutput("montecarlo_plot_dis",height = 400))
              ),
              fluidRow(
                column(6, plotOutput("montecarlo_plot",height = 400)),
                column(6, dataTableOutput("tabla_mc"))
                
              )
      ),
      
      tabItem("tarea_4_5_6", 
              fluidRow(
                sidebarPanel(
                  titlePanel("Parametros a priori"),
                  selectInput("dep", "Variable dependiente", c("X" = "dis", "Y" = "pro")),
                  sliderInput("a", " Parametro a priori a ~ Unif ", min=1, max=10, value=c(1,10)),
                  sliderInput("b", "Parametro a priori b ~ Unif", min=1, max=10, value=c(1,10)),
                  sliderInput("sigma", "Parametro a priori sigma ~ Unif",  min=1, max=10, value=c(1,10))
                ),
                sidebarPanel(
                  titlePanel("Parametros MCMC"),
                  numericInput("n_cadena", "cadenas a simular", value=1, min=1, max=10, step=1),
                  sliderInput("l_cadena", "longitud de cadenas", min=10000, max=100000, value=10000),
                  actionButton("run_mcmc_b", "Calcula MCMC")
                  
                )
              ),
              fluidRow(
                tabsetPanel(type="tabs",
                            tabPanel("Datos", 
                                     fluidRow(
                                       column(6, plotlyOutput("scatter_variables",height = 500)),
                                       column(6, dataTableOutput("tabla_regresion"))
                                       
                                     )
                                     ),
                            tabPanel("Cadenas",
                                     fluidRow(
                                       dataTableOutput("cadenasMCMC")
                                     )
                            ),
                          tabPanel("Histograma a priori", 
                                   fluidRow(
                                     column(4, plotlyOutput("histo_a",height = 500)),
                                     column(4, plotlyOutput("histo_b",height = 500)),
                                     column(4, plotlyOutput("histo_sigma",height = 500))
                           
                            )
                        ),
                        tabPanel("Histograma  posteriori", 
                                 fluidRow(
                                   column(4, plotlyOutput("posterior_a",height = 500)),
                                   column(4, plotlyOutput("posterior_b",height = 500)),
                                   column(4, plotlyOutput("posterior_sigma",height = 500))
                                   
                                 )
                                 
                        )
                        
              )
              )
              
      )
    
    ) # end tab items
  ) # end dash body
)