library(shiny)
library(shinydashboard)

library(plotly)
setwd("~/Documents/compstat2016/todas_las_tareas")


dashboardPage(
  
  # Dashboard header
  dashboardHeader(title = "Tareas de estadistica Computacional"),
  
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Tarea 01", tabName = "tarea_1", icon = icon("unlock ")),
      menuItem("Tarea 02", tabName = "tarea_2", icon = icon("unlock")),
      menuItem("Tarea 04", tabName = "tarea_3", icon = icon("unlock")),
      menuItem("Tarea 05", tabName = "tarea_4", icon = icon("unlock"))
      
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
                  )
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
                )
              ),
              fluidRow(
                column(6, plotOutput("montecarlo_plot",height = 500)),
                column(6, dataTableOutput("tabla_mc"))
                
            )
    ),
    # Third menu tab
    tabItem("tarea_3", 
            fluidRow(
              
              sidebarPanel(
                selectInput("dep", "Variable dependiente", c("Disney" = "dis", "Proyeccion" = "pro")),
                sliderInput("a", " Parametro a priori a ~ Unif ", min=1, max=10, value=c(1,10)),
                sliderInput("b", "Parametro a priori b ~ Unif", min=1, max=10, value=c(1,10)),
                sliderInput("sigma", "Parametro a priori sigma ~ Unif",  min=1, max=10, value=c(1,10))
              )),
            fluidRow(
              column(6, plotlyOutput("scatter_variables_p2",height = 500)),
              column(6, dataTableOutput("tabla_regresion"))
            )),
            
          
    tabItem("tarea_4", 
       fluidRow(
         sidebarPanel(
           titlePanel("Parametros a priori"),
           selectInput("dep_p2", "Variable dependiente", c("Disney" = "dis", "Proyeccion" = "pro")),
           sliderInput("a_p2", " Parametro a priori a ~ Unif ", min=1, max=10, value=c(1,10)),
           sliderInput("b_p2", "Parametro a priori b ~ Unif", min=1, max=10, value=c(1,10)),
           sliderInput("sigma_p2", "Parametro a priori sigma ~ Unif",  min=1, max=10, value=c(1,10))
         ),
         sidebarPanel(
           titlePanel("Parametros, MCMC"),
         numericInput("n_cadena", "cadenas a simular", value=1, min=1, max=10, step=1),
         sliderInput("l_cadena", "longitud de cadenas", min=1000, max=10000, value=1000),
         actionButton("run_mcmc_b", "Calcula MCMC")
         
         )
         
       ),
       fluidRow(
         tabsetPanel(type="tabs",
                     tabPanel("Datos", 
                              fluidRow(
                                column(6, plotlyOutput("scatter_variables_p2",height = 500)),
                                column(6, dataTableOutput("tabla_regresion"))
                               )
                     ),
                     tabPanel("Distribuciones aPriori",
                              fluidRow(
                                #column(4, plotOutput("histo_a")),
                                #column(4, plotOutput("histo_a")),
                                #column(4, plotOutput("histo_a")),
                                #column(4, plotOutput("histo_a"))
                              )
                     ),
                     
                     tabPanel("Parámetros de la regresión",
                              fluidRow(
                                column(12, dataTableOutput("cadenasMCMC"))
                              )),
                     tabPanel("Convergencia de MCMC's", 
                              plotOutput("pConvergencia"))
         
        )
       )
       
    )
    
    
    
    ) # end tab items
  ) # end dash body
)