
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
get_exponential<- function (item, lambda) {
  return (log(1-item)/ (-1*lambda))
}
get_chi_squared<- function(experimental, real) {
  (experimental - real)^2 / real 
}
shinyServer(function(input, output) {

  output$trendPlot <- renderPlotly({
    uni <- runif(input$iterations, 0,1)
    experimental_exp <- sapply(uni, function(item)get_exponential(item,input$lambda))
    real_exp <-rexp(input$iterations,rate=input$lambda)
    chi<- sum(get_chi_squared(experimental_exp, real_exp))
    output$chi = renderText({paste("Prueba de bondad Chi cuadrado: ",toString(chi))})
    #hist(experimental_exp, breaks = input$iterations/10, col = 'darkgray', border = 'white')
    plot_ly(x = experimental_exp, type = "histogram", opacity = .3) %>%
      add_trace (x=real_exp, type = "histogram",opacity = .3, colors = "gray")
    
  })

})
