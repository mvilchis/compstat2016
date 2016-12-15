library(shiny)
library(shinydashboard)
library(plotly)
setwd("~/Documents/compstat2016/")

shinyServer(function(input, output, session) { 
  ###########  Tarea 1  ######################
  get_exponential<- function (item, lambda) {
    return (log(1-item)/ (-1*lambda))
  }
  
  get_chi_squared<- function(experimental, real) {
    (experimental - real)^2 / real 
  }

    output$downloadData <- downloadHandler(
      filename = 'Muestra.csv',
      content = function(file) {
        write.csv(renderDataTable({output$tabla}), file)
      })
    
    output$trendPlot <- renderPlotly({
      distribucion_uniforme <- runif(input$iterations, 0,1)
      distribucion_exponencial <- sapply(distribucion_uniforme, function(item)get_exponential(item,input$lambda))
      real_exp <-rexp(input$iterations,rate=input$lambda)
      min_x = min(distribucion_exponencial)
      max_x = max(distribucion_exponencial)
      chi<- sum(get_chi_squared(distribucion_exponencial, real_exp))/1000
      output$chi = renderText({paste("Prueba de bondad Chi cuadrado: ",toString(chi))})
      output$tabla = renderDataTable({
        cbind(distribucion_exponencial,distribucion_uniforme)
      },  options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
      plot_ly(x = distribucion_exponencial, 
              xbins = list(
                end = max_x, 
                size = (max_x-min_x)/input$nbins, 
                start = min_x
              ),
              autobinx = FALSE,
              type = "histogram", opacity = .3, name = 'Experimental') %>%
        add_trace (x=real_exp, type = "histogram",opacity = .3, colors = "gray", name ='Real') %>%
        layout(barmode = "overlay")
    })
    
  ############ Fin Tarea 1 #########################
    
    ############# Tarea 2 ############################
    trapecio <- function(n,a, b,FUN){
      dim <- length(a)
      x <- seq(a[1], b[1], (b[1]-a[1])/n)
      if(dim == 1){
        fi <- sapply(x, FUN)
      } else{
        fi <- sapply(x, function(x){
          trapezoid(n, a[-1], b[-1], function(y) FUN(c(x,y)))
        })
      }
      return(((b[1]-a[1])/(2*n))*sum(fi[-1]+fi[-(n+1)]))
    }
    output$montecarlo_plot_dis <- renderPlot({
      funcion_value  <- eval(parse(text = input$fun))
      
      nn <- input$n_sim
      from <- input$inf
      to <- input$sup
      to2 <- to - from
      u1 <- runif(nn, from, to)
      aux <- funcion_value(u1)
      aux[is.nan(aux)] <- 0
      curve(funcion_value, from=from, to=to)
      aux <- funcion_value(seq(from, to))
      aux[is.nan(aux)] <- 0
      mn <- min(aux)
      mx <- max(aux)
      points(u1, runif(input$n_sim, mn, mx))
    })
    
    calcula_montecarlo <- function(){
      
      funcion_value  <- reactive({
        texto <- paste("aux <-", input$fun)
        eval(parse(text = texto))
        aux
      })
      
      minimo <-{}
      maximo <-  {}
      trapecio_list <-  {}
      montecarlo_list <- {}
      
      for (i in seq(1,input$n_sim)){
        
        uniform <- runif(1000,input$inf, input$sup)
        y <- sapply(uniform, funcion_value())
        montecarlo_item  <- (input$sup - input$inf)*mean(y)
        montecarlo_list <- c(montecarlo_list, montecarlo_item)
        mc_var <- sd(montecarlo_list)
        
        error <-mc_var*qnorm(1-input$confianza) /sqrt(length(montecarlo_list))
        min_item <- montecarlo_item #- error
        max_item <- montecarlo_item+error
        minimo <- c(minimo, min_item)
        maximo <- c(maximo, max_item)
        #trapecio_list <- trapecio(i ,min,max,funcion_value())
      }
      minimo[1] <-montecarlo_list[1]
      maximo[1] <- montecarlo_list[1]
      
      val_item <- integrate(funcion_value(), input$inf,input$sup)
      val_vec <- (rep.int(val_item[1],input$n_sim))
      xs <- c(1:input$n_sim)
      output$montecarlo_plot <- renderPlot({
        plot(xs, val_vec, type="l",ylim=c(min(minimo),max(maximo)),col=1)
        lines(xs,montecarlo_list,type = "l",col=2)
        lines(xs,maximo,type = "l",col=3)
        lines(xs,minimo,type = "l",col=3)
      })
      return (cbind(montecarlo_list , minimo,maximo))
      
    }
    output$tabla_mc <- renderDataTable({
      calcula_montecarlo()
    },  options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
    ################## Fin Tarea 2 #######################
    ################### Tarea 3 ########################
    output$tabla_regresion <- renderDataTable({
      read.csv('./data/train.csv')
    },  options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
    tabla <- data.frame(read.csv('./data/train.csv'))
    output$scatter_variables <- renderPlotly({
      if(input$dep == 'dis') {
        plot_ly() %>% 
          add_trace(data=tabla, x = ~X, y = ~Y, type="scatter", mode="markers") 
      }else {
        plot_ly() %>% 
          add_trace(data=tabla, x = ~Y, y = ~X, type="scatter", mode="markers") 
      }
      
    })
    
    output$histo_a <- renderPlotly({
      n <- dim(tabla)[1]
      dist_a <- runif(n, min=input$a[1], max = input$a[2])
      max_x <- input$a[2]
      min_x <- input$a[1]
      plot_ly(x = dist_a, 
              xbins = list(
                end = max_x, 
                size = (max_x-min_x)/input$nbins, 
                start = min_x
              ),
              autobinx = FALSE,
              type = "histogram", opacity = .3, name = 'Histograma a')
    })
    
    get_priori <- function(n_items, min,max){
      runif(n_items, min=min, max = max)
    }
    
    output$histo_sigma <- renderPlotly({
      n <- dim(tabla)[1]
      max_x <- input$sigma[2]
      min_x <- input$sigma[1]
      distribucion <- get_priori(n, min_x, max_x)
      
      plot_ly(x = distribucion, 
              xbins = list(
                end = max_x, 
                size = (max_x-min_x)/input$nbins, 
                start = min_x
              ),
              autobinx = FALSE,
              type = "histogram", opacity = .3,  marker = list(  color="#66d9ff"))%>%
        layout(                        
          title = "Histograma sigma",
          xaxis = list(
            title = " Parametro sigma",     
            showgrid = T),      
          yaxis = list(           
            title = "Unif(sigma)"))
    })
    
    output$histo_b <- renderPlotly({
      n <- dim(tabla)[1]
      max_x <- input$b[2]
      min_x <- input$b[1]
      distribucion <- get_priori(n, min_x, max_x)
      
      plot_ly(x = distribucion, 
              xbins = list(
                end = max_x, 
                size = (max_x-min_x)/input$nbins, 
                start = min_x
              ),
              autobinx = FALSE,
              type = "histogram", opacity = .3, name = 'Histograma b', marker = list(color="#0099cc"))%>%
        layout(                        
          title = "Histograma b",
          xaxis = list(
            title = " Parametro b",     
            showgrid = T),      
          yaxis = list(           
            title = "Unif(b)"))
    })
    
    output$histo_a <- renderPlotly({
      n <- dim(tabla)[1]
      max_x <- input$a[2]
      min_x <- input$a[1]
      distribucion <- get_priori(n, min_x, max_x)
      
      plot_ly(x = distribucion, 
              xbins = list(
                end = max_x, 
                size = (max_x-min_x)/input$nbins, 
                start = min_x
              ),
              autobinx = FALSE,
              type = "histogram", opacity = .3, marker = list( color= "#006080"))%>%
        layout(                        
          title = "Histograma a",
          xaxis = list(
            title = " Parametro a",     
            showgrid = T),      
          yaxis = list(           
            title = "Unif(a)"))
    })
    
    ################### Fin tarea 3 ####################
    
    #################### Tarea 4 ,5,6,#######################
    Rcpp::sourceCpp("funciones.cpp")
    chain <- {}
    
    calcula_regresion <- function( update_mc =FALSE){
      if (!is.null(chain) & !update_mc) {
        return(chain)
      }
      if(input$dep != 'dis') {
        y_mc <- tabla$X
        x_mc <- tabla$Y
      }else {
        x_mc <- tabla$X
        y_mc <- tabla$Y
      }
      theta0 <- c(1,1,1)
      chain <- runMCMC(x=x_mc, y=y_mc, startValue=theta0, iterations=input$l_cadena)
      chain <-  data.frame(a=chain[,1], b=chain[,2], sd=chain[,3])
      for (i in 1:(input$n_cadena)){
        aux <- theta0 + round(10*runif(1))
        run_m_1 <- runMCMC(x=x_mc, y=y_mc, startValue=aux, iterations=input$l_cadena)
        run_m_1 <- data.frame(a=run_m_1[,1], b=run_m_1[,2], sd=run_m_1[,3])
        if (input$n_cadena >= 2) {
          chain <- cbind(chain, run_m_1)  
        }
      }
      return(chain)
    }
    df <- eventReactive(input$run_mcmc_b, {
      return (calcula_regresion(update_mc = TRUE))
    })
    
    output$cadenasMCMC <- renderDataTable({print(calcula_regresion())
      calcula_regresion()} ,options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
    
    
    output$posterior_a <- renderPlotly({
        chain <- calcula_regresion()
        mean_a <- mean(chain[,1])
        
        plot_ly(x = chain[,1], 
                type = "histogram", opacity = .3, marker = list( color= "#006080"))%>%
          layout(                        
            title = paste("Histograma posteriori a : ", mean_a, sep=),
            xaxis = list(
              title = " Parametro a",     
              showgrid = T),      
            yaxis = list(           
              title = "Posteriori(a)")) 
        
      })
    output$posterior_b <- renderPlotly({
      chain <- calcula_regresion()
      mean_b <- mean(chain[,2])
      
      plot_ly(x = chain[,2], 
              type = "histogram", opacity = .3, marker = list( color= "#006080"))%>%
        layout(                        
          title = paste("Histograma posteriori b: ", mean_b, sep=""),
          xaxis = list(
            title = " Parametro b",     
            showgrid = T),      
          yaxis = list(           
            title = "Posteriori(b)")) 
      
    })
    output$posterior_sigma <- renderPlotly({
      chain <- calcula_regresion()
      mean_sigma <- mean(chain[,3])
      dens_a <- density(chain[,3])
      
      plot_ly(x = chain[,3], 
              type = "histogram", opacity = .3, marker = list( color= "#006080"))%>%
        layout(                        
          title = paste("Histograma posteriori sigma", mean_sigma, sep = "\n"),
          xaxis = list(
            title = " Parametro sigma",     
            showgrid = T),      
          yaxis = list(           
            title = "Posteriori(a)")) 
    })
    
   
    
    
    
    #################### Fin tarea 4 ###################
  
    
    
    
  
}
)