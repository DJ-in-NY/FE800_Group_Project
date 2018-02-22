#server.R

# Loading/Installing required packages
# if (!require("pacman")) install.packages("pacman")
# pacman::p_load(tm, SnowballC, wordcloud, DT, syuzhet, dplyr, shiny, shinydashboard, plotly, quantmod, rsconnect,
#                DT, sde, dygraphs)

library(tm)
library(SnowballC)
library(wordcloud)
library(DT)
library(syuzhet)
library(dplyr)
library(shiny)
library(shinydashboard)
library(plotly)
library(quantmod)
library(rsconnect)
library(sde)
library(dygraphs)

##########
f <- list(
  family = "Arial, Helvetica, sans-serif",
  size = 16,
  color = "black"
)
x <- list(
  title = "Date",
  titlefont = f
)
y <- list(
  title = "Daily Return in %",
  titlefont = f
)
##########

shinyServer(function(input, output) {
  #Reactive expression for data acquisition (inculding data cleaning for creating plotly plot)
  dataInput = reactive({
    #First Data
    first_data <- getSymbols(input$symb1, 
                             from = input$dates[1],
                             to = input$dates[2],
                             auto.assign = FALSE)
    
    #Second Data
    second_data <- getSymbols(input$symb2,
                              from = input$dates[1],
                              to = input$dates[2],
                              auto.assign = FALSE)
    
    
    
    data_1 = data.frame(date=index(first_data), coredata(first_data))
    data_2 = data.frame(date=index(second_data), coredata(second_data))
    
    daily_return1 = 100 * (diff(data_1[,7])/data_1[,7][-length(data_1[,7])])
    daily_return2 = 100 * (diff(data_2[,7])/data_2[,7][-length(data_2[,7])])
    daily_return11 = c(0, daily_return1)
    daily_return22 = c(0, daily_return2)
    
    combined_data = data.frame(data_1[,1], daily_return11, daily_return22)
  }) 
  
  ############################################
  #Reactive expression for creating simulation
  dataInput_3 <- reactive({
    first_data <- getSymbols(input$symb1, 
                             from = input$dates[1],
                             to = input$dates[2],
                             auto.assign = FALSE)
    
    data_3 = data.frame(first_data)
  })
  
  ############################################
  
  #Reactive expression for creating DYGRAPHS
  dataInput_4 <- reactive({
    first_data <- getSymbols(input$symb1, 
                             from = input$dates[1],
                             to = input$dates[2],
                             auto.assign = FALSE)
    
    data_4 = data.frame(first_data)
    data_4[, 5] = NULL 
    data_4[, 5] = NULL 
    colnames(data_4) = c('Open', 'High', 'Low', 'Close')
    data_5 = as.matrix(data_4)
  })
  
  ############################################  
  #Reactive expression for data acquisition (for chartseries function, for symbol1)
  dataInput_1 = reactive({
    #First Data
    first_data_1 <- getSymbols(input$symb1, 
                               from = input$dates[1],
                               to = input$dates[2],
                               auto.assign = FALSE)
  })
  
  #Reactive expression for data acquisition (for chartseries function, for symbol2)
  dataInput_2 = reactive({
    #Second Data
    second_data_1 <- getSymbols(input$symb2,
                                from = input$dates[1],
                                to = input$dates[2],
                                auto.assign = FALSE)
  })
  
  #Reactive expression to determine whether to include BBand
  bband_1 <- reactive({
    if (input$BBband) return(addBBands())
    NULL
    
  })
  
  #Reactive expression to determine whether to include MACD
  macd_1 <- reactive({
    if (input$MACD) return(addMACD())
    NULL
    
  })
  
  # #Reactive expression to determine whether to include Volumn
  # volumn_1 <- reactive({
  #   if (input$Volumn) return(addVo())
  #   NULL
  # })
  
  #Main plot using Plotly
  output$plot <- renderPlotly({
    plot_ly(dataInput(), x = ~ dataInput()[,1], y = ~ dataInput()[,2], name = input$symb1, type = 'scatter', mode = 'lines') %>% 
      add_trace(y = ~ dataInput()[,3], name = input$symb2, mode = 'lines') %>% 
      layout(xaxis = x, yaxis = y) %>% 
      layout(paper_bgcolor='rgb(240, 240, 240)')
    
  })
  
  #Second plot using chartseries from quantmod
  output$plot_1 <- renderPlot({
    chartSeries(dataInput_1(),type = "candlesticks", theme=chartTheme('white'), TA=c(addVo(), bband_1(), macd_1()), name = input$symb1)
    
  })
  output$plot_2 <- renderPlot({
    chartSeries(dataInput_2(),type = "candlesticks", theme=chartTheme('white'), TA=c(addVo(), bband_1(), macd_1()), name = input$symb2)
    
  })
  
  output$plot_3 <- renderPlot({
    ###calculate sigma
    price <- dataInput_3()[,6]
    n = length(price)
    ret <- log(price[-1]/price[-n])
    vol <- sd(ret) * sqrt(250) * 100
    ###calculate expected return
    return1 = sum(ret)
    ###parameters for GBM
    mu=return1; sigma=vol/100; P0=price[length(price)]; T = as.numeric(input$duration) ##1 month
    nt=100; n=as.numeric(input$number1)
    ###Generate nt trajectories
    dt=T/n; t=seq(0,T,by=dt)
    X=matrix(rep(0,length(t)*nt), nrow=nt)
    for (i in 1:nt) {X[i,]= GBM(x=P0,r=mu,sigma=sigma,T=T/365,N=n)}
    
    ##Plot
    ymax=max(X); ymin=min(X) #bounds for simulated prices
    par(bg = '#F0F0F0')
    plot(t,X[1,],t='l',ylim=c(ymin, ymax), col=1,
         ylab="Price P(t)",xlab="time t")
    
    for(i in 2:nt){lines(t,X[i,], t='l',ylim=c(ymin, ymax),col=i)}
    
  })
  
  output$plot_4 <- renderDygraph({
    dygraph(dataInput_4()) %>% 
      dyRangeSelector() %>% 
      dyCandlestick()
    
  })
  
  
  #Max Box
  output$maxBox <- renderInfoBox({
    ###calculate sigma
    price <- dataInput_3()[,6]
    n = length(price)
    ret <- log(price[-1]/price[-n])
    vol <- sd(ret) * sqrt(250) * 100
    ###calculate expected return
    return1 = sum(ret)
    ###parameters for GBM
    mu=return1; sigma=vol/100; P0=price[length(price)]; T = as.numeric(input$duration) ##1 month
    nt=100; n=as.numeric(input$number1)
    ###Generate nt trajectories
    dt=T/n; t=seq(0,T,by=dt)
    X=matrix(rep(0,length(t)*nt), nrow=nt)
    for (i in 1:nt) {X[i,]= GBM(x=P0,r=mu,sigma=sigma,T=T/365,N=n)}
    infoBox(
      "Max Return", paste0(round(100*(sort(X,decreasing = TRUE)[1]-price[length(price)])/price[length(price)],digits = 3), '%'), icon = icon("smile-o"),
      color = "green")
  })
  
  #Min Box
  output$minBox <- renderInfoBox({
    ###calculate sigma
    price <- dataInput_3()[,6]
    n = length(price)
    ret <- log(price[-1]/price[-n])
    vol <- sd(ret) * sqrt(250) * 100
    ###calculate expected return
    return1 = sum(ret)
    ###parameters for GBM
    mu=return1; sigma=vol/100; P0=price[length(price)]; T = as.numeric(input$duration) ##1 month
    nt=100; n=as.numeric(input$number1)
    ###Generate nt trajectories
    dt=T/n; t=seq(0,T,by=dt)
    X=matrix(rep(0,length(t)*nt), nrow=nt)
    for (i in 1:nt) {X[i,]= GBM(x=P0,r=mu,sigma=sigma,T=T/365,N=n)}
    infoBox(
      "Min Return", paste0(round(100*(sort(X)[1]-price[length(price)])/price[length(price)],digits = 3), '%'), icon = icon("frown-o"),
      color = "red")
  })
  
  #Avg Box
  output$avgBox <- renderInfoBox({
    ###calculate sigma
    price <- dataInput_3()[,6]
    n = length(price)
    ret <- log(price[-1]/price[-n])
    vol <- sd(ret) * sqrt(250) * 100
    ###calculate expected return
    return1 = sum(ret)
    ###parameters for GBM
    mu=return1; sigma=vol/100; P0=price[length(price)]; T = as.numeric(input$duration) ##1 month
    nt=100; n=as.numeric(input$number1)
    ###Generate nt trajectories
    dt=T/n; t=seq(0,T,by=dt)
    X=matrix(rep(0,length(t)*nt), nrow=nt)
    for (i in 1:nt) {X[i,]= GBM(x=P0,r=mu,sigma=sigma,T=T/365,N=n)}
    #
    simple_return = (X[,dim(X)[2]]-X[,1])/X[,1]
    avg_return = mean(simple_return)
    #
    infoBox(
      'Avg Return', paste0(round(avg_return*100,digits = 3), '%'), icon = icon("meh-o"),
      color = "yellow")
  })
  
  
  #Var Box
  output$varBox <- renderInfoBox({
    ###calculate sigma
    price <- dataInput_3()[,6]
    n = length(price)
    ret <- log(price[-1]/price[-n])
    vol <- sd(ret) * sqrt(250) * 100
    ###calculate expected return
    return1 = sum(ret)
    ###parameters for GBM
    mu=return1; sigma=vol/100; P0=price[length(price)]; T = as.numeric(input$duration) ##1 month
    nt=100; n=as.numeric(input$number1)
    ###Generate nt trajectories
    dt=T/n; t=seq(0,T,by=dt)
    X=matrix(rep(0,length(t)*nt), nrow=nt)
    for (i in 1:nt) {X[i,]= GBM(x=P0,r=mu,sigma=sigma,T=T/365,N=n)}
    #
    simple_return = (X[,dim(X)[2]]-X[,1])/X[,1]
    sorted = sort(simple_return)
    var95 = sorted[0.01*(100-input$slider)*length(sorted)]
    #
    infoBox(
      paste0("VaR", input$slider), paste0(round(var95*100,digits = 3), '%'), icon = icon("usd"),
      color = "yellow")
  })
  
  # #Bid Box
  # output$bid = renderInfoBox({
  #   test1 = print(summary_stocks %>% filter(ticker_name == input$symb1 %>% dplyr::select(earnings_date)))
  #   infoBox(test1, icon = icon("usd"), color = "yellow")
  #   
  #   
  # })
  # 
  
  
  #Show data using Datatable
  output$table = DT::renderDataTable({
    ###calculate sigma
    price <- dataInput_3()[,6]
    n = length(price)
    ret <- log(price[-1]/price[-n])
    vol <- sd(ret) * sqrt(250) * 100
    ###calculate expected return
    return1 = sum(ret)
    ###parameters for GBM
    mu=return1; sigma=vol/100; P0=price[length(price)]; T = as.numeric(input$duration) ##1 month
    nt=100; n=as.numeric(input$number1)
    ###Generate nt trajectories
    dt=T/n; t=seq(0,T,by=dt)
    X=matrix(rep(0,length(t)*nt), nrow=nt)
    for (i in 1:nt) {X[i,]= GBM(x=P0,r=mu,sigma=sigma,T=T/365,N=n)}
    datatable(X, rownames = FALSE)
  })
  
  #Shaw key financial indicators
  output$table1 = DT::renderDataTable({
    TABLE1 = summary_stocks %>% filter(ticker_name == input$symb1)
    TABLE1[1:5]
  })
  
  #Shaw key financial indicators
  output$table2 = DT::renderDataTable({
    TABLE1 = summary_stocks %>% filter(ticker_name == input$symb1)
    TABLE1[6:13]
  })
  
  #Shaw key financial indicators
  output$table3 = DT::renderDataTable({
    TABLE1 = summary_stocks %>% filter(ticker_name == input$symb1)
    TABLE1[14:20]
  })
  
})
