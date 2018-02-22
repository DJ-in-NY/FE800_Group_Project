#ui.R

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


shinyUI(dashboardPage(
  dashboardHeader(title = 'U.S. Equity Research', titleWidth = 350,
                  dropdownMenu(type = 'messages',
                    messageItem(
                      from = 'Trading Support',
                      message = 'AAPL Market Sell: 100 Shares at $145.67'
                    ),
                    messageItem(
                      from = 'Price Alert',
                      message = 'AA is up 5.09% to $36.36 so far today'
                    ),
                    messageItem(
                      from = 'Bloomberg Terminal',
                      message = 'JPM earnings call'
                    )
                  )),
  
  dashboardSidebar(width = 350,
  
  #Sidebar user panel
  sidebarUserPanel('FE800 TEAM VIXXX', image = 'https://upload.wikimedia.org/wikipedia/commons/thumb/9/93/NY_Stock_Exchange_logo.svg/200px-NY_Stock_Exchange_logo.svg.png'),
                   
  #helpText
  helpText("Select stock(s) to examine. Information will be collected from Yahoo finance."),
                   
  sidebarMenu(
  
  #Additional Menu Item
  menuItem('Equity Research', tabName = 'equity_research', icon = icon('search')
    ),
  
  #First Menu Item
  menuItem('Comparison', tabName = 'pairs_trading', icon = icon('balance-scale')
          ),
  
  #Second Menu Item
  menuItem('Simulation', tabName = 'simulation', icon = icon('line-chart')
          ),
  
  #Third Menu Item
  menuItem('Simulation Data', tabName = 'data', icon = icon('database')
           ),
  
  #Equity Research Widgets
  textInput("symb1", "Symbol 1 (Main)", "XLU"),
  textInput("symb2", "Symbol 2", "XLK"),
  dateRangeInput("dates","Date range", start = "2017-01-01",end = as.character(Sys.Date())),
  checkboxInput("BBband", "Display Bollinger Band", value = FALSE),
  checkboxInput("MACD", "Display MACD", value = FALSE),
  #Simulation Widgets
  textInput('number1', 'Number of Simulations to Run', '100'),
  textInput('duration', 'Duration (days)', '25'),
  sliderInput("slider", "Confidence Level (VAR)", min = 90, max = 99, step = 9, value = 95)
  )
  ),
  
#Body content  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    tabItems(
      tabItem(tabName = 'pairs_trading',
                     plotlyOutput("plot", height = '550'), 
                     splitLayout(plotOutput("plot_1"), plotOutput("plot_2"))
                     ),
      tabItem(tabName = 'simulation', 
              fluidRow(infoBoxOutput("maxBox"),
                       infoBoxOutput("minBox"),
                       infoBoxOutput("avgBox"),
                       infoBoxOutput("varBox")),
              # h4('Monte Carlo Simulation with Geometric Brownian Motion'),
              h4('S(t) = S0 * exp[(Mu-Sigma^2/2)*t + Sigma*Wt]'),
              fluidRow(plotOutput('plot_3', height = '550'))
              ),
      tabItem(tabName = 'data', fluidRow(box(DT::dataTableOutput('table'), width = 12))
              ),
      tabItem(tabName = 'equity_research', 
              # fluidRow(infoBoxOutput('bid')),
              dygraphOutput('plot_4', height = '700'),
              br(),
              fluidRow(box(DT::dataTableOutput('table1'), width = 12)),
              fluidRow(box(DT::dataTableOutput('table2'), width = 12)),
              fluidRow(box(DT::dataTableOutput('table3'), width = 12))
              )
  
    ) 
  )

  )
 )
