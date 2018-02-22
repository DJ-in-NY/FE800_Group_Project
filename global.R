# Loading/Installing required packages

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tm, SnowballC, wordcloud, DT, syuzhet, dplyr, shiny, shinydashboard, plotly, quantmod, rsconnect,
               DT, sde, dygraphs, ggplot2, data.table)


########## Obatining Financial Data on Top NASDAQ and NYSE conpanies ##########

top_nasdaq = read.csv('./Data/Top NASDAQ.csv')
str(top_nasdaq)
top_nasdaq$Symbol = as.character(top_nasdaq$Symbol)
nasdaq_ticker_list = top_nasdaq$Symbol

summary_nasdaq = read.csv('./Data/summary.csv')


# colnames(x = summary_nasdaq) = c('previous_close','bid','volume','52_wk_range','EPS','PE_ratio','earnings_date',
#                                  'beta', 'ex_div_date','avg_volumn','market_cap','open','div_yield','1yr_target',
#                                  'day_range','ask')

summary_nasdaq$ticker_name = nasdaq_ticker_list[1:300]
summary_nasdaq$company_name = top_nasdaq$Name[1:300]
summary_nasdaq$sector = top_nasdaq$Sector[1:300]
summary_nasdaq$industry = top_nasdaq$Industry[1:300]
# summary_nasdaq$latest_price = top_nasdaq$LastSale[1:105]
summary_nasdaq$EPS = as.character(summary_nasdaq$EPS)
summary_nasdaq$previous_close = as.character(summary_nasdaq$previous_close)
summary_nasdaq$PE_ratio = as.character(summary_nasdaq$PE_ratio)
summary_nasdaq$open_price = as.character(summary_nasdaq$open_price)


summary_nasdaq = summary_nasdaq[, c(17,18,19,20,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]

# # Testing: use dplyr to fetch data needed
# TSLA = summary_stocks %>% filter(ticker_name == 'BABA') 




###########################################################################################################

top_nyse = read.csv('./Data/Top NYSE.csv')
str(top_nyse)
top_nyse$Symbol = as.character(top_nyse$Symbol)
nyse_ticker_list = top_nyse$Symbol

summary_nyse = read.csv('./Data/summary1.csv')

# colnames(x = summary_nyse) = c('previous_close','bid','volume','52_wk_range','EPS','PE_ratio','earnings_date',
#                                'beta', 'ex_div_date','avg_volumn','market_cap','open','div_yield','1yr_target',
#                                'day_range','ask')

summary_nyse$ticker_name = nyse_ticker_list[1:300]
summary_nyse$company_name = top_nyse$Name[1:300]
summary_nyse$sector = top_nyse$Sector[1:300]
summary_nyse$industry = top_nyse$Industry[1:300]
# summary_nyse$latest_price = top_nyse$LastSale[1:105]
summary_nyse$EPS = as.character(summary_nyse$EPS)
summary_nyse$previous_close = as.character(summary_nyse$previous_close)
summary_nyse$PE_ratio = as.character(summary_nyse$PE_ratio)
summary_nyse$open_price = as.character(summary_nyse$open_price)


summary_nyse = summary_nyse[, c(17,18,19,20,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]


#################### JOIN NYSE AND NASDAQ ####################

summary_stocks = rbind(summary_nasdaq, summary_nyse)
