#Remove any existing stats
rm(list =ls())

#Load libraries 
library(quantmod)
library(FinancialInstrument)
library(PerformanceAnalytics)
library(foreach)
library(devtools)
library(blotter)
library(quantstrat)
library(TTR)

options("getSymbols.warning4.0"=FALSE)
Sys.setenv(TZ = "US")
currency('USD')

#Intialize
init_date <- "2008-12-31"
start_date <- "2015-01-01"
end_date <- "2019-04-15"
init_equity <- 1000000
adjustment <- TRUE
getSymbols(Symbols = "ACM", 
           src = "yahoo", index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = adjustment)

knitr::kable(head(ACM))

#Chart RSI and Evwma 
lineChart(ACM,subset='2015-01::2019-04')
addRSI(n = 7, maType = "EMA")
addEVWMA(n = 25)

# A function for computing a transaction fee that is 2% of total value of transaction
fee <- function(TxnQty, TxnPrice, Symbol) {
  # Args:
  #   TxnQty: Numeric for number of shares being traded
  #   TxnPrice: Numeric for price per share
  #   Symbol: The symbol being traded (not used here, but will be passed)
  #
  # Returns:
  #   The fee to be applied
  
  return(-0.002 * abs(TxnQty * TxnPrice))}

#Strategy Set up
strategy.st<-"basic_strat"
portfolio.st<-"basic_portfolio"
account.st<-"basic_account"
rm.strat(portfolio.st)
rm.strat(account.st)
initPortf(name = portfolio.st,symbols = "ACM",initDate = init_date)
initAcct(name = account.st,portfolios = portfolio.st,initDate = init_date,initEq = init_equity)
initOrders(portfolio = portfolio.st,symbols = "ACM",initDate = init_date)
strategy(strategy.st, store = TRUE)

#Add indicators
add.indicator(strategy = strategy.st,
              name = "EVWMA",
              arguments = list(price = quote(Cl(mktdata)), 
                               volume = quote(Vo(mktdata)),
                               n = 25),
              label = "EVWMA_10")

add.indicator(strategy = strategy.st,
              name = "RSI",
              arguments = list(price = quote(Cl(mktdata)), 
                               n = 7),
              label = "RSI_7")

#Apply Indicators
mktdata_ind <- applyIndicators(strategy=strategy.st,mktdata=ACM)
mktdata_ind[is.na(mktdata_ind)]=0
knitr::kable(tail(mktdata_ind))

#RSI less than 40: Buy
add.signal(strategy.st, name = "sigThreshold", 
           arguments = list(column = "rsi.RSI_7",
                            threshold=40,relationship="lt"), 
           label = "RSI_lt_40")

#Close Price below EVWMA: Buy
add.signal(strategy.st, name = "sigCrossover", 
           arguments = list(columns = c("ACM.Close","X1.EVWMA_10"),
                            relationship="lt"), label = "close_lt_evwma")

#Generate Long signal (When RSI < 40 AND Price < EVWMA)
add.signal(strategy.st, name = "sigFormula",
           arguments = list(formula="RSI_lt_40 & close_lt_evwma",
                            cross = FALSE), label = "Long")

#RSI more than 70: Sell
add.signal(strategy.st, name = "sigThreshold", 
           arguments = list(column = "rsi.RSI_7",
                            threshold=70,relationship="gt"), 
           label = "RSI_gt_70")

#Close is above EVWMA: Sell
add.signal(strategy.st, name = "sigCrossover", 
           arguments = list(columns = c("ACM.Close","X1.EVWMA_10"),
                            relationship="gt"), label = "close_gt_evwma")
#Apply Signals
mktdata_sig <- applySignals(strategy=strategy.st,mktdata=mktdata_ind)
mktdata_sig[is.na(mktdata_sig)]=0
knitr::kable(tail(mktdata_sig))

#Adding Rule: Buy when "Long" (When RSI < 40 AND Price < EVWMA)
add.rule(strategy = strategy.st, name="ruleSignal", arguments = list(
  sigcol = "Long", sigval = TRUE, orderqty = 10000 , ordertype = "market",TxnFees = "fee", 
  orderside = "long",prefer="Open", replace = FALSE), type = "enter",label='enter long')

#Adding Rule: Sell 1 when Close price > EVWMA
add.rule(strategy.st,name='ruleSignal', 
         arguments = list(sigcol="close_gt_evwma",
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='market', 
                          orderside='long', 
                          prefer="Open",
                          TxnFees = "fee",
                          replace=TRUE),
         type='exit',
         label='long exit1')

#Adding Rule: Sell 2 when RSI > 70
add.rule(strategy.st,name='ruleSignal', 
         arguments = list(sigcol="RSI_gt_70",
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='market', 
                          orderside='long', 
                          prefer="Open",
                          TxnFees = "fee",orderset='ocolong',
                          replace=TRUE),
         type='exit',
         label='long exit2')

#Apply Strategy, Trade statistics and Graphs
applyStrategy(strategy = strategy.st,portfolios = portfolio.st)
updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
chart.Posn(portfolio.st,"ACM")

trade_stats <- tradeStats(portfolio.st)
trade_stats1 <- as.data.frame(t(tradeStats(portfolio.st)))
knitr::kable(trade_stats1)

knitr::kable(trade_stats1[c("Portfolio","Symbol","Num.Txns","Num.Trades"),])
knitr::kable(trade_stats1[c("Net.Trading.PL","Avg.Trade.PL","Med.Trade.PL","Std.Dev.Trade.PL","Std.Err.Trade.PL","Avg.Daily.PL","Med.Daily.PL","Std.Dev.Daily.PL","Std.Err.Daily.PL"),])
knitr::kable(trade_stats1[c("Avg.WinLoss.Ratio","Avg.Win.Trade","Med.Win.Trade","Avg.Losing.Trade","Med.Losing.Trade","Largest.Winner","Largest.Loser"),])
knitr::kable(trade_stats1[c("Ann.Sharpe"),])

returns<-PortfReturns(Account=account.st)
charts.PerformanceSummary(returns,colorset=bluefocus)
knitr::kable(head(perTradeStats(portfolio.st, Symbol = "ACM")))











