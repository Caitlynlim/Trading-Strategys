rm(list =ls())
library(quantmod)
library(FinancialInstrument)
library(PerformanceAnalytics)
library(foreach)
library(blotter)
library(quantstrat)

fee <- function(TxnQty, TxnPrice, Symbol) {
  # Args:
  #   TxnQty: Numeric for number of shares being traded
  #   TxnPrice: Numeric for price per share
  #   Symbol: The symbol being traded (not used here, but will be passed)
  #
  # Returns:
  #   The fee to be applied
  
  return(-0.002 * abs(TxnQty * TxnPrice))}

#Intialize
init_date <- "2008-12-31"
start_date <- "2009-01-01"
end_date <- "2019-04-11"
init_equity <- 1000000
adjustment <- TRUE
getSymbols(Symbols = "BS6.SI", 
           src = "yahoo", index.class = "POSIXct",
           from = start_date, 
           to = end_date, 
           adjust = adjustment)

knitr::kable(head(BS6.SI))

#Strategy Set up
strategy.st<-"basic_strat"
portfolio.st<-"basic_portfolio"
account.st<-"basic_account"
rm.strat(portfolio.st)
rm.strat(account.st)
initPortf(name = portfolio.st,symbols = "BS6.SI",initDate = init_date)

initAcct(name = account.st,portfolios = portfolio.st,initDate = init_date,initEq = init_equity)

initOrders(portfolio = portfolio.st,symbols = "BS6.SI",initDate = init_date)
strategy(strategy.st, store = TRUE)

add.indicator(strategy = strategy.st,
              name = "RSI",
              arguments = list(price = quote(Cl(mktdata)), 
                               n = 14),
              label = "RSI_14")

#Plot RSI
chartSeries(RSI(BS6.SI$BS6.SI.Close,n=14),theme="black",name="RSI n=14")
abline(a=50,b=0,col="blue")

#EMA
fastMA = 12 
slowMA = 26 
signalMA = 9
maType="EMA"
add.indicator(strategy.st, name = "MACD", 
              arguments = list(x=quote(Cl(mktdata)),
                               nFast=fastMA, 
                               nSlow=slowMA,histogram = TRUE),
              label='MACD' 
)


chartSeries(BS6.SI$BS6.SI.Close,TA="addMACD(fast = 12, slow = 26, signal = 9)",theme="black",name="MACD 26-12-9")

mktdata_ind <- applyIndicators(strategy=strategy.st,mktdata=BS6.SI)
mktdata_ind[is.na(mktdata_ind)]=0
knitr::kable(tail(mktdata_ind))

#Add signals
add.signal(strategy.st, name = "sigThreshold", arguments = list(column = "rsi.RSI_14",threshold=50,relationship="gt"), label = "RSI_gt_50")


add.signal(strategy.st, name = "sigCrossover", arguments = list(columns = c("macd.MACD","signal.MACD"),relationship="gt"), label = "macd_gt_0")


add.signal(strategy.st, name = "sigFormula",
           arguments = list(formula="RSI_gt_50 & macd_gt_0",
                            cross = FALSE), label = "Long")
add.signal(strategy.st, name = "sigThreshold", arguments = list(column = "rsi.RSI_14",threshold=50,relationship="lt"), label = "RSI_lt_50")

add.signal(strategy.st, name = "sigCrossover", arguments = list(columns = c("macd.MACD","signal.MACD"),relationship="lt"),cross=TRUE, label = "macd_lt_0")


mktdata_sig <- applySignals(strategy=strategy.st,mktdata=mktdata_ind)
mktdata_sig[is.na(mktdata_sig)]=0
knitr::kable(tail(mktdata_sig))

#Adding Rule
add.rule(strategy = strategy.st, name="ruleSignal", arguments = list(
  sigcol = "Long", sigval = TRUE, orderqty = 100000, ordertype = "market",TxnFees = 'fee', 
  orderside = "long",prefer="Open", replace = FALSE), type = "enter",label='enter long')

add.rule(strategy.st,name='ruleSignal', 
         arguments = list(sigcol="macd_lt_0",
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='market', 
                          orderside='long', 
                          prefer="Open",
                          TxnFees = 'fee',
                          replace=TRUE),
         type='exit',
         label='long exit1'
         
)

add.rule(strategy.st,name='ruleSignal', 
         arguments = list(sigcol="RSI_lt_50",
                          sigval=TRUE, 
                          orderqty='all', 
                          ordertype='market', 
                          orderside='long', 
                          prefer="Open",
                          TxnFees = 'fee',orderset='ocolong',
                          replace=TRUE),
         type='exit',
         label='long exit2'
)

applyStrategy(strategy = strategy.st,portfolios = portfolio.st)

updatePortf(portfolio.st)
updateAcct(account.st)
updateEndEq(account.st)
chart.Posn(portfolio.st,"BS6.SI")

trade_stats <- tradeStats(portfolio.st)
trade_stats1 <- as.data.frame(t(tradeStats(portfolio.st)))
knitr::kable(trade_stats1)

knitr::kable(trade_stats1[c("Portfolio","Symbol","Num.Txns","Num.Trades"),])
knitr::kable(trade_stats1[c("Net.Trading.PL","Avg.Trade.PL","Med.Trade.PL","Std.Dev.Trade.PL","Std.Err.Trade.PL","Avg.Daily.PL","Med.Daily.PL","Std.Dev.Daily.PL","Std.Err.Daily.PL"),])
knitr::kable(trade_stats1[c("Avg.WinLoss.Ratio","Avg.Win.Trade","Med.Win.Trade","Avg.Losing.Trade","Med.Losing.Trade","Largest.Winner","Largest.Loser"),])
knitr::kable(trade_stats1[c("Ann.Sharpe"),])

returns<-PortfReturns(Account=account.st)
charts.PerformanceSummary(returns,colorset=bluefocus)
knitr::kable(head(perTradeStats(portfolio.st, Symbol = "BS6.SI")))
View(returns)

write.csv(returns, file = "1newreturnss.csv")
?lineChart
lineChart(BS6.SI, type = "candlesticks", theme = chartTheme("black"))
addMACD(fast=12,slow=26,signal=9,type="EMA")
addRSI(n=14,maType="EMA")




