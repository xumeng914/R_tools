library(quantmod)

setSymbolLookup(WK=list(name='000002.sz',src='yahoo'))
getSymbols("WK")
chartSeries(WK)
