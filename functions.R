require_symbol <- function(symbol, envir=parent.frame()) {
  if(is.null(envir[[symbol]])) {
    envir[[symbol]] <- getSymbols(symbol, src="yahoo",
                                  auto.assign = FALSE)
  }
  envir[[symbol]]
}
fNum <- function(x) {
  format(x, decimal.mark=".", big.mark=" ",
         nsmall = 1)
}
make_chart <- function(symbol_data, symbol) {
  candleChart(symbol_data, type="candlesticks",
              up.col = "blue", dn.col = "red",
              theme = "white",
              subset = "2019-01-01/",
              TA = c(addMACD(fast=12, slow=26, signal=2),
                     addRSI(n=14),
                     addBBands(n=20, sd=2, maType="SMA"),
                     addSMA(n=10, col="blue"),
                     addEMA(n=50, col="green"),
                     addSAR(),
                     addVo()),
              name=symbol)
}
lastDay <- function(symbol_data) {
  data.frame(open=tail(symbol_data, n=1)[[1]],
             high=tail(symbol_data, n=1)[[2]],
             low=tail(symbol_data, n=1)[[3]],
             close=tail(symbol_data, n=1)[[4]],
             volume=tail(symbol_data, n=1)[[5]])
}
today <- function(symbol) {
  mData <- getQuote(paste(symbol, "JK", sep="."), src="yahoo")
  data.frame(open=mData[5], high=mData[6],
             low=mData[7], last=mData[2],
             volume=mData[8])
}
sum360s <- function(symbol_data) {
  mData <- na.omit(as.data.frame(tail(symbol_data, n=360)))
  names(mData) <- c('Open','High','Low','Close','Vol','Adj')
  data.frame(highest=max(mData[, c('High')]),
             lowest=min(mData[, c('Low')]),
             mean_price=mean(mData[, c('Close')]),
             mean_volume=mean(mData[, c('Vol')]))
}
sum90s <- function(symbol_data) {
  mData <- na.omit(as.data.frame(tail(symbol_data, n=90)))
  names(mData) <- c('Open','High','Low','Close','Vol','Adj')
  data.frame(highest=max(mData[, c('High')]),
             lowest=min(mData[, c('Low')]),
             mean_price=mean(mData[, c('Close')]),
             mean_volume=mean(mData[, c('Vol')]))
}
sum60s <- function(symbol_data) {
  mData <- na.omit(as.data.frame(tail(symbol_data, n=60)))
  names(mData) <- c('Open','High','Low','Close','Vol','Adj')
  data.frame(highest=max(mData[, c('High')]),
             lowest=min(mData[, c('Low')]),
             mean_price=mean(mData[, c('Close')]),
             mean_volume=mean(mData[, c('Vol')]))
}
sum40s <- function(symbol_data) {
  mData <- na.omit(as.data.frame(tail(symbol_data, n=40)))
  names(mData) <- c('Open','High','Low','Close','Vol','Adj')
  data.frame(highest=max(mData[, c('High')]),
             lowest=min(mData[, c('Low')]),
             mean_price=mean(mData[, c('Close')]),
             mean_volume=mean(mData[, c('Vol')]))
}
sum20s <- function(symbol_data) {
  mData <- na.omit(as.data.frame(tail(symbol_data, n=20)))
  names(mData) <- c('Open','High','Low','Close','Vol','Adj')
  data.frame(highest=max(mData[, c('High')]),
             lowest=min(mData[, c('Low')]),
             mean_price=mean(mData[, c('Close')]),
             mean_volume=mean(mData[, c('Vol')]))
}
sum5s <- function(symbol_data) {
  mData <- na.omit(as.data.frame(tail(symbol_data, n=5)))
  names(mData) <- c('Open','High','Low','Close','Vol','Adj')
  data.frame(highest=max(mData[, c('High')]),
             lowest=min(mData[, c('Low')]),
             mean_price=mean(mData[, c('Close')]),
             mean_volume=mean(mData[, c('Vol')]))
}
summary360 <- function(kemarin, sum360) {
  vol_kemarin <- ifelse(kemarin$volume > sum360$mean_volume, "**atas**", "*bawah*")
  price_kemarin <- ifelse(kemarin$close > sum360$mean_price, "**atas**", "*bawah*")
  paste(sep = " ",
        "Berdasarkan data 360 hari terakhir, volume perdagangan kemarin di", vol_kemarin,
        "rata-rata volume perdagangan dan harga penutupan berada di",
        price_kemarin, "harga rata-rata penutupan")
}
summary90 <- function(kemarin, sum90) {
  vol_kemarin <- ifelse(kemarin$volume > sum90$mean_volume, "**atas**", "*bawah*")
  price_kemarin <- ifelse(kemarin$close > sum90$mean_price, "**atas**", "*bawah*")
  paste(sep = " ",
        "Berdasarkan data 90 hari terakhir, volume perdagangan kemarin di", vol_kemarin,
        "rata-rata volume perdagangan dan harga penutupan berada di",
        price_kemarin, "harga rata-rata penutupan")
}
summary20 <- function(kemarin, sum20) {
  vol_kemarin <- ifelse(kemarin$volume > sum20$mean_volume, "**atas**", "*bawah*")
  price_kemarin <- ifelse(kemarin$close > sum20$mean_price, "**atas**", "*bawah*")
  paste(sep = " ",
        "Berdasarkan data 20 hari terakhir, volume perdagangan kemarin di", vol_kemarin,
        "rata-rata volume perdagangan dan harga penutupan berada di",
        price_kemarin, "harga rata-rata penutupan")
}
