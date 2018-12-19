###########################################
# B2B.Web.ID
# Built for Market.B2B.Web.ID
###########################################
# Library: Quantmod
###########################################

# Sectoral
sectoral <- "Mining"

# StockIDs
stockIDs <- c('INCO','PTBA','ADRO')

# Functions
# 1. get symbol
require_symbol <- function(symbol, envir=parent.frame()) {
  if(is.null(envir[[symbol]])) {
    envir[[symbol]] <- getSymbols(symbol, src="yahoo",
                                  auto.assign = FALSE)
  }
  envir[[symbol]]
}
# 2. numeric
fNum <- function(x) {
  format(x, decimal.mark=".", big.mark=" ",
         nsmall = 1)
}
# 3. pivots
pivots <- function(data, lagts=TRUE) {
  center <- xts(rowSums(HLC(data))/3,
                order.by=index(data))
  R1 <- (2*center)-Lo(data)
  S1 <- (2*center)-Hi(data)
  R2 <- center + (R1 - S1)
  S2 <- center - (R1 - S1)
  ret <- cbind(center, R1, R2, S1, S2)
  colnames(ret) <- c("center", "R1", "R2", "S1", "S2")
  if(lagts) {
    newrow <- xts(t(rep(NA,5)),
                  order.by=last(index(data))+1)
    ret <- rbind(ret, newrow)
    ret <- lag.xts(ret)
  }
  return(ret)
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(paste(sep=" ",
                   sectoral,
                   "Stocks")),
  
  # FluidRow 
  sidebarLayout(
    sidebarPanel(
      selectInput("stockID",
                  "stockID",
                  stockIDs),
      selectInput("chart_type",
                  "Type",
                  c('candlesticks','matchsticks',
                    'bars','line'))
    ),
    # Show a plot of the generated distribution
    mainPanel(
      div(plotOutput("plotStock",
                     width = "100%", height = "425px")),
      p(span("--- SMA(10)", style="color:blue"),
        span("--- EMA(50)", style="color:green")),
      p(textOutput("lastDay")),
      p(textOutput("smaData")),
      p(textOutput("emaData")),
      p(textOutput("todayData")),
      p(textOutput("pivotData")),
      p(textOutput("summaryData"))
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  symbol_env <- new.env()
  make_chart <- function(symbol, smaNum) {
    symbol_data <- require_symbol(paste(symbol,"JK",sep="."),
                                  symbol_env)
    candleChart(symbol_data, type=input$chart_type,
                up.col = "blue", dn.col = "red",
                theme = "white",
                subset = "2017-12-01/",
                TA = c(addMACD(fast=12, slow=26, signal=2),
                       addRSI(n=14),
                       addBBands(n=20, sd=2, maType="SMA"),
                       addSMA(n=10, col="blue"),
                       addEMA(n=50, col="green"),
                       addSAR(),
                       addVo()),
                name=symbol)
  }
  output$plotStock <- renderPlot({
    make_chart(input$stockID)
  })
  output$lastDay <- renderText({
    symbol_data <- na.omit(require_symbol(paste(input$stockID,
                                                "JK",
                                                sep="."),
                                          symbol_env))
    paste("[Yesterday] Open:",
          tail(symbol_data, n=1)[[1]],
          "|",
          "High:",
          tail(symbol_data, n=1)[[2]],
          "|",
          "Low:",
          tail(symbol_data, n=1)[[3]],
          "|",
          "Volume:",
          fNum(tail(symbol_data, n=1)[[5]]))
  })
  output$smaData <- renderText({
    symbol_data <- na.omit(require_symbol(paste(input$stockID,
                                                "JK",
                                                sep="."),
                                          symbol_env))
    paste("[SMA] 10:",
          tail(SMA(symbol_data[,1], n=10), n=1)[1],
          "|",
          "20:",
          tail(SMA(symbol_data[,1], n=20), n=1)[1],
          "|",
          "30:",
          tail(SMA(symbol_data[,1], n=30), n=1)[1],
          "|",
          "40:",
          tail(SMA(symbol_data[,1], n=40), n=1)[1],
          "|",
          "50:",
          tail(SMA(symbol_data[,1], n=50), n=1)[1],
          "|",
          "60:",
          fNum(tail(SMA(symbol_data[,1], n=60), n=1)[1]))
  })
  output$emaData <- renderText({
    symbol_data <- na.omit(require_symbol(paste(input$stockID,
                                                "JK",
                                                sep="."),
                                          symbol_env))
    paste("[EMA] 10:",
          fNum(tail(EMA(symbol_data[,1], n=10), n=1)[1]),
          "|",
          "20:",
          fNum(tail(EMA(symbol_data[,1], n=20), n=1)[1]),
          "|",
          "30:",
          fNum(tail(EMA(symbol_data[,1], n=30), n=1)[1]),
          "|",
          "40:",
          fNum(tail(EMA(symbol_data[,1], n=40), n=1)[1]),
          "|",
          "50:",
          fNum(tail(EMA(symbol_data[,1], n=50), n=1)[1]))
  })
  output$todayData <- renderText({
    mData <- getQuote(paste(input$stockID,
                            "JK",
                            sep="."),
                      src="yahoo")
    paste("[Today] Last:", mData[2], "|",
          "Open :", mData[5], "|",
          "High :", mData[6], "|",
          "Low :", mData[7], "|",
          "Volume :", fNum(mData[8]))
  })
  output$pivotData <- renderText({
    symbol_data <- na.omit(require_symbol(paste(input$stockID,
                                                "JK",
                                                sep="."),
                                          symbol_env))
    monthly_data <- to.monthly(symbol_data, drop.time = TRUE)
    pivot_data <- lag(pivots(monthly_data, lagts = F))
    pivot <- cbind(symbol_data, pivot_data)
    pivot[, 7:11] <- na.locf(pivot[, 7:11])
    piv <- tail(pivot, n=1)
    paste("[Pivot] Center:", fNum(piv$center), "|",
          "S2 :", fNum(piv$S2), "|",
          "S1 :", fNum(piv$S1), "|",
          "R1 :", fNum(piv$R1), "|",
          "R2 :", fNum(piv$R2), "|")
  })
  output$downloadPlot <- downloadHandler(
    filename = function() { paste(input$stockID, '.png', sep='') },
    content = function(file) {
      ggsave(file, make_chart(input$stockID))
    }
  )
  output$summaryData <- renderText({
    symbol_data <- require_symbol(paste(input$stockID,"JK",sep="."),
                                  symbol_env)
    mData <- na.omit(as.data.frame(tail(symbol_data, n=90)))
    names(mData) <- c('Open','High','Low','Close','Vol','Adj')
    paste("[90 days] Lowest:", min(mData[, c('Low')]), "|",
          "Highest :", max(mData[, c('High')]), "|",
          "Mean :", fNum(mean(mData[, c('Close')])), "|",
          "Vol Mean :", fNum(mean(mData[, c('Vol')])),
          sep=" ")
  })
}

# Run the application
shinyApp(ui = ui, server = server)
