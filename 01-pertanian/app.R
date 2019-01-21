###########################################
# B2B.Web.ID
# Built for Market.B2B.Web.ID
###########################################
# Library: Quantmod
###########################################

# Library
library(shiny)
library(quantmod)

# StockIDs
sectoral <- "Agriculture"
stockIDs <- c('BISI','AALI','ANJT','BWPT','DNSG')

# Functions
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
                     width = "100%", height = "475px")),
      p(span("--- SMA(10)", style="color:blue"),
        span("--- EMA(50)", style="color:green")),
      p(textOutput("lastDay")),
      p(textOutput("smaData")),
      p(textOutput("emaData")),
      p(textOutput("todayData")),
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
