###########################################
# B2B.Web.ID
# Built for Market.B2B.Web.ID
###########################################
# Generic Shiny Template
###########################################

library(shiny)
library(quantmod)

root_dir <- local({
  frame_files <- vapply(sys.frames(), function(env) {
    if (exists("ofile", envir = env, inherits = FALSE)) {
      env$ofile
    } else {
      ""
    }
  }, character(1))

  source_file <- tail(frame_files[nzchar(frame_files)], n = 1)

  if (!length(source_file) || !nzchar(source_file)) {
    getwd()
  } else {
    dirname(normalizePath(source_file))
  }
})

source(file.path(root_dir, "functions.R"), local = TRUE)
source(file.path(root_dir, "sectors.R"), local = TRUE)

create_market_signal_ui <- function() {
  ui <- fluidPage(
    titlePanel(textOutput("appTitle")),
    sidebarLayout(
      sidebarPanel(
        uiOutput("sectorInput"),
        uiOutput("stockInput"),
        selectInput("chart_type", "Type", c("candlesticks", "matchsticks", "bars", "line"))
      ),
      mainPanel(
        p(
          span("--- SMA(10)", style = "color:blue"),
          span("--- EMA(50)", style = "color:green")
        ),
        div(plotOutput("plotStock", width = "100%", height = "475px")),
        p(textOutput("smaData")),
        p(textOutput("emaData")),
        p(textOutput("lastDay")),
        p(textOutput("todayData")),
        p(textOutput("featureData")),
        p(textOutput("regimeData")),
        p(textOutput("scoreData")),
        uiOutput("summaryOutputs")
      )
    )
  )

  ui
}

create_market_signal_server <- function(default_sector_key = NULL) {
  function(input, output, session) {
    symbol_env <- new.env(parent = emptyenv())
    sector_choices <- setNames(names(sector_configs), vapply(sector_configs, `[[`, character(1), "label"))
    initial_sector_key <- if (is.null(default_sector_key)) names(sector_configs)[1] else default_sector_key

    selected_sector_key <- reactive({
      if (!is.null(default_sector_key)) {
        default_sector_key
      } else if (is.null(input$sectorID) || !nzchar(input$sectorID)) {
        initial_sector_key
      } else {
        input$sectorID
      }
    })

    current_config <- reactive({
      get_sector_config(selected_sector_key())
    })

    selected_stock_id <- reactive({
      config <- current_config()

      if (is.null(input$stockID) || !nzchar(input$stockID) || !input$stockID %in% config$stocks) {
        config$stocks[[1]]
      } else {
        input$stockID
      }
    })

    output$appTitle <- renderText({
      paste(current_config()$label, "Stocks")
    })

    output$sectorInput <- renderUI({
      if (is.null(default_sector_key)) {
        selectInput(
          "sectorID",
          "sector",
          choices = sector_choices,
          selected = initial_sector_key
        )
      }
    })

    output$stockInput <- renderUI({
      config <- current_config()
      selectInput("stockID", "stockID", config$stocks, selected = selected_stock_id())
    })

    current_symbol_data <- reactive({
      symbol <- build_symbol(selected_stock_id())
      data <- tryCatch(
        na.omit(require_symbol(symbol, symbol_env)),
        error = function(e) NULL
      )

      validate(need(!is.null(data) && NROW(data) > 0, "Yahoo Finance data unavailable for the selected stock."))
      data
    })

    current_features <- reactive({
      feature_engine(current_symbol_data())
    })

    current_regime <- reactive({
      classify_regime(current_features())
    })

    current_score <- reactive({
      score_decision(current_features(), current_regime())
    })

    output$plotStock <- renderPlot({
      make_chart(
        symbol_data = current_symbol_data(),
        symbol = selected_stock_id(),
        chart_type = input$chart_type,
        subset = current_config()$chart_subset
      )
    })

    output$lastDay <- renderText({
      format_last_day(lastDay(current_symbol_data()))
    })

    output$smaData <- renderText({
      format_indicator_summary(current_symbol_data())
    })

    output$emaData <- renderText({
      format_ema_summary(current_symbol_data())
    })

    output$todayData <- renderText({
      format_today(today(selected_stock_id()))
    })

    output$featureData <- renderText({
      format_feature_summary(current_features())
    })

    output$regimeData <- renderText({
      format_regime_summary(current_regime(), current_features())
    })

    output$scoreData <- renderText({
      format_score_summary(current_score())
    })

    output$summaryOutputs <- renderUI({
      config <- current_config()
      tagList(lapply(config$summary_windows, function(window) {
        textOutput(paste0("summaryData", window))
      }))
    })

    observe({
      lapply(c(360, 90, 30, 5), function(window) {
        output[[paste0("summaryData", window)]] <- renderText(NULL)
      })
    })

    observe({
      lapply(current_config()$summary_windows, function(window) {
        local({
          current_window <- window
          output[[paste0("summaryData", current_window)]] <- renderText({
            summary_data <- summarize_window(current_symbol_data(), current_window)
            format_window_summary(current_window, summary_data)
          })
        })
      })
    })
  }
}

create_market_signal_app <- function(sector_key = NULL) {
  ui <- create_market_signal_ui()
  server <- create_market_signal_server(default_sector_key = sector_key)

  shinyApp(ui = ui, server = server)
}
