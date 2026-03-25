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
    tags$head(
      tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
      tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = "anonymous"),
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Space+Grotesk:wght@400;500;700&family=IBM+Plex+Mono:wght@400;500&display=swap"
      ),
      tags$style(HTML("
        :root {
          --ms-bg: #f4efe6;
          --ms-panel: rgba(255, 251, 245, 0.84);
          --ms-panel-strong: #fffaf1;
          --ms-line: rgba(31, 41, 36, 0.12);
          --ms-text: #1f2924;
          --ms-muted: #607067;
          --ms-accent: #16624f;
          --ms-accent-soft: #e0b47c;
          --ms-shadow: 0 18px 40px rgba(29, 39, 34, 0.10);
          --ms-radius: 24px;
        }

        body {
          background:
            radial-gradient(circle at top left, rgba(224, 180, 124, 0.16), transparent 28%),
            radial-gradient(circle at top right, rgba(22, 98, 79, 0.14), transparent 24%),
            linear-gradient(180deg, #fbf7ef 0%, #f3ede2 100%);
          color: var(--ms-text);
          font-family: 'Space Grotesk', sans-serif;
        }

        .container-fluid {
          max-width: 1840px;
          padding: 22px 24px 36px;
        }

        .ms-shell {
          display: grid;
          gap: 22px;
        }

        .ms-hero {
          display: flex;
          justify-content: space-between;
          gap: 20px;
          align-items: end;
          padding: 24px 28px;
          border-radius: 28px;
          border: 1px solid var(--ms-line);
          background: linear-gradient(135deg, rgba(255, 250, 241, 0.92), rgba(246, 239, 230, 0.92));
          box-shadow: var(--ms-shadow);
        }

        .ms-eyebrow,
        .ms-mini {
          font-family: 'IBM Plex Mono', monospace;
          font-size: 12px;
          letter-spacing: 0.12em;
          text-transform: uppercase;
          color: var(--ms-muted);
        }

        .ms-title {
          margin: 10px 0 8px;
          font-size: clamp(34px, 5vw, 58px);
          line-height: 0.95;
          letter-spacing: -0.05em;
          font-weight: 700;
        }

        .ms-copy {
          max-width: 72ch;
          color: var(--ms-muted);
          line-height: 1.7;
          margin: 0;
        }

        .ms-layout {
          display: grid;
          grid-template-columns: 260px minmax(0, 1fr);
          gap: 20px;
          align-items: start;
        }

        .ms-panel {
          background: var(--ms-panel);
          border: 1px solid var(--ms-line);
          border-radius: var(--ms-radius);
          box-shadow: var(--ms-shadow);
          backdrop-filter: blur(10px);
        }

        .ms-sidebar {
          padding: 20px;
          position: sticky;
          top: 18px;
        }

        .ms-sidebar .form-group {
          margin-bottom: 16px;
        }

        .ms-sidebar label {
          font-family: 'IBM Plex Mono', monospace;
          font-size: 12px;
          letter-spacing: 0.1em;
          text-transform: uppercase;
          color: var(--ms-muted);
        }

        .ms-sidebar .form-control {
          border-radius: 16px;
          border: 1px solid var(--ms-line);
          box-shadow: none;
          min-height: 46px;
        }

        .ms-main {
          display: grid;
          gap: 18px;
        }

        .ms-primary-grid {
          display: grid;
          grid-template-columns: minmax(0, 2.15fr) minmax(360px, 0.85fr);
          gap: 18px;
          align-items: start;
        }

        .ms-side-stack {
          display: grid;
          gap: 18px;
        }

        .ms-chart-card,
        .ms-stat-card,
        .ms-summary-card {
          padding: 20px;
        }

        .ms-chart-card {
          background: var(--ms-panel-strong);
        }

        .ms-chart-note {
          display: flex;
          flex-wrap: wrap;
          gap: 14px;
          margin-bottom: 14px;
          color: var(--ms-muted);
          font-size: 13px;
        }

        .ms-chip {
          display: inline-flex;
          align-items: center;
          gap: 8px;
          padding: 8px 12px;
          border-radius: 999px;
          background: rgba(255, 255, 255, 0.72);
          border: 1px solid var(--ms-line);
        }

        .ms-dot {
          width: 9px;
          height: 9px;
          border-radius: 50%;
          display: inline-block;
        }

        .ms-grid {
          display: grid;
          grid-template-columns: repeat(3, minmax(0, 1fr));
          gap: 14px;
          align-items: stretch;
        }

        .ms-grid.ms-secondary-grid {
          grid-template-columns: 1.15fr 1fr 1fr;
        }

        .ms-grid > .shiny-html-output {
          display: contents;
        }

        .ms-summary-stack > .shiny-html-output {
          display: contents;
        }

        .ms-metric-grid {
          display: grid;
          grid-template-columns: repeat(2, minmax(0, 1fr));
          gap: 12px;
          align-items: stretch;
        }

        .ms-metric {
          padding: 14px;
          border-radius: 18px;
          background: rgba(255, 255, 255, 0.78);
          border: 1px solid var(--ms-line);
          min-height: 112px;
        }

        .ms-metric-label {
          font-family: 'IBM Plex Mono', monospace;
          font-size: 11px;
          text-transform: uppercase;
          letter-spacing: 0.08em;
          color: var(--ms-muted);
          margin-bottom: 8px;
        }

        .ms-metric-value {
          font-size: 21px;
          line-height: 1.2;
          letter-spacing: -0.04em;
          font-weight: 700;
          color: var(--ms-text);
        }

        .ms-metric-subvalue {
          margin-top: 6px;
          font-size: 13px;
          line-height: 1.5;
          color: var(--ms-muted);
        }

        .ms-stat-card,
        .ms-summary-card {
          min-height: 100%;
        }

        .ms-stat-card {
          display: flex;
          flex-direction: column;
        }

        .ms-card-label {
          font-family: 'IBM Plex Mono', monospace;
          font-size: 12px;
          text-transform: uppercase;
          letter-spacing: 0.1em;
          color: var(--ms-muted);
          margin-bottom: 12px;
        }

        .ms-value {
          font-size: 14px;
          line-height: 1.75;
          color: var(--ms-text);
          white-space: pre-wrap;
        }

        .ms-score-card {
          transition: background 0.2s ease, border-color 0.2s ease;
        }

        .ms-score-card.bullish {
          background: linear-gradient(135deg, rgba(23, 122, 85, 0.18), rgba(255, 250, 241, 0.96));
          border-color: rgba(23, 122, 85, 0.28);
        }

        .ms-score-card.neutral {
          background: linear-gradient(135deg, rgba(203, 164, 109, 0.18), rgba(255, 250, 241, 0.96));
          border-color: rgba(203, 164, 109, 0.24);
        }

        .ms-score-card.bearish {
          background: linear-gradient(135deg, rgba(156, 63, 48, 0.18), rgba(255, 250, 241, 0.96));
          border-color: rgba(156, 63, 48, 0.28);
        }

        .ms-badge {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          min-height: 32px;
          padding: 0 12px;
          border-radius: 999px;
          border: 1px solid var(--ms-line);
          background: rgba(255, 255, 255, 0.75);
          font-family: 'IBM Plex Mono', monospace;
          font-size: 12px;
          letter-spacing: 0.08em;
          text-transform: uppercase;
          color: var(--ms-text);
        }

        .ms-badge.bullish {
          background: rgba(23, 122, 85, 0.14);
          border-color: rgba(23, 122, 85, 0.26);
          color: #125742;
        }

        .ms-badge.neutral {
          background: rgba(203, 164, 109, 0.16);
          border-color: rgba(203, 164, 109, 0.28);
          color: #7f5a22;
        }

        .ms-badge.bearish {
          background: rgba(156, 63, 48, 0.14);
          border-color: rgba(156, 63, 48, 0.26);
          color: #8f3c2e;
        }

        .ms-summary-stack {
          overflow-x: auto;
        }

        .ms-table {
          width: 100%;
          border-collapse: collapse;
          min-width: 720px;
        }

        .ms-table th,
        .ms-table td {
          padding: 12px 14px;
          border-bottom: 1px solid var(--ms-line);
          text-align: left;
          font-size: 14px;
          vertical-align: middle;
        }

        .ms-table th {
          font-family: 'IBM Plex Mono', monospace;
          font-size: 11px;
          text-transform: uppercase;
          letter-spacing: 0.08em;
          color: var(--ms-muted);
          font-weight: 500;
        }

        .ms-table td {
          color: var(--ms-text);
        }

        .ms-table tr:last-child td {
          border-bottom: none;
        }

        .ms-table-badge {
          display: inline-flex;
          align-items: center;
          justify-content: center;
          min-height: 28px;
          padding: 0 10px;
          border-radius: 999px;
          border: 1px solid var(--ms-line);
          background: rgba(255, 255, 255, 0.72);
          font-family: 'IBM Plex Mono', monospace;
          font-size: 11px;
          text-transform: uppercase;
          letter-spacing: 0.06em;
        }

        .ms-table-badge.above {
          background: rgba(23, 122, 85, 0.12);
          border-color: rgba(23, 122, 85, 0.22);
          color: #125742;
        }

        .ms-table-badge.below {
          background: rgba(156, 63, 48, 0.12);
          border-color: rgba(156, 63, 48, 0.22);
          color: #8f3c2e;
        }

        #plotStock {
          width: 100%;
          min-height: 700px;
        }

        .ms-chart-card {
          padding-bottom: 14px;
        }

        .ms-chart-card .shiny-plot-output {
          border-radius: 18px;
          overflow: hidden;
          background: rgba(255, 255, 255, 0.54);
        }

        @media (min-width: 1700px) {
          .container-fluid {
            max-width: 1920px;
            padding-left: 28px;
            padding-right: 28px;
          }

          .ms-layout {
            grid-template-columns: 250px minmax(0, 1fr);
          }

          .ms-primary-grid {
            grid-template-columns: minmax(0, 2.35fr) minmax(380px, 0.8fr);
          }

          #plotStock {
            min-height: 760px;
          }
        }

        @media (max-width: 1100px) {
          .ms-primary-grid {
            grid-template-columns: 1fr;
          }

          .ms-grid {
            grid-template-columns: repeat(2, minmax(0, 1fr));
          }

          .ms-grid.ms-secondary-grid {
            grid-template-columns: repeat(2, minmax(0, 1fr));
          }

          .ms-summary-stack {
            grid-template-columns: repeat(2, minmax(0, 1fr));
          }
        }

        @media (max-width: 900px) {
          .ms-layout,
          .ms-hero {
            grid-template-columns: 1fr;
            display: grid;
          }

          .ms-sidebar {
            position: static;
          }

          #plotStock {
            min-height: 500px;
          }
        }

        @media (max-width: 640px) {
          .container-fluid {
            padding: 16px 12px 24px;
          }

          .ms-hero,
          .ms-sidebar,
          .ms-chart-card,
          .ms-stat-card,
          .ms-summary-card {
            padding: 16px;
            border-radius: 20px;
          }

          #plotStock {
            min-height: 380px;
          }

          .ms-grid,
          .ms-grid.ms-secondary-grid,
          .ms-metric-grid,
          .ms-summary-stack {
            grid-template-columns: 1fr;
          }
        }
      "))
    ),
    div(
      class = "ms-shell",
      div(
        class = "ms-hero",
        div(
          div(class = "ms-eyebrow", "Market Signal / Shiny Dashboard"),
          div(class = "ms-title", textOutput("appTitle", inline = TRUE)),
          p(class = "ms-copy", "Sector-level scanning with chart overlays, regime rules, and score-based bias from daily OHLCV data.")
        ),
        div(
          class = "ms-mini",
          "Shared template, multi-sector selector, rule-based scoring"
        )
      ),
      div(
        class = "ms-layout",
        div(
          class = "ms-panel ms-sidebar",
          uiOutput("sectorInput"),
          uiOutput("stockInput"),
          selectInput("chart_type", "Type", c("candlesticks", "matchsticks", "bars", "line"))
        ),
        div(
          class = "ms-main",
          div(
            class = "ms-primary-grid",
            div(
              class = "ms-panel ms-chart-card",
              div(
                class = "ms-chart-note",
                span(class = "ms-chip", span(class = "ms-dot", style = "background:#2c7be5;"), "SMA(10)"),
                span(class = "ms-chip", span(class = "ms-dot", style = "background:#1f8a58;"), "EMA(50)")
              ),
              plotOutput("plotStock", width = "100%", height = "800px")
            ),
            div(
              class = "ms-side-stack",
              uiOutput("signalCard"),
              div(
                class = "ms-panel ms-stat-card",
                div(class = "ms-card-label", "Market Snapshot"),
                uiOutput("marketTiles")
              ),
              div(
                class = "ms-panel ms-stat-card",
                div(class = "ms-card-label", "Indicators"),
                uiOutput("indicatorTiles")
              )
            )
          ),
          div(
            class = "ms-grid ms-secondary-grid",
            div(
              class = "ms-panel ms-stat-card",
              div(class = "ms-card-label", "Signal Layer"),
              uiOutput("featureTiles")
            ),
            div(
              class = "ms-panel ms-stat-card",
              div(class = "ms-card-label", "Regime"),
              uiOutput("regimeTiles")
            ),
            div(
              class = "ms-panel ms-stat-card",
              div(class = "ms-card-label", "Decision"),
              uiOutput("scoreTiles")
            )
          ),
          div(
            class = "ms-panel ms-summary-card",
            div(class = "ms-card-label", "Rolling Summary"),
            div(class = "ms-summary-stack", uiOutput("summaryTable"))
          )
        )
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

    metric_tile <- function(label, value, subvalue = NULL) {
      div(
        class = "ms-metric",
        div(class = "ms-metric-label", label),
        div(class = "ms-metric-value", value),
        if (!is.null(subvalue)) div(class = "ms-metric-subvalue", subvalue)
      )
    }

    score_badge_class <- reactive({
      score_tone(current_score())
    })

    output$indicatorTiles <- renderUI({
      indicators <- indicator_snapshot(current_symbol_data())

      div(
        class = "ms-metric-grid",
        metric_tile("SMA 10", display_num(indicators$sma10), paste("SMA 20", display_num(indicators$sma20))),
        metric_tile("SMA 50", display_num(indicators$sma50), paste("SMA 60", display_num(indicators$sma60))),
        metric_tile("EMA 10", display_num(indicators$ema10), paste("EMA 20", display_num(indicators$ema20))),
        metric_tile("EMA 50", display_num(indicators$ema50), paste("EMA 30", display_num(indicators$ema30)))
      )
    })

    output$marketTiles <- renderUI({
      yesterday <- lastDay(current_symbol_data())
      today_snapshot <- today(selected_stock_id())

      div(
        class = "ms-metric-grid",
        metric_tile("Yesterday Close", display_num(yesterday$close), paste("Vol", display_num(yesterday$volume))),
        metric_tile("Yesterday Range", paste(display_num(yesterday$low), "-", display_num(yesterday$high)), paste("Open", display_num(yesterday$open))),
        metric_tile("Today Last", if (is.null(today_snapshot)) "N/A" else display_num(today_snapshot$last), if (is.null(today_snapshot)) "Quote unavailable" else paste("Open", display_num(today_snapshot$open))),
        metric_tile("Today Range", if (is.null(today_snapshot)) "N/A" else paste(display_num(today_snapshot$low), "-", display_num(today_snapshot$high)), if (is.null(today_snapshot)) NULL else paste("Vol", display_num(today_snapshot$volume)))
      )
    })

    output$signalCard <- renderUI({
      tone <- score_tone(current_score())

      div(
        class = paste("ms-panel ms-stat-card ms-score-card", tone),
        div(class = "ms-card-label", "Bias Overview"),
        uiOutput("biasOverview")
      )
    })

    output$biasOverview <- renderUI({
      score_output <- current_score()
      badge_class <- score_badge_class()

      div(
        class = "ms-metric-grid",
        metric_tile("Score", as.character(score_output$score), as.character(score_output$action)),
        div(
          class = "ms-metric",
          div(class = "ms-metric-label", "Label"),
          div(class = paste("ms-badge", badge_class), as.character(score_output$label)),
          div(class = "ms-metric-subvalue", paste("Regime", as.character(score_output$regime)))
        )
      )
    })

    output$featureTiles <- renderUI({
      features <- current_features()

      div(
        class = "ms-metric-grid",
        metric_tile("Dist EMA50", paste0(display_num(features$distance_ema50_pct), "%")),
        metric_tile("Momentum 5D", paste0(display_num(features$momentum_5d_pct), "%")),
        metric_tile("ATR %", paste0(display_num(features$atr_pct), "%")),
        metric_tile("Volume x20", display_num(features$volume_ratio_20))
      )
    })

    output$regimeTiles <- renderUI({
      features <- current_features()
      regime <- current_regime()
      badge_class <- score_badge_class()

      div(
        class = "ms-metric-grid",
        div(
          class = "ms-metric",
          div(class = "ms-metric-label", "Current Regime"),
          div(class = paste("ms-badge", badge_class), regime),
          div(class = "ms-metric-subvalue", paste("EMA50 slope", ifelse(features$ema50_slope_up, "up", "down")))
        ),
        metric_tile("Above EMA50", ifelse(features$price_above_ema50, "Yes", "No")),
        metric_tile("SMA20 > SMA60", ifelse(features$sma_stack_bullish, "Yes", "No")),
        metric_tile("20D Range", paste0(display_num(features$range_20d_pct), "%"))
      )
    })

    output$scoreTiles <- renderUI({
      features <- current_features()
      score_output <- current_score()
      regime <- current_regime()

      tile_defs <- list(
        list(
          key = "breakout",
          weight = if (regime == "bullish_breakout") 100 else if (features$breakout_20d) 80 else 20,
          tile = metric_tile("Breakout 20D", ifelse(features$breakout_20d, "Yes", "No"))
        ),
        list(
          key = "breakdown",
          weight = if (regime == "bearish_breakdown") 100 else if (features$breakdown_20d) 80 else 20,
          tile = metric_tile("Breakdown 20D", ifelse(features$breakdown_20d, "Yes", "No"))
        ),
        list(
          key = "ema20",
          weight = if (score_output$score >= 0 && features$price_above_ema20) 70 else if (score_output$score < 0 && !features$price_above_ema20) 70 else 30,
          tile = metric_tile("Price > EMA20", ifelse(features$price_above_ema20, "Yes", "No"))
        ),
        list(
          key = "ema50",
          weight = if (score_output$score >= 0 && features$price_above_ema50) 90 else if (score_output$score < 0 && !features$price_above_ema50) 90 else 40,
          tile = metric_tile("Price > EMA50", ifelse(features$price_above_ema50, "Yes", "No"))
        ),
        list(
          key = "close_pos_20",
          weight = if (!is.na(features$close_position_20d) && (features$close_position_20d >= 80 || features$close_position_20d <= 20)) 85 else 35,
          tile = metric_tile("Close Pos 20D", paste0(display_num(features$close_position_20d), "%"))
        ),
        list(
          key = "close_pos_90",
          weight = if (!is.na(features$close_position_90d) && (features$close_position_90d >= 70 || features$close_position_90d <= 30)) 75 else 35,
          tile = metric_tile("Close Pos 90D", paste0(display_num(features$close_position_90d), "%"))
        )
      )

      if (regime == "compression") {
        tile_defs <- c(
          list(list(
            key = "compression",
            weight = 110,
            tile = metric_tile("Compression", "Yes", paste("Range 20D", paste0(display_num(features$range_20d_pct), "%")))
          )),
          tile_defs
        )
      }

      ordered_tiles <- lapply(tile_defs[order(vapply(tile_defs, function(item) item$weight, numeric(1)), decreasing = TRUE)], function(item) item$tile)

      div(
        class = "ms-metric-grid",
        ordered_tiles
      )
    })

    output$summaryTable <- renderUI({
      config <- current_config()

      summary_rows <- rolling_summary_table(current_symbol_data(), config$summary_windows)

      tags$table(
        class = "ms-table",
        tags$thead(
          tags$tr(
            tags$th("Window"),
            tags$th("Low"),
            tags$th("High"),
            tags$th("Mean"),
            tags$th("Range %"),
            tags$th("Close Pos"),
            tags$th("Close vs Mean"),
            tags$th("Vol vs Avg")
          )
        ),
        tags$tbody(
          lapply(seq_len(nrow(summary_rows)), function(i) {
            row <- summary_rows[i, ]

            tags$tr(
              tags$td(row$window),
              tags$td(row$low),
              tags$td(row$high),
              tags$td(row$mean),
              tags$td(row$range_pct),
              tags$td(row$close_position),
              tags$td(tags$span(class = paste("ms-table-badge", row$close_vs_mean), row$close_vs_mean)),
              tags$td(tags$span(class = paste("ms-table-badge", row$vol_vs_avg), row$vol_vs_avg))
            )
          })
        )
      )
    })
  }
}

create_market_signal_app <- function(sector_key = NULL) {
  ui <- create_market_signal_ui()
  server <- create_market_signal_server(default_sector_key = sector_key)

  shinyApp(ui = ui, server = server)
}
