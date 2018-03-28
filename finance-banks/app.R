# Sectoral
sectoral <- "Banks"

# StockIDs
stockIDs <- c('BABP','BBKP','BBRI',
              'BBTN','BJTM','INPC',
              'BBCA','BBNI','BDMN',
              'BMRI')

# Source
source("../source.R")

# Run the application
shinyApp(ui = ui, server = server)
