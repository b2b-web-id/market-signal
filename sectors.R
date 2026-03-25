sector_configs <- list(
  "01-pertanian" = list(
    label = "Agriculture",
    stocks = c("BISI", "AALI", "ANJT", "BWPT", "DNSG", "SIMP", "DSFI"),
    summary_windows = c(90),
    chart_subset = "2017-12-01/"
  ),
  "02-pertambangan" = list(
    label = "Mining",
    stocks = c("INCO", "PTBA", "ADRO", "ESSA", "BUMI", "ANTM"),
    summary_windows = c(90),
    chart_subset = "2017-12-01/"
  ),
  "03-ind-dasar-kimia" = list(
    label = "Basic & Chem Ind's",
    stocks = c("SMGR", "KRAS", "WTON", "WSBP", "JPFA"),
    summary_windows = c(90),
    chart_subset = "2017-12-01/"
  ),
  "04-aneka-ind" = list(
    label = "Misc Ind's",
    stocks = c("UNTR", "ASII", "SRIL"),
    summary_windows = c(90),
    chart_subset = "2017-12-01/"
  ),
  "05-ind-brg-konsumsi" = list(
    label = "Consumer Ind's",
    stocks = c("SIDO", "FOOD", "KAEF", "KLBF", "IIKP", "GOOD", "CLEO", "CINT", "HMSP", "WIIM"),
    summary_windows = c(360, 90, 30, 5),
    chart_subset = "2018-12-01/"
  ),
  "06-properti-real-estat" = list(
    label = "Property & RE",
    stocks = c("JRPT", "BKSL", "ASRI", "RBMS"),
    summary_windows = c(90),
    chart_subset = "2017-12-01/"
  ),
  "07-infra-util-trans" = list(
    label = "Infrastrustures etc",
    stocks = c(
      "KOPI", "LAPD", "MPOW", "PGAS", "POWR", "RAJA", "TGRA", "CMNP", "JSMR", "META",
      "BTEL", "EXCL", "FREN", "ISAT", "TLKM", "APOL", "ASSA", "BBRM", "BIRD", "BLTA",
      "BULL", "CANI", "CASS", "GIAA", "HITS"
    ),
    summary_windows = c(90),
    chart_subset = "2017-12-01/"
  ),
  "08-keuangan" = list(
    label = "Finance",
    stocks = c("BJBR", "BJTM", "BBRI", "BBCA", "BBKP", "BBNI", "BMRI", "BBTN", "INPC", "BGTG"),
    summary_windows = c(360, 90, 30, 5),
    chart_subset = "2018-12-01/"
  ),
  "09-dagang-jasa-invest" = list(
    label = "Trade, Services, Invest",
    stocks = c("CLAY", "SDPC", "PJAA", "MNCN", "MAMI", "BHIT"),
    summary_windows = c(90),
    chart_subset = "2017-12-01/"
  )
)

get_sector_config <- function(sector_key) {
  config <- sector_configs[[sector_key]]

  if (is.null(config)) {
    stop(paste("Unknown sector key:", sector_key), call. = FALSE)
  }

  config
}
