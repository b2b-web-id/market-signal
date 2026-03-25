# market-signal

Shiny app untuk visualisasi saham IDX berbasis data Yahoo Finance via `quantmod`.

Repo ini sekarang memakai satu template app bersama, konfigurasi sektor terpusat, serta layer analisis rule-based untuk feature extraction, regime classification, dan score-based decision output.

## Struktur Saat Ini

- `app.R`: entry point utama di root project, dengan selector sektor dan saham.
- `source.R`: template Shiny generik yang dipakai oleh app root dan semua app sektoral.
- `functions.R`: helper untuk data loading, formatting, feature engine, regime classifier, dan score output.
- `sectors.R`: konfigurasi sektor, daftar saham, horizon summary, dan chart subset.
- `index.html`: landing page statis yang menaut ke app sektoral.
- `01-pertanian/` sampai `09-dagang-jasa-invest/`: wrapper tipis per sektor yang memanggil template generik.

## Fitur App

- Chart saham dengan `candlestick`, `matchsticks`, `bars`, atau `line`.
- Indikator teknikal bawaan: MACD, RSI, Bollinger Bands, SMA(10), EMA(50), SAR, dan volume.
- Snapshot ringkas:
  - data hari terakhir
  - SMA dan EMA
  - quote harian jika tersedia
  - summary rolling 90 hari atau beberapa window lain tergantung sektor
- Feature engine berbasis OHLCV harian:
  - distance ke EMA50
  - momentum 5 hari
  - ATR%
  - range 20 hari
  - rasio volume vs rata-rata 20 hari
  - breakout/breakdown 20 hari
- Rule-based regime classifier:
  - `bullish_breakout`
  - `bearish_breakdown`
  - `trend_up`
  - `trend_down`
  - `high_volatility`
  - `compression`
  - `range`
- Score-based decision output:
  - skor agregat
  - label seperti `strong_bullish`, `bullish`, `neutral`, `bearish`, `strong_bearish`
  - action seperti `favor_long_bias`, `watch_long_setups`, `wait`, `defensive`, `avoid_longs`

## Dependency

- R
- Paket R:
  - `shiny`
  - `quantmod`
- Koneksi internet untuk `getSymbols()` dan `getQuote()` dari Yahoo Finance

Install dependency:

```r
install.packages(c("shiny", "quantmod"))
```

## Menjalankan App

Jalankan app utama dari root project:

```r
shiny::runApp(".")
```

Jalankan app sektoral tertentu:

```r
shiny::runApp("01-pertanian")
shiny::runApp("08-keuangan")
```

Atau load template generik langsung:

```r
source("source.R")
create_market_signal_app("08-keuangan")
```

Untuk mode multi-sektor dari root:

```r
source("source.R")
create_market_signal_app()
```

## Catatan

- Sumber data sepenuhnya bergantung pada Yahoo Finance. Jika quote intraday tidak tersedia, app akan menampilkan fallback seperti `N/A` atau `Quote unavailable`.
- Summary window berbeda per sektor. Sebagian besar sektor memakai 90 hari, sementara sektor tertentu juga menampilkan 360, 90, 30, dan 5 hari.
- Decision layer saat ini bersifat rule-based dan transparan, belum berbasis backtest atau optimasi statistik formal.
- Wrapper per sektor dipertahankan agar struktur deploy lama tetap bisa berjalan tanpa duplikasi logika app.
