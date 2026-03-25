# market-signal

Dashboard Shiny untuk visualisasi saham IDX berbasis data Yahoo Finance via `quantmod`.

Repo ini sekarang memakai satu template app bersama, konfigurasi sektor terpusat, layout dashboard yang dimodernisasi, serta layer analisis rule-based untuk feature extraction, regime classification, rolling context, dan score-based decision output.

## Struktur Repo

- `app.R`: entry point utama di root project, dengan selector sektor dan saham.
- `source.R`: template Shiny generik untuk seluruh app, termasuk layout dashboard, panel metric, chart area, dan rolling summary table.
- `functions.R`: helper untuk data loading, formatting, feature engine, regime classifier, score engine, dan rolling summary calculation.
- `sectors.R`: konfigurasi sektor, daftar saham, horizon rolling summary, dan chart subset.
- `index.html`: landing page statis yang mengarah ke app utama dan wrapper sektoral.
- `01-pertanian/` sampai `09-dagang-jasa-invest/`: wrapper tipis per sektor yang memanggil template generik.

## Fitur Saat Ini

- Chart saham dengan mode `candlesticks`, `matchsticks`, `bars`, atau `line`.
- Overlay teknikal dari `quantmod`:
  - MACD
  - RSI
  - Bollinger Bands
  - SMA(10)
  - EMA(50)
  - SAR
  - volume
- Panel dashboard:
  - `Bias Overview`
  - `Market Snapshot`
  - `Indicators`
  - `Signal Layer`
  - `Regime`
  - `Decision`
  - `Rolling Summary`

## Feature Engine

Feature engine dihitung dari data OHLCV harian dan dipakai oleh regime classifier serta score engine.

Fitur utama yang saat ini dipakai:

- distance ke EMA50
- momentum 5 hari
- ATR%
- range 20 hari
- rasio volume terhadap rata-rata 20 hari
- breakout / breakdown 20 hari
- close position dalam range 20D, 60D, 90D, dan 360D
- range % untuk beberapa window menengah dan panjang

## Regime Classifier

Regime classifier masih rule-based dan transparan.

Regime yang tersedia:

- `bullish_breakout`
- `bearish_breakdown`
- `trend_up`
- `trend_down`
- `high_volatility`
- `compression`
- `range`

## Score-Based Decision Output

Decision layer memakai gabungan:

- posisi harga terhadap EMA20 dan EMA50
- slope EMA50
- struktur SMA20 vs SMA60
- momentum 5 hari
- volume ratio
- breakout / breakdown
- distance dari EMA50
- close position di dalam range beberapa window
- konteks compression / volatility

Output yang ditampilkan:

- skor agregat
- label seperti `strong_bullish`, `bullish`, `neutral`, `bearish`, `strong_bearish`
- action seperti `favor_long_bias`, `watch_long_setups`, `wait`, `defensive`, `avoid_longs`

## Rolling Summary

Rolling summary sekarang ditampilkan dalam format tabel per-window, bukan teks panjang.

Window yang digunakan:

- `5D`
- `10D`
- `15D`
- `20D`
- `30D`
- `60D`
- `90D`
- `120D`
- `240D`
- `360D`

Kolom yang ditampilkan:

- `Low`
- `High`
- `Mean`
- `Range %`
- `Close Pos`
- `Close vs Mean`
- `Vol vs Avg`

Kolom `Range %` dan `Close Pos` juga dipakai untuk memperkaya score engine.

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

- Sumber data sepenuhnya bergantung pada Yahoo Finance.
- Jika quote intraday tidak tersedia, app akan menampilkan fallback seperti `N/A` atau `Quote unavailable`.
- Decision layer saat ini masih rule-based dan belum berbasis backtest formal.
- Wrapper sektoral tetap dipertahankan agar struktur deploy lama tetap bisa jalan tanpa duplikasi logic app.
- Layout dashboard sudah dioptimalkan untuk layar lebih lebar, tetapi tetap responsive untuk tablet dan mobile.
