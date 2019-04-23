# a collection of R functions to download stock index data

...works only in predefined specific setting. Not robust at all!

As of March 3, 2019 automatic download works for:

* Quandl
* Yahoo Finance
* S&P Dow Jones Indices (https://us.spindices.com)
* BNP (relies on `library(RSelenium)` and Firefox Browser, not very stable)
* STOXX
* Ossiam
* SIX (various specific parameters implemented in 4 functions)
* ICE
* Vienna
* FTSE


Automatic download no longer works for:

* MSCI Indices (has to be manually downloaded from https://www.msci.com/end-of-day-data-country)
* HFR (has to be downloaded manually from https://www.hedgefundresearch.com/download-hfrx-index-performance-data registration required)




System: Ubuntu 16.04, R 3.5.2, Firefox Quantum 65.0

## Notes


* The running `main.R` will read a spreadsheet file `Basiswerte.xlsx`, which predefines the tickers of interest to me. However, the download functions can also be run independently of it.

* For **Quandl** a API key is required. You can get it in your personal Quandl.com settings. See the *setup* part of `main.R` to read it. The key could be stored in a file `quandlkey.private`.
* Error `Error in curl::curl_fetch_disk(url, x$path, handle = handle) : SSL certificate problem: unable to get local issuer certificate` could be solved with https://stackoverflow.com/a/54660072
* Error `Error in wdman::selenium(port = port, verbose = verbose, version = version,  : Selenium server signals port = 4566 is already in use.`: no solution found yet, https://stackoverflow.com/a/43993442 didn't help

# PCA use case

See in `PCA` directory. Published on RPubs here: http://rpubs.com/mgei/pca-usecase
