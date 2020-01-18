# dogR
The aim of the package is to provide a wrapper for the [Datadog-API](https://docs.datadoghq.com/api/):

This R package provides functions for:
* Managing Datadog-Api credentials in an R session
* Managing top level domains of the API Address
* Sending new logs to Datadog as plain text `send_plain()` of JSON Objects `send_json()`
* Quering logs and converting them to a `data.frame()` via `query_logs_df()` or return the whole [httr](https://httr.r-lib.org/index.html) responce

# Installation
Install from CRAN (TBA)


or via github:
```{r, eval=FALSE}
# install.packages("devtools")
devtools::install_github("holzben/dogR")
