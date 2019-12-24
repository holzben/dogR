



query_logs_df <- function(query, time_from = "now -1h", time_to = "now", ..., include_meta_data = FALSE, api_tld = ".eu") {
  suppressPackageStartupMessages({
    require("dplyr", quietly = FALSE, warn.conflicts = FALSE)
  })

  req <- query_logs_httr(query, time_from, time_to, ..., api_tld)

  if(req$status_code != 200){
    warning(req)
    return(as_tibble())
}

  parsed <- jsonlite::fromJSON(httr::content(req, type = "text"), flatten = TRUE)
  parsed$logs <- dplyr::as_tibble(parsed$logs)

  if(include_meta_data)
    return(parsed)

  return( parsed$logs)

}








