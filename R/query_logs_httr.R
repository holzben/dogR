#' Log files quering
#'
#' @description Send a curl request with parameters to Data Dog API and returns dataframe with log files as rows.
#'
#' @param query the search query using the datadog search syntax.
#' @param time_from the starting point as ISO-8601 string, unix timestamp or in realtive time eg. \code{now -10m, now - 1h, now - 1d}.
#' @param time_to the end point as ISO-8601 string, unix timestamp or in realtive time eg. \code{now -10m, now - 1h, now - 1d}.
#' @param limit the maximum number of result rows, Inf for the maximum.
#' @param sort \code{asc} or \code{dsc}.
#' @param dump_to_file shold the result be saved as \code{.Rdata} file.
#' @param env from which system logs should be quiried \code{SCHEME} or \code{Acquibase}.
#' @param verbose should additional debug data be priinted?
#' @return dogR object.
#' @usage
#' query_logs(query, time_from = "now -1h", time_to = "now", limit = 10, sort = 'desc', dump_to_file = NULL, env = "SCHEME")
#'
#' query_logs_scheme(query, time_from = "now -1h", time_to = "now", limit = 10, sort = 'desc', dump_to_file = NULL)
#'
#' query_logs_acquibase(query, time_from = "now -1h", time_to = "now", limit = 10, sort = 'desc', dump_to_file = NULL)
#'
#' @details The function sends a curl line to the Data Dog API and returns the matching lines. One row of the returning dataframe corresponds to
#' one search result. The rows represent the attributes, same as in the Data Dog interface.
#'
#' @author Benjamin Holzknecht
#' @keywords query, logs
#' @examples
#'
#' \dontrun{
#'
#' query_logs_scheme("Transaction Success service:sdk_api", time_from = "now -1d", limit = 20)
#'
#' # get all from last hour
#' query_logs_scheme("Transaction Success service:sdk_api", time_from = "now -1d", limit = Inf)
#'
#' }





query_logs_httr <- function(query, time_from = "now -1h", time_to = "now", ..., api_tld = ".eu") {
  suppressPackageStartupMessages({
    require("dplyr", quietly = FALSE, warn.conflicts = FALSE)
  })

  # TODO: Check why the shit shows up!!!
  #authentication(set_new = FALSE)
  set_api_top_level_domains(api_tld = api_tld)

  .check_parameters(query = query, time_from = time_from, time_to = time_to, ...)

  req <- .get_httr_request(query = query, time_from = time_from, time_to =time_to, ...)

  return(req)

}


.get_httr_request <- function(query, time_from, time_to, ...){
  url <-  stringr::str_interp(
    paste0(get_api_urls()[2], "list?api_key=${api_key}&application_key=${app_key}"),
    list(api_key = Sys.getenv("DATADOG_API_KEY"),
         app_key = Sys.getenv("DATADOG_APP_KEY")))

  in_body <- c(list(query = query,  time = list(from = time_from, to = time_to)),
               list(...))

  return(httr::POST(url = url, body = in_body, encode = "json"))

}

.check_parameters <- function(query, time_from, time_to, ...){
  req <- any(sapply(c(query, time_from, time_to), FUN = function(x) is.na(x) | is.null(x)))
  if(req)
    warning("Missing or invalid required parameter.")

  nam <- names(list(...))
  kown <- c("index", "limit", "time_timezone", "time.timezone", "time.offset", "time_offset", "startAt", "sort")

  if(any(i <- !(nam %in% kown)))
    warning(paste0("Unknown optional parameter: ", paste(nam[i], collapse = " ")))

}
