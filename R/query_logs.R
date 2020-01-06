#' Log files quering - httr
#'
#' @description
#' \code{query_logs_httr(query)} Send a httr POST request Data Dog API.
#'
#' @param query the search query using the datadog search syntax.
#' @param time_from the starting point as ISO-8601 string, unix timestamp or in realtive time eg. \code{now -10m, now - 1h, now - 1d}.
#' @param time_to the end point as ISO-8601 string, unix timestamp or in realtive time eg. \code{now -10m, now - 1h, now - 1d}.
#' @param ... optional parameters.
#' @return httr reuest including headers and body.
#'
#' @details More details on all optional parameters can be found at the Datadog API description: <https://docs.datadoghq.com/api/?lang=bash#logs-indexes>.
#'
#' For the request the addresses spesified with \link{set_api_top_level_domains()} are used.
#'
#' @usage
#' query_logs(query, time_from = "now -1h", time_to = "now")
#'
#' @author Benjamin Holzknecht
#' @seealso query_logs_df, set_api_top_level_domains, send_logs,
#' @keywords query, logs
#' @examples
#'
#' \dontrun{
#'
#' query_logs_httr("Transaction Success service:sdk_api", time_from = "now -1d", limit = 20)
#'
#' # get all from last hour
#' query_logs_httr("Transaction Success service:sdk_api", time_from = "now -1d", limit = Inf)
#'
#' }

query_logs_httr <- function(query, time_from = "now -1h", time_to = "now", ...) {
  suppressPackageStartupMessages({
    require("dplyr", quietly = FALSE, warn.conflicts = FALSE)
  })

  # TODO: Check why the shit shows up!!!
  #authentication(set_new = FALSE)
  .check_parameters(query = query, time_from = time_from, time_to = time_to, ...)

  req <- .get_httr_request(query = query, time_from = time_from, time_to = time_to, ...)

  return(req)

}


#' Log files quering - df
#'
#' @description
#' \code{query_logs_df(query)} sends a httr POST request via Data Dog API and returns a \code{dplyr::data_frame()}.
#'
#' @param query the search query using the datadog search syntax.
#' @param time_from the starting point as ISO-8601 string, unix timestamp or in realtive time eg. \code{now -10m, now - 1h, now - 1d}.
#' @param time_to the end point as ISO-8601 string, unix timestamp or in realtive time eg. \code{now -10m, now - 1h, now - 1d}.
#' @param ... optional parameters.
#' @return \code{dplyr::data_frame()} with the result data.
#'
#' @details Only the query result will be returned, for metadata and request headers use \link{query_logs_httr}
#' More details on all optional parameters can be found at the Datadog API description: <https://docs.datadoghq.com/api/?lang=bash#logs-indexes>.
#'
#' For the request the addresses spesified with \link{set_api_top_level_domains()} are used.
#'
#'
#' @usage
#' query_logs_df(query, time_from = "now -1h", time_to = "now")
#'
#' @author Benjamin Holzknecht
#' @seealso query_logs_httr, set_api_top_level_domains, send_logs,
#' @keywords query, logs
#' @examples
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
#' query_logs_df("Transaction Success service:sdk_api", time_from = "now -1d", limit = 20)
#'
#' # get all from last hour
#' query_logs_df("Transaction Success service:sdk_api", time_from = "now -1d", limit = Inf)
#'
#' }



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



