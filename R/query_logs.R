#' Log files quering - httr.
#'
#' @description
#' \code{query_logs_httr()} Send a httr POST request Datadog-API.
#'
#' @param query the search query using the Datadog search syntax.
#' @param time_from the starting point as ISO-8601 string, Unix timestamp or in relative time eg. \code{now -10m, now - 1h, now - 1d}.
#' @param time_to the end point as ISO-8601 string, Unix timestamp or in relative time eg. \code{now -10m, now - 1h, now - 1d}.
#' @param ... optional parameters.
#' @return httr request including headers and body.
#'
#' @details More details on all optional parameters can be found at the Datadog-API
#' description: \href{https://docs.datadoghq.com/api/?lang=bash#logs-indexes}{Datadog-API}
#'
#' For the request the addresses specified with \code{\link{set_api_top_level_domains}} are used.
#'
#' By default maximum 10 logs will be returned per response, higher numbers can be set via the \code{limit} parameter with
#' a maximum of \code{limit = 1000}.
#'
#' @usage
#' query_logs_httr(query, time_from = "now -1h", time_to = "now", ...)
#'
#' @author Benjamin Holzknecht
#' @seealso \code{\link{query_logs_df}}, \code{\link{set_api_top_level_domains}}, \code{\link{send_json}}
#' @keywords query logs
#' @examples
#'
#' \dontrun{
#'
#' query_logs_httr("POST 200", time_from = "now -1d")
#'
#' # get all from last hour
#' query_logs_httr("POST 200", time_from = "now -1d", limit = 30)
#'
#'
#' # limtit = 1000 --> maximum per response
#' query_logs_httr("POST 200", time_from = "now -1d", limit = 1000)
#'
#' # to get the same result as query_logs_df
#' response <- query_logs_httr("POST 200", time_from = "now -1d", limit = 1000)
#' response <- jsonlite::fromJSON(httr::content(response, type = "text"), flatten = TRUE)
#'
#' # cast to df
#' as.data.frame(response$logs)
#'
#' }

query_logs_httr <- function(query, time_from = "now -1h", time_to = "now", ...) {
  authentication(set_new = FALSE)
  .check_parameters(query = query, time_from = time_from, time_to = time_to, ...)

  req <- .get_httr_request(query = query, time_from = time_from, time_to = time_to, ...)

  return(req)

}


#' Log files quering - df.
#'
#' @description
#' \code{query_logs_df()} sends a httr POST request via
#' Datadog-API and returns a \code{data.frame()}.
#'
#' @param query the search query using the Datadog search syntax.
#' @param time_from the starting point as ISO-8601 string, Unix timestamp or in relative time eg. \code{now -10m, now - 1h, now - 1d}.
#' @param time_to the end point as ISO-8601 string, Unix timestamp or in relative time eg. \code{now -10m, now - 1h, now - 1d}.
#' @param ... optional parameters.
#' @param include_meta_data should Datadog query metadata be included?
#' @return \code{data.frame()} with the result data or an empty \code{data.frame()}.
#'
#' @details Only the query result will be returned, for metadata and request headers use \code{\link{query_logs_httr}}.
#' More details on all optional parameters can be found at the Datadog API description: \href{https://docs.datadoghq.com/api/?lang=bash#logs-indexes}{Datadog-API}
#'
#' For the request the addresses specified with \code{\link{set_api_top_level_domains}} are used.
#'
#' By default maximum 10 logs will be returned per response, higher numbers can be set via the \code{limit} parameter with
#' a maximum of \code{limit = 1000}.
#'
#' In case no results are found or invalid parameters are used for the request, an empty \code{data.frame()} will
#' be returned.
#'
#' For more details about the httr response see \href{https://cran.r-project.org/web/packages/httr/vignettes/quickstart.html}{httr-package.}
#'
#' @usage
#' query_logs_df(query, time_from = "now -1h", time_to = "now", ...,
#'               include_meta_data = FALSE)
#'
#' @author Benjamin Holzknecht
#' @seealso \code{\link{query_logs_httr}}, \code{\link{set_api_top_level_domains}}, \code{\link{send_json}}
#' @keywords query logs
#' @examples
#'
#' \dontrun{
#'
#' query_logs_df("POST 200", time_from = "now -1d")
#'
#' # get all from last hour
#' query_logs_df("POST 200", time_from = "now -1d", limit = 30)
#'
#' # with ISO 8601 date-time format
#' query_logs_df("POST 200", time_from = "2020-01-18T13:00Z", limit = 1)
#'
#' # limtit = 1000 --> maximum per response
#' query_logs_df("POST 200", time_from = "now -1d", limit = 1000)
#'
#'
#' }

query_logs_df <- function(query, time_from = "now -1h", time_to = "now", ...,
                          include_meta_data = FALSE) {
  req <- query_logs_httr(query, time_from, time_to, ...)

  if(req$status_code != 200){
    warning(req, call. = FALSE)
    return(data.frame())
  }

  parsed <- jsonlite::fromJSON(httr::content(req, type = "text", encoding = "UTF-8"), flatten = TRUE)
  parsed$logs <- as.data.frame(parsed$logs)

  if(include_meta_data)
    return(parsed)

  return(parsed$logs)

}


.get_httr_request <- function(query, time_from, time_to, ...){
  url <-  stringr::str_interp(
    paste0(get_api_urls()[2], "list?api_key=${api_key}&application_key=${app_key}"),
    list(api_key = Sys.getenv("DATADOG_API_KEY"),
         app_key = Sys.getenv("DATADOG_APP_KEY")))

  in_body <- c(list(query = query,  time = list(from = time_from, to = time_to)),
               list(...))

  # remove url since keys are included
  req <- httr::POST(url = url, body = in_body, encode = "json")
  req$url <- ""
  return(req)

}

.check_parameters <- function(query, time_from, time_to, ...){
  req <- any(sapply(c(query, time_from, time_to), FUN = function(x) is.na(x) | is.null(x)))
  if(req)
    warning("Missing or invalid required parameter.", call. = FALSE)

  nam <- names(list(...))
  kown <- c("index", "limit", "time_timezone", "time.timezone", "time.offset", "time_offset", "startAt", "sort")

  if(any(i <- !(nam %in% kown)))
    warning(paste0("Unknown optional parameter: ", paste(nam[i], collapse = " ")))

}
