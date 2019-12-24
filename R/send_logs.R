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





send_json <- function(message_body){
  .send(message_body = message_body, send_type = "json")

}


send_plain_text <- function(message_body){
  .send(message_body = message_body, send_type = "plain")
}

#curl -X POST https://http-intake.logs.datadoghq.eu/v1/input/8edd5ab1390db4ce8240e8c088e1672c \
.send <- function(message_body, send_type = c("plain", "json")){
  # TODO: Why the shit shows up!
  #authentication(set_new = FALSE)
  if(!(send_type %in% c("plain", "json")))
    warning(paste0("Unknown send type type: ", send_type))

  header <-   c("text/plain", "application/json")[c("plain", "json") == send_type]

  url <- paste0(get_api_urls()[1], Sys.getenv(paste0("DATADOG_API_KEY")))
  return(httr::POST(url = url,
             body = message_body,
             httr::add_headers("Content-Type" = header)))
}


