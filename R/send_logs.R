#' Send logs via the Datadog-API.
#'
#' @description
#' \code{send_plain_text()} is an easy to use function to send logs via the Datadog-API.
#'
#' @usage send_plain_text(message_body)
#'
#' @param message_body the message in plain text format.
#' @return response of the Datadog-API call.
#'
#' @details Sending logs in plain text format to the Datadog-API url set with \code{\link{set_api_top_level_domains}}. If no top level
#' domain is specified \code{.eu} will be used.
#'
#' @seealso \code{\link{send_json}}, \code{\link{set_api_top_level_domains}}, \code{\link[httr]{POST}}
#'
#' @examples
#'
#' \dontrun{
#'
#' # send "Hello DDog!"
#' mbody <- "Hello DDog!"
#' send_plain_text(mbody)
#'
#' }
#'

send_plain_text <- function(message_body){
  .send(message_body = message_body, send_type = "plain")
}


#' Send logs via the Datadog-API.
#'
#' @description
#' \code{send_json()} is an easy to use function to send logs in json format via the Datadog-API.
#'
#' @usage send_json(message_body)
#'
#' @param message_body the message in plain text format.
#' @return response of the Datadog-API call.
#'
#' @details Sending logs in json format to the Datadog-API url set with \code{\link{set_api_top_level_domains}}. If no top level
#' domain is specified \code{.eu} will be used.
#'
#' @seealso \code{\link{send_plain_text}}, \code{\link{set_api_top_level_domains}}
#'
#' @examples
#'
#'\dontrun{
#'
#' # simple example with some attributes
#' mock_data <- c(http_status = 200,
#'                hostname = "https://github.com/holzben",
#'                env = "prod")
#' # create json
#' vec_as_json <- jsonlite::toJSON(mock_data)
#'
#' # send the stuff
#' send_json(vec_as_json)
#'
#'
#'
#'
#' # creating some mock data
#' df <- data.frame(
#'   http_status = c(200, 400, 500),
#'   hostname = c("https://github.com/holzben",
#'                "https://github.com/holzben_not",
#'                "https://github.com/holzben_not"),
#'   env = c("prod", "prod", "prod"),
#'   message = c("All Good: https://github.com/holzben_not",
#'               "Ok: https://github.com/holzben_not",
#'               "Not Good: https://github.com/holzben_not"))
#'
#' send_json(jsonlite::toJSON(df))
#'  }
#'


send_json <- function(message_body){
  .send(message_body = message_body, send_type = "json")
}


# helper function
.send <- function(message_body, send_type = c("plain", "json")){
  authentication(set_new = FALSE)

    if(!(send_type %in% c("plain", "json")))
    warning(paste0("Unknown send type type: ", send_type))

  header <-   c("text/plain", "application/json")[c("plain", "json") == send_type]

  url <- paste0(get_api_urls()[1], Sys.getenv(paste0("DATADOG_API_KEY")))
  resp <- httr::POST(url = url,
                    body = message_body,
                    httr::add_headers("Content-Type" = header))

  # keys are contained in url
  resp$url <- ""


  return(resp)

  }
