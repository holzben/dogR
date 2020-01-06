#' Send logs via the Datadog API
#'
#' @description
#' \code{send_plain_text()} is an easy to use function to send logs via the Datadog API.
#'
#' @usage send_plain_text(message_body)
#'
#' @param message_body the message in plain text format.
#' @return responce of the API call.
#'
#' @details Sending logs in plain text format to the API url set with \code(\Link{set_api_top_level_domains()}). If no top level
#' domain is specified \code{.eu} will be used.
#'
#' @seealso send_plain_json(), set_api_top_level_domains()
#'
#' @examples
#'
#'\dontrun{
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


#' Send logs via the Datadog API
#'
#' @description
#' \code{send_json()} is an easy to use function to send logs in json format via the Datadog API.
#'
#' @usage send_json(message_body)
#'
#' @param message_body the message in plain text format.
#' @return responce of the API call.
#'
#' @details Sending logs in json format to the API url set with \code(\Link{set_api_top_level_domains()}). If no top level
#' domain is specified \code{.eu} will be used.
#'
#' @seealso send_plain_text(), set_api_top_level_domains()
#'
#' @examples
#'
#'\dontrun{
#'
#' # simple example with some attrigutes
#' mock_data <- c(http_status = 200,
#'                hostname = "https://github.com/holzben",
#'                env = "prod")
#' # creat json
#' vec_as_json <- jsonlite::toJSON(mock_data)
#'
#' # send the stuff
#' send_json(vec_as_json)
#'
#'
#'
#'
#' # creating some mock data and send it in dplyr syntax
#' data.frame(
#'   http_status = c(200, 400, 500),
#' hostname = c("https://github.com/holzben",
#'                "https://github.com/holzben_not",
#'                "https://github.com/holzben_not"),
#'   env = c("prod", "prod", "prod"),
#'   message = c("All Good: https://github.com/holzben_not",
#'               "Ok: https://github.com/holzben_not",
#'               "Not Good: https://github.com/holzben_not")) %>%
#'   jsonlite::toJSON() %>%
#'   send_json()
#'  }
#'


send_json <- function(message_body){
  .send(message_body = message_body, send_type = "json")
}



# helper function
.send <- function(message_body, send_type = c("plain", "json"), api_tld){
  # TODO: Why the shit shows up!
  authentication(set_new = FALSE)
  set_api_top_level_domains(api_tld = api_tld)

  if(!(send_type %in% c("plain", "json")))
    warning(paste0("Unknown send type type: ", send_type))

  header <-   c("text/plain", "application/json")[c("plain", "json") == send_type]

  url <- paste0(get_api_urls()[1], Sys.getenv(paste0("DATADOG_API_KEY")))
  return(httr::POST(url = url,
                    body = message_body,
                    httr::add_headers("Content-Type" = header)))
}










