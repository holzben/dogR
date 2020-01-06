#' Setting Top Level Domain for the Datadog API Address
#'
#' @description
#' \code{set_api_top_level_domains()} is an easy to use setter function for the Datadog API adress top level doamins \code{.eu} and \code{.eu}.
#'
#' @usage set_api_top_level_domains(api_tld = c(".eu", ".us"), api_type = c("query", "send"))
#'
#' @param api_tld which region, \code{.eu} or \code{.us}.
#' @param api_type which type \code{"query"}, \code{"send"} or both \code{c("query", "send")}.
#'
#' @details Depanding on your geographic location the Datadog API address toplevel domain changes, see also <https://docs.datadoghq.com/api/?lang=bash#api-reference>.
#' Currently following urls are implemented and choosen according to the parmeters, for US:
#' \itemize{
#'  \item{"send"}{<https://http-intake.logs.datadoghq.us/v1/input/>}
#'  \item{"queries"}{<https://api.datadoghq.us/api/v1/logs-queries>}
#' }
#' and for Europa:
#' \itemize{
#'  \item{"send"}{<https://http-intake.logs.datadoghq.eu/v1/input/>}
#'  \item{"queries"}{<https://api.datadoghq.eu/api/v1/logs-queries>}
#' }
#'
#' @author Benjamin Holzknecht
#' @keywords API address, top level domains
#' @seealso set_api_url()
#'
#' @examples
#'
#'\dontrun{
#'
#' # set .eu and sending
#' set_api_top_level_domains(api_tld = ".eu", api_tld = "send")
#'
#' # set .eu for both urls
#' set_api_top_level_domains(api_tld = ".eu")
#'
#' # set .de -> results in Error, unknown top level domain
#' set_api_top_level_domains(api_tld = ".de")
#'
#'
#'}

set_api_top_level_domains <- function(api_tld = c(".eu", ".us"), api_type = c("query", "send")){
  if(length(api_tld) > 1){
    warning("api_tld length > 1 only first argument will be used.")
    api_tld <- api_tld[1]
  }

  if(!(api_tld %in%  c(".eu", ".us"))){
    stop(paste0("Unknown top level domain: ", api_tld))
  }

  if(any(api_type == "send")){
    set_api_url(api_url = stringr::str_interp(
      "https://http-intake.logs.datadoghq${api_tld_in}/v1/input/", list(api_tld_in = api_tld)), api_type = "send")
  }

  if(any(api_type == "send")){
    set_api_url(api_url = stringr::str_interp(
      "https://api.datadoghq${api_tld_in}/api/v1/logs-queries/",   list(api_tld_in = api_tld)), api_type = "query")
  }

}


#' Setting URLs for the Datadog API Address
#'
#' @description
#' \code{set_api_url()} is a straight forward method for setting \code{"query"} and/or \code{"send"} URL addresses.
#'
#' @usage set_api_url(api_url, api_type = c("query", "send"))
#'
#' @param api_url the Datadog API URL.
#' @param api_type which type \code{"query"}, \code{"send"} or both \code{c("query", "send")}.
#'
#' @details Compared to \link{set_api_top_level_domains()} the whole URL must be provided, not only the top level domain.
#' For more details see \link{set_api_top_level_domains()}.
#'
#'
#' @author Benjamin Holzknecht
#' @keywords API address, top level domains
#' @seealso set_api_top_level_domains()
#'
#' @examples
#'
#'\dontrun{
#'
#' # setting EU query url
#' url <- "https://api.datadoghq${api_tld_in}/api/v1/logs-queries/"
#' set_api_url(api_url = url, api_tld = "send")
#'
#'
#'}



set_api_url <- function(api_url, api_type = c("query", "send")){
  if(length(api_type) != 1){
    warning("api_type length > 1 only first argument will be used.")
    api_type <- api_type[1]
  }

  if(length(api_url) != 1){
    warning("api_url length > 1 only first argument will be used.")
    api_url <- api_url[1]
  }

  if(!endsWith(api_url, "/")){
    api_url <- paste0(api_url[1], "/")
  }

  if(api_type == "query"){
    Sys.setenv("API_URL_QUERY" = api_url)
  }

  if(api_type == "send"){
    Sys.setenv("API_URL_SEND" = api_url)
  }

  if(!api_type %in%  c("query", "send")){
    stop(paste0("Unknown apy type: ", api_type))
  }

  cat(paste0("Api url set to: ", api_url, "\n"))
}



#' Getting teh currently set API Addressese
#'
#' @description
#' \code{get_api_urls()} returns the currently set API urls for \code{send} and \code{query} API.
#'
#' @usage get_api_urls()
#' @return a vector with send and query url address or empty string if not set.
#'
#' @details If now address is present the \code{.eu} addresses will set as default.
#'
#' @author Benjamin Holzknecht
#' @keywords API address, top level domains
#' @seealso set_api_url(), set_api_top_level_domains()
#'
#' @examples
#'
#'\dontrun{
#'
#' # get strings length zero -> no addresses set
#' get_api_urls()
#'
#' # set .eu for both urls
#' set_api_top_level_domains(api_tld = ".eu")
#'
#' # nwo get .eu URLs
#' get_api_urls()
#'
#'}


get_api_urls <- function(){
  if(nchar(Sys.getenv("API_URL_SEND") == 0)){
    warning("No API address for SEND is set, seting .eu address.")
    set_api_top_level_domains(api_tld = ".eu", "send")
  }

  if(nchar(Sys.getenv("API_URL_QUERY") == 0)){
    warning("No API address for Query is set, seting .eu address.")
    set_api_top_level_domains(api_tld = ".eu", "query")
  }


  return( c(api_send  = Sys.getenv("API_URL_SEND"),
            api_query = Sys.getenv("API_URL_QUERY")))
}




