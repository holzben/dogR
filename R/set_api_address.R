#' API - Address
#'
#' @description
#' The functions saves the API- and the Applicationkey for the corresponding environment (\code{SCHEME} or \code{ACQUIBASE}). For each session the key’s need to be set only once.
#'

#https://docs.datadoghq.com/api/?lang=bash#logs

#"https://api.datadoghq.com/api/v1/logs-queries/list"


#For Datadog US: http-intake.logs.datadoghq.com
#For Datadog EU: http-intake.logs.datadoghq.eu

set_api_top_level_domains <- function(api_tld= c(".eu", ".us")){
  if(length(api_tld) != 1){
    warning("api_tld length > 1 only first argument will be used.")
    api_tld <- api_tld[1]
  }

  if(!(api_tld %in%  c(".eu", ".us")))
    warning(paste0("Unknown top level domain: ", api_tld))

  set_api_url(api_url = stringr::str_interp(
    "https://http-intake.logs.datadoghq${api_tld_in}/v1/input/", list(api_tld_in =api_tld)), api_type = "send")


  set_api_url(api_url = query_url <-stringr::str_interp(
    "https://api.datadoghq${api_tld_in}/api/v1/logs-queries/", list(api_tld_in =api_tld)), api_type = "query")

}

set_api_url <- function(api_url, api_type = c("query", "send")){
  if(length(api_url) != 1){
    warning("api_url length > 1 only first argument will be used.")
    api_url <- api_url[1]
  }

  if(!endsWith(api_url, "/"))
    api_url <- paste0(api_url[1], "/")

  if(api_type == "query")
    Sys.setenv(API_URL_QUERY = api_url)

  if(api_type == "send")
    Sys.setenv(API_URL_SEND = api_url)

  if(!api_type %in%  c("query", "send"))
    stop(paste0("Unknown apy type: ", api_type))

  cat(paste0("Api url set to: ", api_url, "\n"))
}


get_api_urls <- function(){
 return( c(api_send  = Sys.getenv("API_URL_SEND"),
          api_query = Sys.getenv("API_URL_QUERY")))
}

