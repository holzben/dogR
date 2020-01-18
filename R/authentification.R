#' Datadog-API - Authentication.
#'
#' @description
#' \code{authentication()} saves the API- and the APPlication-Key. For each session the key’s need to be set only once.
#'
#' @usage authentication(set_new = FALSE)
#'
#' @param set_new \code{TRUE} for setting nev credentials.
#'
#' @return \code{"Keys saved."}
#' @details For each session the key’s will saved in an environment variable, use \code{set_new = TRUE} for entering new credentials.
#' For each key pair the length will be checked, Dataadog API-Key is expected to have 32 and the Datadog Application-Key is
#' expected to have 40 characters.
#' A warning massage occur if the length differs.
#'
#' @author Benjamin Holzknecht
#' @keywords authentication
#' @examples
#'
#'\dontrun{
#' authentication(set_new = TRUE)
#'}



# sets environment variable for authentication
# set_new if new user wants to enter new credentials
authentication <- function(set_new = FALSE){
  if(!.is_key_length_ok() | set_new){
    Sys.setenv(DATADOG_API_KEY = .ask_for_secret("API Key: "))
    Sys.setenv(DATADOG_APP_KEY = .ask_for_secret("Application Key: "))

    if(!.is_key_length_ok())
      warning("Key length ok?.", call. = FALSE)

    return("Keys saved.")

  }

}


# opens up password prompt if RStudio-API
# is used otherwise console is used
.ask_for_secret <- function(prompt){
  if(rstudioapi::isAvailable()) {
    rstudioapi::askForPassword(prompt)
  }else {
    cat(prompt)
    readLines(n = 1L)
  }
}


# checks length of API and APP key
.is_key_length_ok <- function(){
  return(
      nchar(Sys.getenv("DATADOG_API_KEY")) == 32 &&
      nchar(Sys.getenv("DATADOG_APP_KEY")) == 40
  )
}
