#' API - Authentication
#'
#' @description
#' \code{authentication()} saves the API- and the Applicationkey. For each session the key’s need to be set only once.
#'
#' @usage authentification(set_new = FALSE)
#'
#' @param set_new \code{TRUE} if new credentials should be entered.
#'
#' @return \code{"Keys saved."}
#' @details For each session the key’s will only set once per environment, use \code{set_new = TRUE} for entering new credentials.
#' For each key pair the lenght will be checked, the API Key is expacted to have 32 and the Application Key is expacted to have 40 characters.
#' A warning massage occure if the lenght differs.

#'
#' @author Benjamin Holzknecht
#' @keywords authentication
#' @examples
#'
#'\dontrun{
#' authentication(set_new = TRUE)
#'}



# sets enviorment variable for authentification
# set_new if new user wants to enter new credentials
authentication <- function(set_new = FALSE){
  if(!.is_key_length_ok() | set_new){
    Sys.setenv(DATADOG_API_KEY = .ask_for_secret("API Key: "))
    Sys.setenv(DATADOG_APP_KEY = .ask_for_secret("Application Key: "))

    if(!.is_key_length_ok())
      warning("Authentification failed.")

    return("Keys saved.")

  }

}


# opens up password promt if RStudio API
# is used oderwise console is used
.ask_for_secret <- function (prompt){
  if (rstudioapi::isAvailable()) {
    rstudioapi::askForPassword(prompt)
  }
  else {
    cat(prompt)
    readLines(n = 1L)
  }
}


# cheks length of API and APP key
.is_key_length_ok <- function(){
  return(
      nchar(Sys.getenv("DATADOG_API_KEY")) == 32 &&
      nchar(Sys.getenv("DATADOG_APP_KEY")) == 40
  )
}

