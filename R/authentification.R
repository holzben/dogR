#' API - Authentication
#'
#' @description
#' The functions saves the API- and the Applicationkey for the corresponding environment (\code{SCHEME} or \code{ACQUIBASE}). For each session the key’s need to be set only once.
#'
#' @usage authentification(env = "SCHEME", set_new = FALSE)
#'
#' @param env which enviorment, \code{SCHEME} or \code{ACQUIBASE}.
#' @param set_new \code{TRUE} if new credentials should be entered.
#'
#' @details For each session the key’s will only set once per environment, use \code{set_new = TRUE} for entering new credentials.
#' @author Benjamin Holzknecht
#' @keywords authentication
#' @examples
#'
#'\dontrun{
#' authentication(env = "SCHEME", set_new = TRUE)
#'}



# sets enviorment variable for authentification
# set_new if new user wants to enter new credentials
authentication <- function(set_new = FALSE){
  if(!.is_key_length_ok() | set_new){
    Sys.setenv(DATADOG_API_KEY = .ask_for_secret("API Key: "))
    Sys.setenv(DATADOG_APP_KEY = .ask_for_secret("Application Key: s"))

    if(!.is_key_length_ok())
      warning("Authentification failed.")

    return("Keys saved.")

  }

}


# opens up password promt if RStudio API
# is used oderwise console is used
.ask_for_secret <- function (prompt){
  # (c)  https://yutannihilation.github.io/K9/
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

