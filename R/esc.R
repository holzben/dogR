#' Escape sequence
#'
#' @description
#' \code{esc()} escapes " for DataDog log queries.
#'
#' @usage esc(string)
#'
#' @param str a string which shoud be under ".
#' @return an escaped string.
#'
#' @details The function can be used if " would be necessary in Datadog. This is the case if a exact match
#' of the string sequence should be searched.
#'
#' @author Benjamin Holzknecht
#' @keywords query, ewcape sequences
#' @examples
#'
#'esc("200 POST")
#'

esc <- function(str){
  return(paste0("\\\"", str, "\\\""))
}


#' Escape sequence
#'
#' @description
#' \code{quote()} escapes " for DataDog log queries.
#'
#' @usage quote(string)
#'
#' @param str a string which shoud be under ".
#' @return an escaped string.
#'
#' @details The function can be used if " would be necessary in Datadog. This is the case if a exact match
#' of the string sequence should be searched.
#'
#' @author Benjamin Holzknecht
#' @keywords query, ewcape sequences
#' @examples
#'
#'quote("200 POST")
#'


quote <- function(str){
  return(paste0("\"", str, "\""))
}
