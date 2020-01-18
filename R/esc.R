#' Escape sequence.
#'
#' @description
#' \code{quote()} escapes " for Datadog log queries.
#'
#' @usage quote(str)
#'
#' @param str a string which should be under ".
#' @return an escaped string.
#'
#' @details The function can be used if " would be necessary in Datadog. This is the case if a exact match
#' of the string sequence should be searched.
#'
#' @author Benjamin Holzknecht
#' @keywords query, escape sequences
#' @examples
#'
#'quote("200 POST")
#'


quote <- function(str){
  return(paste0("\"", str, "\""))
}
