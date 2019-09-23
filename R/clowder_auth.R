#' Setup authentication parameters for connecting to clowder
#'
#' @param user username used to login to clowder
#' @param pass password
#' @param apikey optional used preferentially over username/pass
#' @param baseurl the url used to access clowder api e.g. clowder.ncsa.illinois.edu/clowder/api
#'
#' @details use clowder auth to save credentials for use with clowder functions
#'
#' @examples
#'\dontrun{
#'clowder_auth(user = "exampleuser@examplemail.com",
#'             pass = "pass",
#'             baseurl = "https://clowder.ncsa.illinois.edu/clowder/api")
#'}
#'
#' @export
clowder_auth <- function(user = NULL, pass = NULL, apikey = NULL, baseurl){
  check <- function(x) length(x) == 1 && is.character(x)
  setop <- function(x) {
    xn <- deparse(substitute(x))
    if (is.na(x)) x <- NA_character_
    if (check(x)) {
      p <- list(x)
      names(p) <- paste0("CLOWDER_", toupper(xn))
      do.call(what = options, p)
    } else {
      warning("Invalid '", xn, ",' no changes made to CLOWDER_", toupper(xn))
    }
  }


  if (!is.null(user)) setop(user)
  if (!is.null(pass)) setop(pass)
  if (!is.null(apikey)) setop(apikey)
  if (!is.null(baseurl)) setop(baseurl)
}
