#' Title
#'
#' @param uid
#'
#' @return
#' @import httr
#' @export
#'
#' @examples
clowder_load_file <- function(uid = NULL) {
  if (!is.null(getOption("CLOWDER_APIKEY"))) {
    download <- paste0(getOption("CLOWDER_BASEURL"), "/files/", uid, "/blob", "?key=", getOption("CLOWDER_APIKEY"))
    dat <- content(httr::GET(download))
  }
  if (!is.null(getOption("CLOWDER_USER")) & !is.null(getOption("CLOWDER_PASS"))) {
    download <- paste0(getOption("CLOWDER_BASEURL"), "/files/", uid, "/blob")
    dat <- httr::content(httr::GET(
      download,
      httr::authenticate(getOption("CLOWDER_USER"), getOption("CLOWDER_PASS"))
    ), encoding = "UTF-8")
  }

  data.table::fread(dat)
}
