#' Pipe operator
#'
#' See \code{\link[magrittr]{\%>\%}} for more details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

#' Arrumar nomes
#'
#' @export
arrumar_nomes <- function(x) {
  nm <- names(x)
  names(x) <- gsub("\\(a\\)", "", gsub(" +", "_", tolower(desacentuar(nm))))
  x
}

#' Desacentuar
#'
#' @export
desacentuar <- function (x) {
  gsub("`|\\'", "", iconv(x, to = "ASCII//TRANSLIT"))
}
