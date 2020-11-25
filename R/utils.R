# internal fx for safe list iteration ----
fmt_safe_chr <- function(x) {ifelse( is.null(x) , NA_character_, x )}
fmt_safe_int <- function(x) {ifelse( is.null(x) , NA_integer_, x)}
fmt_safe_lgl <- function(x) {ifelse( is.null(x), NA, x)}
fmt_safe_date <- function(x) {as.Date( substring( ifelse( is.null(x), NA, x) , 1, 10)) }
fmt_safe_lst <- function(x) {ifelse( is.null(x), NA, x)}

#' @noRd
#' @keywords internal
get_stubs <- function(convo) {lapply(convo, FUN = names)}

#' @noRd
#' @keywords internal
get_desc <- function(convo) {
  lapply(convo,
         FUN = function(x) vapply(x, function(y) fmt_safe_chr(y[["desc"]]), character(1)))
}

#' @noRd
#' @keywords internal
get_valid <- function(convo) {
  lapply(convo,
         FUN = function(x) lapply(x, function(y) y[["valid"]]))
}
