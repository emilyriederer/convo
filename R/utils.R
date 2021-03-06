# internal fx for safe list iteration ----
fmt_safe_chr <- function(x) {ifelse( is.null(x) , NA_character_, x )}
fmt_safe_int <- function(x) {ifelse( is.null(x) , NA_integer_, x)}
fmt_safe_lgl <- function(x) {ifelse( is.null(x), NA, x)}
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

#' @noRd
#' @keywords internal
get_rename <- function(convo) {

  names(convo) <- NULL
  renames <-
    lapply(convo,
           FUN = function(x) lapply(x, function(y) y[["rename"]]))
  renames <- unlist(renames, FALSE)
  renames <- renames[vapply(renames, length, numeric(1)) != 0]
  unlist(lapply(1:length(renames),
                function(y) lapply(renames[[y]], function(x) c(names(renames)[y], x[["when"]], x[["then"]]))),
         recursive = FALSE)

}

#' @noRd
#' @keywords internal
stop_suggest <- function(pkg, fun) {

  if(!requireNamespace(pkg, quietly = TRUE)) {
    stop(
      "The package '", pkg, "' is required to use the convo function ", fun,
      ". Please install from CRAN and retry."
    )
  }

}
