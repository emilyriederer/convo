#' Pretty print a convo object
#'
#' @param x A \code{convo} object
#' @param ... Additional arguments passed to function. Currently unused.
#'
#' @return No return - pretty prints convo object
#' @export
#'
#' @examples
#' convo <- create_convo(list(letters[1:3], letters[4:6]))
#' print(convo)
print.convo <- function(x, ...) {

  l <- x
  titles <- paste("Level", 1:length(l))
  values <- vapply(
    lapply(get_stubs(l), function(x) paste("-", x)),
    FUN = function(x) paste(unique(x), collapse = "\n"),
    FUN.VALUE = character(1))
  combin <- paste0(titles, "\n", values, "\n")
  cat(paste(combin, collapse = ""))

}

#' Pretty print a convomin object
#'
#' \code{convomin} objects are returned by \code{compare_convo()} and \code{evaluate_convo()}
#'
#' @param x A \code{convomin} object (a multi-level list of stub names)
#' @param ... Additional arguments passed to function. Currently unused.
#'
#' @return No return - pretty prints convomin object
#' @export
#'
#' @examples
#' x <- c("a_b")
#' convo <- parse_stubs(x)
#' print(convo)
print.convomin <- function(x, ...) {

  l <- x
  titles <- paste("Level", 1:length(l))
  values <- vapply(
    lapply(l, function(x) paste("-", x)),
    FUN = function(x) paste(unique(x), collapse = "\n"),
    FUN.VALUE = character(1))
  combin <- paste0(titles, "\n", values, "\n")
  cat(paste(combin, collapse = ""))

}

