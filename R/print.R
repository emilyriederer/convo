#' Pretty print a convo object
#'
#' @param l A \code{convo} object
#'
#' @return No return - pretty prints convo object
#' @export
#'
#' @examples
#' l <- c("a_b")
#' convo <- parse_stubs(l)
#' print(convo)
print.convo <- function(l) {

  titles <- paste("Level", 1:length(l))
  values <- vapply(
    lapply(get_stubs(l), function(x) paste("-", x)),
    FUN = function(x) paste(unique(x), collapse = "\n"),
    FUN.VALUE = character(1))
  combin <- paste0(titles, "\n", values, "\n")
  cat(combin)

}
