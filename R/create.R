#' Create convo object from list of stubs
#'
#' @param l List of stubs with one element per level containing a character vector of stubs
#'
#' @return \code{convo} object
#' @export
#'
#' @examples
#' stubs <- list(letters[1:3], letters[4:5], letters[6:7])
#' create_convo(stubs)
create_convo <- function(l) {

  make_stub_list <- function(lvl) {
    sapply(lvl,
           FUN = function(x) list(desc = NULL, valid = NULL), USE.NAMES = TRUE, simplify = FALSE)
  }
  convo_obj <- lapply(l, make_stub_list)
  class(convo_obj) <- c("convo", class(convo_obj))
  return(convo_obj)

}

#' Add new stub to convo object
#'
#' This function adds new stubs to a controlled vocabulary or overwrites existing stubs.
#'
#' @param convo A \code{convo} object
#' @param level Level at which to add new entry
#' @param stub Character. New stub to add to controlled vocabulary at given level
#' @param desc Character. Optional human-readable description of stub
#' @param valid Character vector. Optional \code{pointblank} validation checks for stub
#' @param overwrite Boolean. Whether to allow existing stub to be overwritten
#'
#' @return \code{convo} object
#' @export
#'
#' @examples
#' stubs <- list(letters[1:3], letters[4:5], letters[6:7])
#' convo <- create_convo(stubs)
#' add_convo_stub(convo, 1, "z")
add_convo_stub <- function(convo, level, stub, desc = NA, valid = NA, overwrite = FALSE) {

  # validate inputs ----
  stopifnot(is.numeric(level))
  if (!overwrite & is.element(stub, names(convo[[level]])) ) {
    stop("Provided stub already exists at level provided.",
         "If you wish to overwrite, please set the overwrite argument to FALSE.")
  }

  entry <- list()
  if (!is.na(desc)) entry[["desc"]] <- desc
  if (!is.na(valid)) entry[["valid"]] <- valid
  convo[[level]][[stub]] <- entry
  return(convo)

}

#' Remove stub from convo
#'
#' @param convo A \code{convo} object
#' @param level Level at which to add new entry
#' @param stub Character. Stub to remove from controlled vocabulary at given level
#'
#' @return \code{convo} object
#'
#' @export
#'
#' @examples
#' stubs <- list(letters[1:3], letters[4:5], letters[6:7])
#' convo <- create_convo(stubs)
#' remove_convo_stub(convo, 1, "a")
remove_convo_stub <- function(convo, level, stub) {

  # validate inputs ----
  stopifnot(is.numeric(level))
  stopifnot(is.element(stub, names(convo[[level]])))

  # remove element ----
  convo[[level]][[stub]] <- NULL
  return(convo)

}
