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
#' This function adds new stubs to a controlled vocabularly or overwrites existing stubs.
#'
#' @param convo A \code{convo} object
#' @param level Level at which to add new entry
#' @param stub Character. New stub to add to controlled vocabularly at given level
#' @param desc Character. Optional human-readable description of stub
#' @param valid Character vector. Optional \code{pointblank} validation checks for stub
#'
#' @return \code{convo} object
#' @export
#'
#' @examples
#' stubs <- list(letters[1:3], letters[4:5], letters[6:7])
#' convo <- create_convo(stubs)
#' add_convo_stub(convo, 1, "z")
add_convo_stub <- function(convo, level, stub, desc = NA, valid = NA) {

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
#' @param stub Character. New stub to add to controlled vocabularly at given level
#'
#' @return
#' @return \code{convo} object
#'
#' @examples
#' stubs <- list(letters[1:3], letters[4:5], letters[6:7])
#' convo <- create_convo(stubs)
#' remove_convo_stub(convo, 1, "a")
remove_convo_stub <- function(convo, level, stub) {

  convo[[level]][[stub]] <- NULL
  return(convo)

}
