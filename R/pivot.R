#' Find index of all stubs in a convo or set of stubs
#'
#' Pivot convo structure to have one list element per stub each containing a
#' numeric vector with the level(s) at which that stub appears (as opposed to one
#' list element per level with entries for each stub).
#'
#' The main use case for this function is validating that stubs do not appear at
#' multiple levels. For this reason, the argument \code{repeats_only} defaults to \code{TRUE}
#' and only stubs existing in multiple levels are returned.
#'
#' @param convo A \code{convo} object or list of stubs by level
#' @param repeats_only Boolean. Whether to return only stubs which exist in multiple levels
#'
#' @return Named list with one element per stubs (of any level) containing a
#'     character vector of all levels at which that stub appears
#' @export
#'
#' @examples
#' convo <- list(letters[1:3], letters[2:4], letters[3:6])
#' pivot_convo(convo)
#' pivot_convo(convo, repeats_only = FALSE)
pivot_convo <- function(convo, repeats_only = TRUE) {

  stubs <-
    if (inherits(convo, "convo")) {
      get_stubs(convo)
    } else {
      convo
    }
  vec_stubs <- unique(unlist(stubs))
  vec_level <-
    unlist(lapply(
      1:length(stubs),
      FUN = function(x)
        rep(x, length(stubs[[x]]))
    ))
  stub_levels <-
    sapply(
      vec_stubs,
      FUN = function(x)
        vec_level[which(vec_stubs == x)],
      USE.NAMES = TRUE,
      simplify = FALSE
    )

  if (repeats_only) {
    n_levels <-
      vapply(stub_levels, FUN = length, FUN.VALUE = numeric(1))
    stub_levels <- stub_levels[n_levels > 1]
  }

  return(stub_levels)

}
