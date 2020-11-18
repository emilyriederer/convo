#' Evaluate a set of names against a controlled vocabulary
#'
#' @param convo Controlled vocabulary object or list of stub names by level
#' @param vbl_names Names to evaluate
#'
#' @return Returns \code{convo} object (list) of violating names by level
#' @export
#'
#' @examples
#' convo <- list(c("ind"), letters[1:3], c("pre", "post"))
#' vbl_names <- c("ind_a", "ind_d", "amt_c", "cat_c_pre", "cat_c_post")
#' evaluate_convo(convo, vbl_names)
evaluate_convo <- function(convo, vbl_names) {

  if (inherits(convo, "convo")) {convo <- get_stubs(convo)}

  convo_regex <-
    vapply(1:length(convo),
           FUN = function(x) to_regex(convo, x),
           FUN.VALUE = character(1)
    )

  invalids <- lapply(convo_regex,
                     FUN = function(x) vbl_names[!grepl(x[[1]], vbl_names)]
  )

  class(invalids) <- c("convomin", class(invalids))
  return(invalids)

}

#' @keywords internal
#' @noRd
to_regex <- function(l, i, sep = "_") {

  # construct full-length matches ----
  stubs_string <-
    vapply(l,
           FUN = function(x) paste0("(",paste0(x, collapse = "|"),")"),
           FUN.VALUE = character(1)
    )
  wildcard <- paste0("([A-Za-z0-9]*", sep, ")")
  terminal <- paste0("($|", rep("_", (i > 1)), stubs_string[i],")")
  regex <-
    if (i > 1) {
      paste0("^",
             wildcard, "{", i-2, "}",
             "[A-Za-z0-9]*",
             terminal,
             "($|_)")
    }
  else {paste0("^", stubs_string[1], "($|_)") }

  # allow acceptance if fewer levels ----
  shorter <- "^"
  if (i > 1) {
    shorter <- paste0(shorter, "[A-Za-z0-9]+(_[A-Za-z0-9]+){0,",i-2,"}")
  }
  shorter <- paste0(shorter, "$")

  # combine regexes ----
  final_regex <- paste0("(", shorter, "|", regex,")")

  return(final_regex)

}
