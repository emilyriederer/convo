#' Compare vocabularies with set operations
#'
#' @param vocab1 Vocabulary list (or `convo` object). In the case of validating a new vocabulary
#'     against an existing one, this argument should take the new vocabulary.
#' @param vocab2 Vocabulary list (or `convo` object). In the case of validating a new vocabulary
#'     against an existing one, this argument should take the existing vocabulary.
#' @param fx Comparison function. `setdiff` returns those in `vocab1` not in `vocab2`; `intersect`
#'     returns those in both `vocab1` and `vocab2`; `union` returns all elements in either
#'
#' @return List (of class `convomin`) of mismatches
#' @export
#'
#' @examples
#' v1 <- list(letters[1:3], letters[4:6], letters[7:9])
#' v2 <- list(letters[(1:3)+1], letters[(4:6)-2], letters[7])
#' compare_convo(v1, v2, setdiff)
#' compare_convo(v1, v2, union)
#' compare_convo(v1, v2, intersect)
compare_convo <- function(vocab1, vocab2, fx = c("setdiff", "intersect", "union")) {

  # if full convo object provided, extract only stubs ----
  if (inherits(vocab1, "convo")) {vocab1 <- get_stubs(vocab1)}
  if (inherits(vocab2, "convo")) {vocab2 <- get_stubs(vocab2)}

  # standardize lengths ----
  max_len <- max(length(vocab1), length(vocab2))
  if (max_len > length(vocab1)) {vocab1[(length(vocab1)+1):max_len] <- NA}
  if (max_len > length(vocab2)) {vocab2[(length(vocab2)+1):max_len] <- NA}

  # retrieve correct comparison fx ----
  fx <- switch(fx,
               setdiff = convo_setdiff,
               intersect = convo_intersect,
               union = convo_union)

  # make comparison ----
  stubs <-
    mapply(FUN = function(x, y) fx(x,y),
         x = vocab1, y = vocab2,
         SIMPLIFY = FALSE)
  class(stubs) <- c("convomin", class(stubs))

  return(stubs)

}

#' @noRd
#' @keywords internal
convo_intersect <- function(x, y) {

  y <- paste0("^",y,"$")
  matches <- lapply(y, FUN = function(pattern) grepl(pattern, x))
  valids <- lapply(matches, function(ind) x[ind])
  unique(unlist(valids))

}

#' @noRd
#' @keywords internal
convo_union <- function(x, y){

  y <- paste0("^",y,"$")
  matches <- lapply(y, FUN = function(pattern) grepl(pattern, x))
  valids <- lapply(1:length(matches),
                   FUN = function(i) c(gsub("\\^|\\$", "", y[i]),
                                       x[!matches[[i]]])
                   )
  unique(unlist(valids))

}

#' @noRd
#' @keywords internal
convo_setdiff <- function(x, y){

  y <- paste0("^",y,"$")
  matches <- lapply(y, FUN = function(pattern) grepl(pattern, x))
  match_inds <- vapply(do.call(Map, c(f = c, matches)),
                       FUN = function(x) as.logical(max(x)),
                       FUN.VALUE = logical(1))
  valids <- x[!match_inds]
  unique(unlist(valids))

}
