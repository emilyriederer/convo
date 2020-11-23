#' Decompose name into component parts
#'
#' @param names Names created by vocabulary as character vector
#' @param sep Separator between different levels of name
#'
#' @keywords internal
#' @noRd
parse_decomp <- function(names, sep) {

  if (sep == ".") {sep <- "\\."}
  names_split <- strsplit(names, split = sep)
  return(names_split)

}

#' Parse vocabulary from a set of names
#'
#' @inheritParams parse_decomp
#' @return List of class `convomin` to represent controlled vocabulary
#' @export
#'
#' @examples
#' vbl_names <- c("ind_a", "ind_b", "amt_a", "amt_c", "cat_c_pre", "cat_c_post")
#' parse_stubs(vbl_names, sep = "_")
parse_stubs <- function(names, sep = "_") {

  names_split <- parse_decomp(names, sep)
  max_l <- max(vapply(names_split, FUN = length, FUN.VALUE = integer(1)))
  stubs <- lapply(1:max_l,
                  FUN = function(x) {
                    vapply(names_split,
                           FUN = function(l) l[x],
                           FUN.VALUE = character(1))}
                  )
  stubs_clean <- lapply(stubs, FUN = function(x) x[!is.na(x)])
  stubs_distn <- lapply(stubs_clean, FUN = unique)

  class(stubs_distn) <- c("convomin", class(stubs_distn))

  return(stubs_distn)

}

#' Parse vocabulary from a set of names and convert to data.frame
#'
#' @inheritParams parse_decomp
#' @return A data.frame with one row per name and one column per vocabulary level
#' @export
#'
#' @examples
#' vbl_names <- c("ind_a", "ind_b", "amt_a", "amt_c", "cat_c_pre", "cat_c_post")
#' parse_df(vbl_names, sep = "_")
parse_df <- function(names, sep = "_") {

  names_split <- parse_decomp(names, sep)
  max_l <- max(vapply(names_split, FUN = length, FUN.VALUE = integer(1)))
  names_split_eqi <- lapply(names_split, FUN = function(x) c(x, rep(NA, max_l - length(x))))
  names_split_df <- do.call(rbind, names_split_eqi)
  names_split_df <- as.data.frame(names_split_df)
  colnames(names_split_df) <- paste0("level", 1:ncol(names_split_df))
  names_split_df <- cbind(list(var_name = names), names_split_df)
  rownames(names_split_df) <- NULL

  return(names_split_df)

}

