#' Rename computed columns based on controlled vocabulary
#'
#' Often when columns are operated on, their former name will no longer correctly indicate their
#' properties. For example, when binary indicator variables are summed, they become integer count
#' variables instead.
#'
#' This function allows users to specify the \code{rename} property in their \code{convo} to
#' explain how certain variables ought be renamed when transformed. It plays particularly nicely
#' with the automatic renaming conventions of \code{dplyr} scoped operators, as shown in the second example.
#'
#' @param vbls Character vector of variable names to transform
#' @param convo \code{convo} object with at least one vocabulary item with \code{rename} property
#'
#' @return Character vector of column names renamed per \code{convo} specification
#' @export
#'
#' @examples
#' path <- system.file("", "ex-convo.yml", package = "convo")
#' convo <- read_convo(path)
#' vbl_names <- c("IND_A_AVG", "IND_B_SUM")
#' rename_convo(vbl_names, convo)
#'
#'
#' \dontrun{
#' data.frame(
#'   IND_A = sample(0:1, 10, replace = TRUE),
#'   IND_B = sample(0:1, 10, replace = TRUE)
#'   ) %>%
#'     summarize_all(list(SUM = sum, AVG = mean)) %>%
#'     rename_all(~rename_convo(., convo))
#' }
rename_convo <- function(vbls, convo) {

  vbl_names <- vbls
  rules <- get_rename(convo)
  for (i in 1:length(rules)) {

    r <- rules[[i]]
    regex <- make_regex(r)
    subfx <- make_sub_fx(r)
    tochg <- grepl(regex, vbl_names)
    vbl_names[tochg] <- subfx(vbl_names[tochg])

  }
  return(vbl_names)

}

#' @noRd
#' @keywords internal
make_sub_fx <- function(rule) {

  sub_fx <- function(vbl){
    vbl_nosuff <- gsub(paste0("_", rule[2]), "", vbl)
    vbl_newpre <- gsub(rule[1], rule[3], vbl_nosuff)
    return(vbl_newpre)
  }

  return(sub_fx)

}

#' @noRd
#' @keywords internal
make_regex <- function(rule) {

  paste0("^", rule[1], "_[A-Z]*_", rule[2], "$")

}
