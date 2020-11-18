#' Create dictionary defining controlled vocabulary realizations
#'
#' @param vars Character vector of variable names
#' @param convo Controlled vocabulary object
#' @param desc_str Prototypical string from which to create variable definition
#'
#' @return \code{data.frame} data dictionary
#' @export
#'
#' @import glue
#' @examples
#' vars <- c("AMT_A_2019", "IND_B_2020")
#' filepath <- system.file("", "ex-convo.yml", package = "convo")
#' convo <- read_convo(filepath)
#' describe_names(vars, convo, desc_str = "{level1} of {level2} in given year")
describe_names <- function(vars, convo, desc_str = "{level1} of entity {level2}") {

  vars_df <- parse_df(vars)
  desc <- unlist(setNames(convo:::get_desc(convo), NULL))
  desc_str_mod <- gsub("\\{", "\\{desc[", desc_str)
  desc_str_mod <- gsub("\\}", "]\\}", desc_str_mod)
  desc_out <- glue::glue_data(vars_df, desc_str_mod)
  vars_df$desc <- desc_out
  return(vars_df)

}


#' Create schema dictionary
#'
#' @param convo \code{convo} object containing controlled vocabulary
#'
#' @return \code{data.frame} schema dictionary
#' @export
#'
#' @examples
#' filepath <- system.file("", "ex-convo.yml", package = "convo")
#' convo <- read_convo(filepath)
#' describe_convo(convo)
describe_convo <- function(convo) {

  descs <- get_desc(convo)
  stubs <- get_stubs(convo)
  level <- 1:length(stubs)
  nstub <- vapply(stubs, FUN = length, FUN.VALUE = numeric(1))

  desc_df <- data.frame(
    level =      rep(level, nstub),
    stub =       unlist(stubs),
    stub_desc  = unlist(descs)
  )

  return(desc_df)

}
