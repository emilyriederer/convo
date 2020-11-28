#' Create dictionary defining controlled vocabulary realizations
#'
#' @param vars Character vector of variable names
#' @param convo Controlled vocabulary object
#' @param desc_str Prototypical string from which to create variable definition
#'
#' @return \code{data.frame} data dictionary
#' @export
#'
#' @examples
#' vars <- c("AMT_A_2019", "IND_B_2020")
#' filepath <- system.file("", "ex-convo.yml", package = "convo")
#' convo <- read_convo(filepath)
#' describe_names(vars, convo, desc_str = "{level1} of {level2} in given year")
describe_names <- function(vars, convo, desc_str = "{level1} of entity {level2}") {

  stop_suggest("glue", "describe_names()")

  vars_df <- parse_df(vars)
  desc <- unlist(setNames(get_desc(convo), NULL))
  desc_str_mod <- gsub("\\{", "\\{desc[", desc_str)
  desc_str_mod <- gsub("\\}", "]\\}", desc_str_mod)
  desc_out <- glue::glue_data(vars_df, desc_str_mod)
  vars_df$desc <- desc_out
  return(vars_df)

}


#' Create schema dictionary
#'
#' @param convo \code{convo} object containing controlled vocabulary
#' @param include_valid Boolean whether to include human-readable documentation of related data validations
#' @param for_DT Boolean whether intended table will be displayed with \code{DT} package. Used for applying breaklines
#'
#' @return \code{data.frame} schema dictionary
#' @export
#'
#' @importFrom stats aggregate
#'
#' @examples
#' filepath <- system.file("", "ex-convo.yml", package = "convo")
#' convo <- read_convo(filepath)
#' describe_convo(convo)
describe_convo <- function(convo, include_valid = FALSE, for_DT = TRUE) {

  descs <- get_desc(convo)
  stubs <- get_stubs(convo)
  level <- 1:length(stubs)
  nstub <- vapply(stubs, FUN = length, FUN.VALUE = numeric(1))

  desc_df <- data.frame(
    level =      rep(level, nstub),
    stub =       unlist(stubs),
    stub_desc  = unlist(descs)
  )
  row.names(desc_df)

  if (include_valid) {

    stop_suggest("pointblank", "describe_convo() with include_valid = TRUE")

    df <- setNames(as.data.frame(matrix(1, ncol = length(convo[[1]]))), names(convo[[1]]))
    agent <- create_pb_agent(convo, df)
    x_list <- pointblank::get_agent_x_list(agent)
    validation_set <- x_list$validation_set
    validation <- validation_set[c('column', 'brief')]
    validation$column <- as.character(validation$column)
    validation$brief <- gsub("(in `[A-Za-z]+` )|(`[A-Za-z]+` )", "", validation$brief)
    sep <- if (for_DT) "<br/>" else ";"
    valid_df <- stats::aggregate(brief ~ column, data = validation, FUN = function(x) paste(x, collapse = sep))
    names(valid_df) <- c("stub", "checks")
    cmbnd_df <- merge(x = desc_df, y = valid_df, by = "stub", all.x = TRUE)
    desc_df <- cmbnd_df[order(cmbnd_df$level), c("level", "stub", "stub_desc", "checks")]

  }

  return(desc_df)

}
