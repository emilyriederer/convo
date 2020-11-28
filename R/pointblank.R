#' Generate YAML for pointblank validation agent from convo
#'
#' @param convo \code{convo} object read from YAML
#' @param col_names Character vector of column names
#' @param level Numeric. Level of controlled vocabulary containing validation checks
#' @param filename Name of YAML file to create
#' @param path Optional path to directory in which YAML file should be created
#'
#' @return Does not return. Call function for side-effect of writing file.
#' @export
#'
#' @importFrom stats setNames
#' @importFrom utils capture.output
#'
#' @examples
#' \dontrun{
#' filepath <- system.file("", "ex-convo.yml", package = "convo")
#' convo <- read_convo(filepath)
#' write_pb(convo, c("IND_A", "AMT_B"), filename = "convo-validation.yml", path = ".")
#' }
write_pb <- function(convo, col_names, level = 1, filename = "convo-validation.yml", path = ".") {

  stop_suggest("pointblank", "write_pb()")

  stubs_funs_step <- get_pb_lines(convo, level)

  df_code <-
    sprintf("setNames(as.data.frame(matrix(1, ncol = %d)), c( %s))",
            length(col_names),
            paste0("'", col_names, "'", collapse = ",")
    )
  pb_fun_call <- paste0("yaml_write(filename = '", filename, "', path = '", path, "')")
  code_lines <- c(paste0("create_agent(read_fn = ~", df_code,")"),
                  stubs_funs_step,
                  pb_fun_call)
  code_lines <- paste0("pointblank::", code_lines)
  code <- paste(code_lines, collapse = " %>% ")

  eval(parse(text = code))

}



#' Generate agent object for validation of controlled vocabulary conditions for a data set
#'
#' @inheritParams write_pb
#' @param tbl The input table passed to \code{pointblank::create_agent()}
#'
#' @return \code{pointblank} agent for validation pipeline
#' @export
#'
#' @examples
#' filepath <- system.file("", "ex-convo.yml", package = "convo")
#' convo <- read_convo(filepath)
#' agent <- create_pb_agent(convo, data.frame(IND_A = 1, IND_B = 5, DT_B = as.Date("2020-01-01")))
create_pb_agent <- function(convo, tbl, level = 1) {

  stop_suggest("pointblank", "create_pb_agent()")

  stubs_funs_step <- get_pb_lines(convo, level)
  code_lines <- c(paste0("create_agent(tbl)"), stubs_funs_step)
  code_lines <- paste0("pointblank::", code_lines)
  code <- paste(code_lines, collapse = " %>% ")
  eval(parse(text = code))

}

#' @noRd
#' @keywords internal
get_pb_lines <- function(convo, level) {

  stubs <- names(convo[[level]])
  funs  <- lapply(convo[[level]], function(x) x[["valid"]])
  no_funs <- vapply(funs, is.null, logical(1))

  if (all(no_funs)) {stop("convo object supplied has no validation checks at level provided")}
  stubs <- stubs[!no_funs]
  funs <- funs[!no_funs]

  edit_expect <- function(stub, fun) {
    stub_prep <- paste0("(", "matches('", "^([A-Za-z]_){", level-1 ,"}", stub, "')")
    stub_prep <- ifelse(!grepl("\\(\\)", fun), paste0(stub_prep, ", "), stub_prep)
    final <- sub("\\(", stub_prep, fun)
    return(final)
  }
  stubs_funs <- mapply(edit_expect,
                       stub = rep(stubs, times = vapply(funs, length, numeric(1))),
                       fun = unlist(funs))
  stubs_funs_step <- vapply(1:length(stubs_funs),
                            FUN = function(x) gsub("\\)$", paste0(", step_id = ", x, ")"), stubs_funs[x]),
                            FUN.VALUE = character(1))
  return(stubs_funs_step)

}
