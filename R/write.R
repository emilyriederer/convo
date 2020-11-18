#' Write convo config file template from stubs list
#'
#' @param convo_min List of stubs with one element per level containing a character vector of stubs
#' @param filename Desired filename. Defaults to \code{convo-config.yaml}
#' @param path Desired filepath. Defaults to current directory
#'
#' @return Does not return. Call function for side-effect of writing file.
#' @export
#'
#' @examples
#' \dontrun{
#' valid_stubs <- list(letters[1:3], letters[4:5], letters[6:7])
#' write_convo(valid_stubs)
#' convo <- read_convo("convo-config.yml")
#' print(convo)
#' evaluate_convo(convo, c("a_d", "a_e_g", "c_b", "d"))
#' }
write_convo <- function(convo_min, filename = "convo-config.yml", path = ".") {

  if(!requireNamespace("yaml", quietly = TRUE)) {
    stop(
      "The package 'yaml' is required to use function convo::write_convo()",
      "Please install from CRAN and retry."
    )
  }

  make_stub_list <- function(lvl) {
    sapply(lvl,
           FUN = function(x) list(desc = NULL, valid = NULL), USE.NAMES = TRUE, simplify = FALSE)
  }
  convo_obj <- lapply(convo_min, make_stub_list)
  yaml::write_yaml(convo_obj, file = file.path(path, filename))

}




