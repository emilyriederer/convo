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

  stop_suggest("yaml", "write_convo()")
  convo_obj <- create_convo(convo_min)
  yaml::write_yaml(convo_obj, file = file.path(path, filename))

}




