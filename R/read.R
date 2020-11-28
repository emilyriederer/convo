#' Read controlled vocabulary from YAML
#'
#' @param path Path to YAML file containing \code{convo} specification
#'
#' @return Controlled vocabulary as \code{convo} (list) object
#' @export
#'
#' @examples
#' path <- system.file("", "ex-convo.yml", package = "convo")
#' convo <- read_convo(path)
read_convo <- function(path) {

  stop_suggest("yaml", "read_convo()")
  l <- yaml::read_yaml(file = path)
  class(l) <- c("convo", class(l))
  return(l)

}
