#' Cluster stubs by level to find duplicates
#'
#' This function clusters name stubs by level to find potential
#' typos or similarities between names in the \code{convo}. Note that this is a rough,
#' exploratory tool but due to the complexity of human language and the variety of potential
#' stubs, it is not a foolproof method of determining duplication. This is illustrated in
#' the documentation's examples in which the clustering of level 1 stubs does not
#' highlight the desired relationship ("AMT" versus "AMOUNT") whereas it does highlight
#' the level 2 duplication ("ACCOUNT" versus "ACCT" versus "ACCNT").
#'
#' The distance matrix is calculated using Levenshtein (edit) distance as implemented in
#' \code{utils::adist}. By default, the cost of insertion and deletion operations is set
#' lower to that of substitution since, in this case, we are more likely to wish to
#' identify redundancies caused by increasing levels of abbreviation. Thus, we prefer
#' to consider as more similiar those stubs which are pure subsets of other stubs. This
#' weighting can be controlled by the \code{adist_costs} argument.
#'
#' The clustering is done using hierarchical clustering as implemented in \code{stats::hclust()}.
#' By default, the agglomeration method is "single", but this can be altered with the argument
#' \code{hclust_method}.
#'
#' @param convo A \code{convo} object or list of stubs by level
#' @param adist_costs Relative costs of insertion, deletion, and substitution
#'   passed to the \code{costs} argument of \code{adist()}. Must be named vector with elements
#'   named \code{insertion}, \code{deletion}, and \code{substituion} or partial matches.
#' @param hclust_method Agglomeration method passed to the \code{method} argument of \code{hclust()}
#'
#' @return A list of \code{hclust} objects by level of the vocabulary
#' @export
#'
#' @importFrom utils adist
#' @importFrom stats as.dist
#'
#' @examples
#' convo <- list(c("IND", "IS", "AMT", "AMOUNT", "CAT", "CD"),
#'               c("ACCOUNT", "ACCT", "ACCNT", "PROSPECT", "CUSTOMER"))
#' clusts <- cluster_convo(convo)
#' plot(clusts[[1]])
#' plot(clusts[[2]])
#'
#' stubs <- parse_stubs(c("IND_ACCOUNT", "AMT_ACCT", "ID_ACCNT", "DT_LOGIN", "DT_ENROLL"))
#' clusts <- cluster_convo(stubs, adist_costs = c(ins = 10, del = 10, sub = 1))
#'
cluster_convo <- function(convo,
                          adist_costs = c(ins = 1, del = 1, sub = 5),
                          hclust_method = "single") {

  # if full convo object provided, extract only stubs ----
  stubs <- if (inherits(convo, "convo")) {get_stubs(convo)} else {convo}

  # stop if user-provided inputs to adist or hclust fail ----
  if (is.null(names(adist_costs))) {
    stop("Invalid argument to adist_costs",
         "Must be named vector with names partially matching",
         "insertion, deletion, and substituion.",
         "See ?utils::adist documentation of costs argument for details.")
  }

  match.arg(hclust_method,
            c("ward.D", "ward.D2", "single", "complete",
              "average", "mcquitty", "median", "centroid"))

  # compute levenshtein distance, favoring ins/del over sub ----
  dists <- lapply(stubs,
                  FUN = function(x) utils::adist(x, costs = adist_costs))
  dists <- mapply(FUN = function(x,y) {rownames(x) <- y; return(x)},
                  x = dists, y = stubs, SIMPLIFY = FALSE)
  clusts <- lapply(dists,
                   FUN = function(x) tryCatch(stats::hclust(stats::as.dist(x), method = hclust_method),
                                              error = function(e) NA))

  # return list of clusters by level ----
  return(clusts)

}
