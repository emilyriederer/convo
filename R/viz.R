#' Visualize name set as tree
#'
#' @param convo_df \code{data.frame} with variable names split by level
#' @export
#'
#' @examples
#' vbls <- c("a_1_x", "a_2", "a_3", "a_4", "b_1", "b_2")
#' vbls_df <- parse_df(vbls)
#' viz_names(vbls_df)
viz_names <- function(convo_df) {

  stop_suggest("collapsibleTree", "viz_names()")
  cols <- names(convo_df)
  cols <- sort(cols[grepl("level\\d", cols)])
  collapsibleTree::collapsibleTree(convo_df,
                  hierarchy = cols,
                  nodeSize = "leafCount")

}

