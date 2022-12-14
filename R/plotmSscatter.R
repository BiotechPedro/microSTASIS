#' Plot a scatter and side boxplot of the stability results.
#'
#' @param results input data.frame resulting from [microSTASIS::mSpreviz()].
#' @param times a vector with the names of each paired time, e.g. "t1_t2".
#' @param order NULL object or character: mean or median; if the individuals 
#'         should be sorted by any of those statistics of the stability values.
#' @param gridLines logical; FALSE to print a blank background or TRUE to 
#'         include a gray grid.
#' @param sideScale numeric; scale of the side boxplot.
#'
#' @return A scatter plot and a side boxplot of the stability values in the 
#'         form of a [ggplot2::ggplot()] object.
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' data(clr)
#' times <- pairedTimes(data = clr, sequential = TRUE, common = "_0_")
#' mS <- iterativeClustering(pairedTimes = times, common = "_")
#' results <- mSpreviz(results = mS, times = times)
#' plotmSscatter(results = results, order = "median", times = c("t1_t25", 
#'                                                          "t25_t26"), 
#'           gridLines = TRUE, sideScale = 0.2)
plotmSscatter <- function(results, order = NULL, times, gridLines = FALSE, 
                      sideScale = 0.3){
  results <- results[, c(1, which(colnames(results) %in% times))]
  results[, 1] <- factor(results[, 1])
  manual.melt <- data.frame(individual = rep(results[, 1], 
                                             length(colnames(results)) - 1),
                            variable = factor(rep(colnames(results)[-1], 
                                                  each = nrow(results))),
                            value = unlist(results[, -1]))
  rownames(manual.melt) <- seq_len(nrow(results) * 
                                     (length(colnames(results)) - 1))
  if (!is.null(order)) {
    manual.melt$individual <- factor(x = manual.melt$individual,
                                     levels = results$individual[
                                       as.integer(results[order(
                                     apply(results[, -1], 1, function(x) {
                                       if (order == "median") {
                                         stats::median(x, na.rm = TRUE)
                                       } else if (order == "mean") {
                                         mean(x, na.rm = TRUE)
                                       }
                                     }), decreasing = FALSE), 1])], 
                                     ordered = TRUE)
  }
  ggplot2::ggplot(manual.melt, ggplot2::aes(y = .data$individual, 
                                            x = .data$value)) +
    ggplot2::geom_point(ggplot2::aes(color = .data$variable), size = 3) + 
    ggplot2::theme_bw() + ggplot2::xlim(0, 1) + 
    ggplot2::labs(y = "Individual", x = "Stability") +
    ggside::geom_xsideboxplot(ggplot2::aes(y = .data$variable, 
                                           fill = .data$variable), 
                              orientation = "y") + 
    ggplot2::theme(legend.position = "none", 
                   legend.title = ggplot2::element_blank(),
                   panel.grid.major.x = if (gridLines){
                     ggplot2::element_line(colour = "gray95")} else {
                       ggplot2::element_blank()
                     }, panel.grid.major.y = if (gridLines){
                       ggplot2::element_line(colour = "gray90")} else {
                         ggplot2::element_blank()
                       }, axis.ticks = ggplot2::element_blank(),
                   ggside.panel.scale = sideScale)
}
