#' Plot a heatmap of the stability results.
#'
#' @param results input data.frame resulting from [microSTASIS::mSpreviz()].
#' @param order NULL object or character: none, mean or median; if the individuals should be sorted by any of those statistics of the stability values.
#' @param times character; names of the paired times to plot, i.e. colnames of results.
#' @param label logical; FALSE to avoid printing the mS score or TRUE to print it.
#' @param low color for the lowest value.
#' @param mid color for the middle value.
#' @param high color for the highest values.
#' @param midpoint value to situate the middle.
#'
#' @return A heatmap of the stability values in the form of a [ggplot2::ggplot()] object.
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' times <- pairedTimes(data = clr, sequential = TRUE, common = "_0_")
#' mS <- iterativeClustering(pairedTimes = times, parallel = TRUE, common = "_0_")
#' results <- mSpreviz(results = mS, times = times)
#' mSheatmap(results = results, order = "mean", times = c("t1_t25", "t25_t26"), label = TRUE)
mSheatmap <- function(results, order = NULL, times, label = FALSE, low = "red2", mid = "yellow",
                       high = "forestgreen", midpoint = 0.5) {
  results <- results[, c(1, which(colnames(results) %in% times))]
  results[, 1] <- factor(results[, 1])
  manual.melt <- data.frame(individual = rep(results[, 1], length(colnames(results)) - 1),
                            variable = factor(rep(colnames(results)[-1], each = dim(results)[1])),
                            value = unlist(results[,-1]))
  rownames(manual.melt) <- 1:(dim(results)[1] * (length(colnames(results)) - 1))
  if (!is.null(order)) {
    manual.melt$individual <- factor(x = manual.melt$individual,
                                     levels = results$individual[as.integer(results[order(
                                       apply(results[, -1], 1, function(x) {
                                         if (order == "median") {
                                           stats::median(x, na.rm = TRUE)
                                         } else if (order == "mean") {
                                           mean(x, na.rm = TRUE)
                                         }
                                       }), decreasing = FALSE), 1])], ordered = TRUE)
  }
  plot <- ggplot2::ggplot(manual.melt, ggplot2::aes(y = .data$individual, x = .data$variable)) +
    ggplot2::geom_tile(ggplot2::aes(fill = .data$value)) +
    ggplot2::scale_fill_gradient2(high = high, mid = mid, midpoint = midpoint,
                         low = low, na.value = 'white') +
    ggplot2::theme_void() + ggplot2::guides(fill = ggplot2::guide_colorbar(title.position = 'top',
                                                title.hjust = 0.5,
                                                barwidth = grid::unit(10, 'lines'),
                                                barheight = grid::unit(0.5, 'lines'))) +
    ggplot2::theme(legend.position = 'top', legend.title = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 2.5, r = 0, b = 0, l = 0)),
          axis.text.y = ggplot2::element_text(margin = ggplot2::margin(t = 0, r = 5, b = 0, l = 0)),
          axis.ticks = ggplot2::element_line(size = 0.5),
          axis.ticks.length.y = grid::unit(0.25, "cm"),
          axis.ticks.length.x = grid::unit(0.25, "cm"))
  if (label) {
    plot + ggplot2::geom_text(ggplot2::aes(label = .data$value))
  } else {
    plot
  }
}
