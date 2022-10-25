#' Generate boxplots of the stability dynamics throughout sampling times 
#' by groups.
#'
#' @param results input data.frame resulting from [microSTASIS::mSpreviz()].
#' @param groups vector with the same length as individuals, i.e. the number 
#'         of rows in the [microSTASIS::mSpreviz()] output.
#' @param points logical; FALSE to only visualize boxplots or TRUE to also add
#'         individual points.
#' @param linetype numeric; type of line to connect the median value of 
#'         paired times; 0 to avoid the line.
#'
#' @return A plot with as many boxes as paired times by group in the form of a 
#'         [ggplot2::ggplot()] object.
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' data(clr)
#' times <- pairedTimes(data = clr, sequential = TRUE, common = "_0_")
#' mS <- iterativeClustering(pairedTimes = times, common = "_")
#' results <- mSpreviz(results = mS, times = times)
#' metadata <- data.frame(Sample = rownames(clr), age = c(rep("youth", 65), 
#'                        rep("old", 131-65)))
#' group <- mSmetadataGroups(metadata = metadata, samples = metadata$Sample, 
#'                           common = "_0_", individuals = results$individual, 
#'                           variable = "age")
#' plotmSdynamics(results, groups = group, points = TRUE, linetype = 0)
plotmSdynamics <- function(results, groups, points = TRUE, linetype = 2){
    results$group <- groups
    manual.melt <- data.frame(individual = rep(results[, 1], 
                                               length(colnames(results)) - 2),
                              group = rep(results$group, 
                                          length(colnames(results)) - 2),
                              variable = factor(rep(colnames(results)[
                                - c(1, ncol(results))], 
                                                    each = nrow(results))),
                              value = unlist(results[- c(1, ncol(results))]))
    rownames(manual.melt) <- seq_len(nrow(results) * 
                                       (length(colnames(results)) - 2))
    plot <- ggplot2::ggplot(manual.melt, ggplot2::aes(x = .data$variable, 
                                                      y = .data$value)) +
      ggplot2::stat_summary(fun = stats::median, geom = "line", 
                            ggplot2::aes(group = .data$group, 
                                         color = .data$group),
                            linetype = linetype, size = 1.3) +
      ggplot2::geom_boxplot(ggplot2::aes(fill = .data$group), 
                            outlier.shape = NA,
                            width = 0.2, colour = "black", alpha = 0.6) +
      ggplot2::labs(x = NULL, y = "Stability") + ggplot2::theme_classic() +
      ggplot2::theme(legend.position = "top", 
                     legend.title = ggplot2::element_blank(),
                     axis.text.x = ggplot2::element_text(
                       margin = ggplot2::margin(t = 2.5, r = 0, b = 0, l = 0)),
                     axis.text.y = ggplot2::element_text(
                       margin = ggplot2::margin(t = 0, r = 5, b = 0, l = 0)),
                     axis.ticks = ggplot2::element_line(size = 0.5),
                     axis.ticks.length.y = grid::unit(0.25, "cm"),
                     axis.ticks.length.x = grid::unit(0.25, "cm"))
    if (points) {
      plot + ggplot2::geom_jitter(ggplot2::aes(fill = .data$group),
                                  position = ggplot2::position_jitterdodge(
                                    jitter.width = 0.15),
                                  size = 2, pch = 21, alpha = 0.75)
    } else {plot}
}
