#' Plot the stability values after [microSTASIS::iterativeClusteringCV()].
#'
#' @description Plot lines connecting the mS score for each subset of the original matrix of paired times.
#'
#' @param pairedTime input matrix with paired times whose stability has being assessed.
#' @param CVklist list resulting from [microSTASIS::iterativeClusteringCV()].
#' @param k integer; number of individuals to subset from the data. The same as used in [microSTASIS::iterativeClusteringCV()].
#' @param points logical; if plotting, FALSE to only plot lines and TRUE to add points on the mS score, i.e. result from [microSTASIS::iterativeClusteringCV()].
#' @param sizeLine numeric; if plotting, size of the multiple lines.
#'
#' @return A line plot in the form of a [ggplot2::ggplot()] object with the values of stability for the multiple subsets and the original matrix of paired samples (points).
#' @importFrom rlang .data
#' @export
#'
#' @examples
#' times <- pairedTimes(data = clr[, 1:20], sequential = TRUE, common = "_0_")
#' mS <- iterativeClustering(pairedTimes = times, parallel = TRUE, common = "_")
#' cv_klist_t1_t25_k2 <- iterativeClusteringCV(pairedTimes = times, 
#'                                             results = mS, name = "t1_t25",
#'                                             common = "_0_", k = 2L, 
#'                                             parallel = TRUE)
#' mSlinesCV(pairedTime = times$t1_t25, CVklist = cv_klist_t1_t25_k2, k = 2L)
mSlinesCV <- function (pairedTime, CVklist,  k = 1L, points = TRUE, 
                       sizeLine = 0.5) {
  individuals <- unique(stringr::str_split(rownames(pairedTime), "_0_", 
                                           simplify = TRUE)[,1])
  if (k == 1L) {
    object <- data.frame(x = stats::reorder(individuals, 
                                            sort(as.character(individuals))), 
                         y = unlist(CVklist), 
                         individual = sort(rep(individuals, length(CVklist))))
  }
  else {
    samples <- seq(1, dim(pairedTime)[1], by = 2)
    CVsamples <- colnames(t(as.data.frame(CVklist)))
    removed_ind <- vapply(seq_along(CVsamples), function(sample) {
      stringr::str_c(individuals[which(samples %in% 
                                         as.vector(stringr::str_split(
                                           CVsamples[sample], ", ", 
                                           simplify = TRUE)))], 
                     collapse = ", ")
    })
    object <- data.frame(x = stats::reorder(removed_ind,
                                            sort(as.character(removed_ind))), 
                         y = unlist(CVklist), 
                         individual = sort(rep(individuals, 
                                               length(CVklist[[1]]))))
  }
  p <- ggplot2::ggplot(object, ggplot2::aes(x = .data$x, y = .data$y, 
                                            group = .data$individual, 
                                            colour = .data$individual)) + 
    ggplot2::geom_line(size = sizeLine) + ggplot2::theme_bw() + 
    ggplot2::labs(x = NULL, y = "Stability") + 
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  if (points) {
    p + ggplot2::geom_point(data = if (k == 1L) {
      object[object$x == object$individual, ]
    } else {
      object[stringr::str_detect(object$x, object$individual), ]
    }, mapping = ggplot2::aes(x = .data$x, y = .data$y, group = 
                                .data$individual, colour = .data$individual))
  }
  else {
    p
  }
}
