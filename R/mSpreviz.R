#' Process the [microSTASIS::iterativeClustering()] output to a new format ready for the implemented visualization functions.
#'
#' @param results list; output of [microSTASIS::iterativeClustering()].
#' @param times list; output of [microSTASIS::pairedTimes()].
#'
#' @return A data frame ready for its use under the implemented visualization functions and others.
#' @export
#'
#' @examples
#' times <- pairedTimes(data = clr, sequential = TRUE, common = "_0_")
#' mS <- iterativeClustering(pairedTimes = times, parallel = TRUE, common = "_0_")
#' results <- mSpreviz(results = mS, times = times)
mSpreviz <- function(results, times){
  if (length(results) == length(names(times))) {
    individual <- sort(unique(unlist(lapply(results, names))))
    byTimes <- lapply(seq_along(results), function(timePoint) {
      resultsByTime <- data.frame(names(results[[timePoint]]), results[[timePoint]])
      colnames(resultsByTime) <- c("ind", names(times)[timePoint])
      resultsByTime
    })
    resultslist <- as.data.frame(lapply(seq_along(byTimes), function(resultsByTime) {
      merge(data.frame(individual), byTimes[[resultsByTime]], by.x = "individual", by.y = "ind", 
            all.x = TRUE, all.y = FALSE, sort = TRUE)
      }))
    if(dim(resultslist)[2] > 2) {
      resultslist[, -seq(3, dim(resultslist)[2] - 1, 2)]
    } else{
      resultslist
    }
  } else {
    stop("Both arguments must have the same length")
  }
}
