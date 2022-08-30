#' Stability of individuals after iteratively performing Hartigan-Wong k-means clustering.
#'
#' @description Perform Hartigan-Wong [stats::kmeans()] algorithm as many times as possible. The values of k are from 2 to the number of samples minus 1. Those individuals whose paired samples are clustered under the same label sum 1. If paired samples are in different clusters, then sum 0, except when the euclidean distance between them is smaller to the ones of each sample to its centroid. This is done for all possible values of k and, finally, divided the sum by k, so obtaining a value between 0 and 1.
#'
#' @param pairedTimes list of matrices with paired times, i.e. samples to be stressed to multiple iterations.
#' @param parallel logical; FALSE to sequentially run the internal loop or TRUE to do it by parallel computing.
#' @param common pattern that separates the ID and the sampling time.
#'
#' @return µSTASIS stability score (mS) for the individuals from the corresponding paired times.
#' @export
#'
#' @examples
#' times <- pairedTimes(data = clr, sequential = TRUE, common = "_0_")
#' mS <- iterativeClustering(pairedTimes = times, parallel = TRUE, common = "_0_")
iterativeClustering <- function(pairedTimes, parallel = TRUE, common) {
  kmeansList <- BiocParallel::bplapply(pairedTimes, function(pairedTimesMatrix){
    lapply(2:(dim(pairedTimesMatrix)[1] - 1), function(k) {
      stats::kmeans(pairedTimesMatrix, k, iter.max = 20, nstart = 50)
    })}, BPPARAM = if(parallel) {BiocParallel::bpparam()
    } else {BiocParallel::SerialParam(progressbar = TRUE)}
  )
  individualsList <- lapply(pairedTimes, function(pairedTimesMatrix) {
    unique(stringr::str_split(rownames(pairedTimesMatrix), common, simplify = TRUE)[,1])
  })
  results <- lapply(names(kmeansList), function(pairedTime){
    stability <- data.frame(matrix(0, nrow = length(individualsList[[pairedTime]]), ncol = length(kmeansList[[pairedTime]])))
    stability <- as.data.frame(lapply(seq_along(kmeansList[[pairedTime]]), function(klist){
      unlist(lapply(seq_along(individualsList[[pairedTime]]), function(individual){
        indSamples <- which(stringr::str_detect(names(kmeansList[[pairedTime]][[klist]]$cluster), individualsList[[pairedTime]][individual]))
        if (kmeansList[[pairedTime]][[klist]]$cluster[indSamples][1] ==
            kmeansList[[pairedTime]][[klist]]$cluster[indSamples][2]){
          1
        } else if (stats::dist(rbind(pairedTimes[[pairedTime]][individual,], 
                                     kmeansList[[pairedTime]][[klist]]$centers[
                                       kmeansList[[pairedTime]][[klist]]$cluster[individual],])) >
                   stats::dist(rbind(pairedTimes[[pairedTime]][individual,], 
                                     pairedTimes[[pairedTime]][individual + 1,])) &
                   stats::dist(rbind(pairedTimes[[pairedTime]][individual + 1,],
                                     kmeansList[[pairedTime]][[klist]]$centers[
                                       kmeansList[[pairedTime]][[klist]]$cluster[individual + 1],])) >
                   stats::dist(rbind(pairedTimes[[pairedTime]][individual,], 
                                     pairedTimes[[pairedTime]][individual + 1,]))){
          1
        } else {0}
      }))
    }))
    rownames(stability) <- individualsList[[pairedTime]]
    colnames(stability) <- 1 + (seq_along(kmeansList[[pairedTime]]))
    stability
  })
  names(results) <- names(kmeansList)
  lapply(results, function(stabilityMatrix) round(apply(stabilityMatrix, 1, sum) / length(stabilityMatrix), 3))
}
