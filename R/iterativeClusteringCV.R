#' Cross validation of the iterative Hartigan-Wong k-means clustering.
#'
#' @description Perform cross validation in the way of leave-one-out (LOO) or k-fold of the stability results from [microSTASIS::iterativeClustering()].
#'
#' @param pairedTimes list of matrices with paired times, i.e. samples to be stressed to multiple iterations.
#' @param results the list output of [microSTASIS::iterativeClustering()].
#' @param name character; name of the paired times whose stability is being assessed.
#' @param common pattern that separates the ID and the sampling time.
#' @param k integer; number of individuals to remove from the data for each time running [microSTASIS::iterativeClustering()].
#' @param parallel logical; FALSE to sequentially run the internal loop or TRUE to do it by parallel computing.
#'
#' @return Multiple lists with multiple objects of class "kmeans".
#' @export
#'
#' @examples
#' times <- pairedTimes(data = clr[, 1:20], sequential = TRUE, common = "_0_")
#' mS <- iterativeClustering(pairedTimes = times, parallel = TRUE, common = "_")
#' cv_klist_t1_t25_k2 <- iterativeClusteringCV(pairedTimes = times, 
#'                                             results = mS, name = "t1_t25",
#'                                             common = "_0_", k = 2L, 
#'                                             parallel = TRUE)
iterativeClusteringCV <- function(pairedTimes, results, name, common, k = 1L, 
                                  parallel = TRUE) {
  if (((dim(pairedTimes[[name]])[1] / 2) %% k) == 0) {} else {
    stop(
      "This k number does not allow for exact sampling for the paired times")
  }
  samples <- seq(1, dim(pairedTimes[[name]])[1], by = 2)
  kfold <- as.list(as.data.frame(matrix(sample(samples), nrow = k)))
  subsetsPairedTimes <- lapply(kfold, function(removeSamples){
    pairedTimes[[name]][-c(removeSamples, removeSamples + 1), ]
  })
  CVresult <- microSTASIS::iterativeClustering(subsetsPairedTimes, parallel, 
                                               "_")
  individuals <- unique(stringr::str_split(rownames(pairedTimes[[name]]), 
                                           common, simplify = TRUE)[, 1])
  CVlist <- lapply(individuals, function(ind) {
    valuesByIndividual <- unlist(lapply(seq_along(CVresult), 
                                        function(mSsubsetPairedTime) {
      if (any(ind == names(CVresult[[mSsubsetPairedTime]]))) {
        CVresult[[mSsubsetPairedTime]][ind == names(
          CVresult[[mSsubsetPairedTime]])]
      } else {
        results[[name]][names(results[[name]]) == ind]
      }
    }), use.names = FALSE)
    if (k == 1) {
      names(valuesByIndividual) <- unlist(kfold)
    } else {
      names(valuesByIndividual) <- unlist(lapply(kfold, paste, collapse = ", "))
    }
    valuesByIndividual
  })
  names(CVlist) <- individuals
  CVlist
}
