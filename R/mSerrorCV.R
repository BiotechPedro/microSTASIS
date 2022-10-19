#' Compute the mean absolute error (MAE) in percentage after 
#' [microSTASIS::iterativeClusteringCV()].
#'
#' @description Compute the mean absolute error after the cross validation or
#'         plot lines connecting the stability values for each subset of the 
#'         original matrix of paired times.
#'
#' @param pairedTime input matrix with paired times whose stability 
#'         has being assessed. One of the lists output of 
#'         [microSTASIS::pairedTimes()].
#' @param CVklist list resulting from [microSTASIS::iterativeClusteringCV()].
#' @param k integer; number of individuals to subset from the data. 
#'         The same as used in [microSTASIS::iterativeClusteringCV()].
#'
#' @return A vector with MAE values for each individual's mS score.
#' @export
#'
#' @examples
#' times <- pairedTimes(data = clr[, 1:20], sequential = TRUE, common = "_0_")
#' mS <- iterativeClustering(pairedTimes = times, common = "_")
#' cv_klist_t1_t25_k2 <- iterativeClusteringCV(pairedTimes = times, 
#'                                             results = mS, name = "t1_t25",
#'                                             common = "_0_", k = 2L)
#' MAE_t1_t25 <- mSerrorCV(pairedTime = times$t1_t25, 
#'                         CVklist = cv_klist_t1_t25_k2,  k = 2L)
#' MAE <- mSpreviz(results = list(MAE_t1_t25), 
#'                 times = list(t1_t25 = times$t1_t25))
#' mSheatmap(results = MAE, times = c("t1_t25", "t25_t26"), label = TRUE,
#'           high = 'red2',  low = 'forestgreen', midpoint = 5)
mSerrorCV <- function(pairedTime, CVklist, k = 1L){
  individuals <- unique(stringr::str_split(rownames(pairedTime), "_0_", 
                                           simplify = TRUE)[,1])
  samples <- seq(1, nrow(pairedTime), by = 2)
  CVmatrix <- t(as.data.frame(CVklist))
  limits <- seq(k, length(samples), length(samples) / ncol(CVmatrix))
  location <- t(stringr::str_split(colnames(CVmatrix), ", ", simplify = TRUE))
  CVmatrix <- CVmatrix[, vapply(as.character(samples), function(sample) {
    place <- which(location %in% sample)
    if (place[1] > limits[1]) {place <- sum(place > limits) + 1} else {
      place <- 1
    }
    place
  }, FUN.VALUE = 1)]
  MAE <- round(vapply(seq_len(nrow(CVmatrix)), function(ind){
    sum(abs(CVmatrix[ind, ind] - CVmatrix[ind, ])) / (length(CVmatrix[ind, ]) - 
                                                        k) * 100
  }, FUN.VALUE = 1), 2)
  names(MAE) <- individuals
  MAE
}
