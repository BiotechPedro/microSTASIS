#' Easily extract groups of individuals from sample metadata.
#'
#' @param metadata input data.frame with data corresponding to samples. It can
#'         be the [SummarizedExperiment::colData()] from the 
#'         TreeSummarizedExperiment.
#' @param samples vector from metadata corresponding to the samples ID, 
#'         if applicable; should be NULL if ID and timePoints are provided
#'         from a TreeSummarizedExperiment, for example.
#' @param individuals vector of individuals; first column of the 
#'         [microSTASIS::mSpreviz()] output.
#' @param variable column name with the variable used for grouping individuals.
#' @param common pattern that separates the ID and the sampling time in 
#'         rownames, if applicable.
#' @param ID If applicable, one of the colData() colnames from the 
#'         TreeSummarizedExperiment should be given as individuals.
#' @param timePoints If applicable, one of the colData() colnames from the 
#'         TreeSummarizedExperiment should be given as sampling times.
#'
#' @return A vector with the same length as the number of rows in the 
#'         [microSTASIS::mSpreviz()] output.
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
mSmetadataGroups <- function(metadata, samples, individuals, variable, common,
                             ID, timePoints){
  if (is.null(samples)){
    id <- metadata[[ID]]
    common <- "_"
    tp <- metadata[[timePoints]]
    samples <- paste(id, tp, sep = common)
  }
  vapply(seq_along(individuals), function(ind){
    samples_ind <- which(stringr::str_detect(samples, 
                                             paste(individuals[ind], 
                                                   substr(common, 1, 1), 
                                                   sep = "")))
    if (length(unique(metadata[samples_ind, variable])) == 1) {
      as.character(unique(metadata[samples_ind, variable]))
    } else {
      stop("Some individual/s presents multiple values for variable")
    }
  }, FUN.VALUE = "vector")
}
