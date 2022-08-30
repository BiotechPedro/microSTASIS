#' Easily extract groups of individuals from sample metadata.
#'
#' @param metadata input data.frame with data corresponding to samples.
#' @param samples vector from metadata corresponding to the samples ID, if applicable.
#' @param individuals vector of individuals; first column of the [microSTASIS::mSpreviz()] output.
#' @param colname name of the column with the variable used for grouping individuals.
#' @param common pattern that separates the ID and the sampling time in rownames, if applicable.
#' @param TreeSummarizedExperiment TRUE if colData(data), or colData(altExp(data)), passed as metadata.
#' @param ID If TreeSummarizedExperiment, one of the colData(data) colnames should be given as individuals.
#' @param time_points If TreeSummarizedExperiment, one of the colData(data) colnames should be given as sampling times.
#'
#' @return A vector with the same length as the number of rows in the [microSTASIS::mSpreviz()] output.
#' @export
#'
#' @examples
#' times <- pairedTimes(data = clr, sequential = TRUE, common = "_0_")
#' mS <- iterativeClustering(pairedTimes = times, parallel = TRUE, common = "_0_")
#' results <- mSpreviz(results = mS, times = times)
#' metadata <- data.frame(Sample = rownames(clr), age = c(rep("youth", 65), rep("old", 131-65)))
#' group <- mSmetadataGroups(metadata = metadata, samples = metadata$Sample, common = "_0_",
#'                           individuals = results$individual, variable = "age")
mSmetadataGroups <- function(metadata, samples, individuals, variable, common,
                            TreeSummarizedExperiment = FALSE, ID, timePoints){
  if (TreeSummarizedExperiment){
    id <- metadata[[ID]]
    common <- "_"
    tp <- metadata[[timePoints]]
    samples <- paste(id, tp, sep = common)
  }
  sapply(seq_along(individuals), function(ind){
    samples_ind <- which(stringr::str_detect(samples,  paste(individuals[ind], substr(common, 1, 1), sep = "")))
    if (length(unique(metadata[samples_ind, variable])) == 1) {
      as.character(unique(metadata[samples_ind, variable]))
    } else {
      stop(paste("Individual", individuals[ind], "presents multiple values for variable", variable))
    }
  })
}
