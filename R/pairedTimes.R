#' Generate one or multiple matrices with paired times.
#'
#' @param data input object: either a matrix with rownames including ID, common pattern and sampling time, or a TreeSummarizedExperiment object.
#' @param ... Additional argument list that might not ever be used.
#' @param sequential TRUE if paired times to analyse are sequential and present the desired alphanumerical order.
#' @param assay If class(data) == "TreeSummarizedExperiment", name of the assay to use.
#' @param alternativeExp If class(data) == "TreeSummarizedExperiment", name of the alternative experiment to use (if applicable).
#' @param ID If class(data) == "TreeSummarizedExperiment", one of the colData(data) colnames should be given as individuals.
#' @param timePoints If class(data) == "TreeSummarizedExperiment", one of the colData(data) colnames should be given as sampling times.
#' @param common If is.matrix(data), pattern that separates the ID and the sampling time in rownames.
#' @param specifiedTimePoints character vector to specify the selection of concrete paired times.
#'
#' @return A list of matrices with the same number of columns as input and with samples from paired sampling times as rows.
#'
#' @export
#' @docType methods
#' @rdname pairedTimes-methods
#'
#' @examples
#' times <- pairedTimes(data = clr, sequential = TRUE, common = "_0_")
#' times_b <- pairedTimes(data = clr, sequential = FALSE, common = "_0_", 
#'                        specifiedTimePoints = c("1", "26"))
setGeneric("pairedTimes", function(data, ...) standardGeneric("pairedTimes"))
#' @rdname pairedTimes-methods
#' @aliases pairedTimes,matrix,matrix-method
setMethod("pairedTimes", signature = "matrix", 
          definition = function(data, sequential, common, specifiedTimePoints){
  microSTASIS::mSinternalPairedTimes(data = data, 
                                     specifiedTimePoints = if (sequential) {
    stringr::str_sort(unique(stringr::str_split(rownames(data), common, 
                                                simplify = TRUE)[, 2]), 
                      numeric = TRUE)
  }  else {if (all(specifiedTimePoints %in% unique(
    stringr::str_split(rownames(data), common, simplify = TRUE)[, 2]))) {
    specifiedTimePoints
  } else {stop("\nNot all specified time points are measured!")}}, 
                                     common = common)
})
#' @rdname pairedTimes-methods
#' @aliases pairedTimes,TreeSummarizedExperiment,TreeSummarizedExperiment-method
setMethod("pairedTimes", signature = "TreeSummarizedExperiment", 
          definition = function(data, sequential, assay, alternativeExp, 
                                ID, timePoints, specifiedTimePoints){
  if (!is.null(assay) & !is.null(alternativeExp) & 
      length(SingleCellExperiment::altExpNames(data)) != 0){
    if (alternativeExp %in% SingleCellExperiment::altExpNames(data)) {
      data <- data@int_colData@listData[["altExps"]]@listData[[alternativeExp]]@se
    } else {stop("\nNo alternative experiment with that name in the TreeSummarizedExperiment object")}
  }
  else if (!is.null(assay) & 
           length(SummarizedExperiment::assayNames(data)) != 0) {
    if (assay %in% SummarizedExperiment::assayNames(data)){
    } else {stop("\nNo assay with that name in the TreeSummarizedExperiment object")}
  }
  else {stop("\nAssay or alternative experiment with length equal to 0 or with the names not present in the TreeSummarizedExperiment object")}
  if (!all(c(ID, timePoints) %in% 
           colnames(SummarizedExperiment::colData(data)))){
    stop("\nID or timePoints not in colnames(colData(data)); data is the TSE or altExp assay")
  }
  id <- SummarizedExperiment::colData(data)[[ID]]
  common <- "_"
  tp <- SummarizedExperiment::colData(data)[[timePoints]]
  data <- t(SummarizedExperiment::assay(data, assay))
  rownames(data) <- paste(id, tp, sep = common)
  microSTASIS::mSinternalPairedTimes(data = data, 
                                     specifiedTimePoints = if (sequential) {
    stringr::str_sort(unique(stringr::str_split(rownames(data), common, 
                                                simplify = TRUE)[, 2]), 
                      numeric = TRUE)
  }  else {if (all(specifiedTimePoints %in% unique(
    stringr::str_split(rownames(data), common, simplify = TRUE)[, 2]))) {
    specifiedTimePoints
  } else {stop("\nNot all specified time points are measured!")}}, 
                                     common = common)
})
