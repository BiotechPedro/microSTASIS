#' Internal function for [microSTASIS::pairedTimes()].
#'
#' @param data matrix with rownames including ID, common pattern and sampling time.
#' @param specifiedTimePoints character vector to specify the selection of concrete paired times.
#' @param common pattern that separates the ID and the sampling time in rownames.
#'
#' @return A list of matrices with the same number of columns as input and with samples from paired sampling times as rows.
#' @export
#'
#' @examples
#' t1_t2 <- mSinternalPairedTimes(data = clr, specifiedTimePoints = c("1", "25"), common = "_0_")
mSinternalPairedTimes <- function(data, specifiedTimePoints, common) {
  multiplePairedTimes <- lapply(seq_along(specifiedTimePoints)[-length(specifiedTimePoints)], 
                                  function(timePoint){
    subset(data, stringr::str_detect(rownames(data),
                             paste(substr(common, start = nchar(common), stop = nchar(common)),
                                             specifiedTimePoints[timePoint], sep = "")) |
             stringr::str_detect(rownames(data),
                                 paste(substr(common, start = nchar(common), stop = nchar(common)),
                                         specifiedTimePoints[timePoint + 1], sep = ""))
           )
  })
  names(multiplePairedTimes) <- sapply(seq_along(specifiedTimePoints)[-length(specifiedTimePoints)], 
                                         function(timePoint){
    paste("t", specifiedTimePoints[timePoint], "_t", specifiedTimePoints[timePoint + 1], sep = "")
  })
  multiplePairedTimes <- lapply(multiplePairedTimes, function(timePoint){
    individuals <- stringr::str_split(rownames(timePoint), common, simplify = TRUE)[, 1]
    remove <- which(individuals %in% names(which(table(individuals) == 1)))
    if (is.null(remove)){
      timePoint
    } else {
      timePoint[-remove, ]
    }
  })
  multiplePairedTimes
}
