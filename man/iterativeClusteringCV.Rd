% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/iterativeClusteringCV.R
\name{iterativeClusteringCV}
\alias{iterativeClusteringCV}
\title{Cross validation of the iterative Hartigan-Wong k-means clustering.}
\usage{
iterativeClusteringCV(
  pairedTimes,
  results,
  name,
  common = "_",
  k = 1L,
  BPPARAM = BiocParallel::bpparam()
)
}
\arguments{
\item{pairedTimes}{list of matrices with paired times,
i.e. samples to be stressed to multiple iterations.
Output of \code{\link[=pairedTimes]{pairedTimes()}}.}

\item{results}{the list output of \code{\link[=iterativeClustering]{iterativeClustering()}}.}

\item{name}{character; name of the paired times whose stability is being assessed.}

\item{common}{pattern that separates the ID and the sampling time.}

\item{k}{integer; number of individuals to remove from the data for
each time running \code{\link[=iterativeClustering]{iterativeClustering()}}.}

\item{BPPARAM}{supply a \code{BiocParallel} parameters object,
e.g. \code{\link[BiocParallel:SerialParam-class]{BiocParallel::SerialParam()}} in the specific case of Windows OS
or \code{\link[BiocParallel:register]{BiocParallel::bpparam()}}.}
}
\value{
Multiple lists with multiple objects of class "kmeans".
}
\description{
Perform cross validation of the stability results from
\code{\link[=iterativeClustering]{iterativeClustering()}}in the way of leave-one-out (LOO)
or leave-k-out (understood as quitting k individuals each time for
calculating the metric over individuals).
}
\examples{
data(clr)
times <- pairedTimes(data = clr[, 1:20], sequential = TRUE, common = "_0_")
mS <- iterativeClustering(pairedTimes = times, common = "_")
cv_klist_t1_t25_k2 <- iterativeClusteringCV(pairedTimes = times, 
                                            results = mS, name = "t1_t25",
                                            common = "_0_", k = 2L)
}
