\name{mergeDataSets}
\alias{mergeDataSets}
\title{Merge Multiple Data Sets}
\description{This function can take up to three data sets, calculate their MLEs and merge them.}
\usage{
mergeDataSets(data1 = NULL, data2 = NULL, data3 = NULL, calcMLE = TRUE)
}

\arguments{
  \item{data1, data2, data3}{Data sets that contain at least 1 column of values.  A column of taxa levels is required if the data sets have different taxa levels.}
  \item{calcMLE}{If 'FALSE' the MLEs for the data sets will not be calculated, otherwise they are added to the end.}
}
\details{If the data sets do not contain the same number of rows, then they must all be able to merge on the first column in the data set, otherwise
an error will be produced.}
\value{A single data set containing all the data from the three data sets and possibly their individual MLEs and a combined MLE.}

\author{Berkley Shands, Patricio S. La Rosa, Elena Deych, William D. Shannon}

\examples{
data(saliva)
data(stool)

dataComb <- mergeDataSets(saliva, stool, calcMLE=TRUE)
}