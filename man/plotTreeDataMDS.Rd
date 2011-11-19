\name{plotTreeDataMDS}
\alias{plotTreeDataMDS}
\title{Plot an MDS Plot of a Group of Trees}
\description{This function takes up to three data sets and plots them on an MDS plot to show differences.}
\usage{
plotTreeDataMDS(data1 = NULL, data2 = NULL, data3 = NULL, 
myTitle = "Tree MDS Comparisons", calcMLE = TRUE, 
mleTitle1 = "Data1 MLE", mleTitle2 = "Data2 MLE", mleTitle3 = "Data3 MLE")
}

\arguments{
  \item{data1, data2, data3}{Data sets that contain at least 1 column of values.  A column of taxa levels is required if the data sets have different taxa levels.}
  \item{myTitle}{A title for the MDS plot.}
  \item{calcMLE}{If 'FALSE' the MLEs for the data sets will not be calculated and plotted.}
  \item{mleTitle1, mleTitle2, mleTitle3}{The labels for each MLE data point on the MDS plot.}
}
\details{This function uses mergeDataSets to combine the data and therefore has the same requirement that 
if the data sets do not contain the same number of rows, then they must all be able to merge on the first column in the data set, otherwise
an error will be produced.}
\value{A MDS plot of the data.}

\author{Berkley Shands, Patricio S. La Rosa, Elena Deych, William D. Shannon}

\examples{
data(saliva)
data(stool)

plotTreeDataMDS(saliva, stool, mleTitle1="Saliva", mleTitle2="Stool")
}