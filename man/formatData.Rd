\name{formatData}
\alias{formatData}
\title{Formats a Data Set}
\description{This function will take a data set and format by ignoring low count trees, and/or normalizing counts.}
\usage{
formatData(data, countThreshold = 1000, normalizeThreshold = 10000)
}

\arguments{
  \item{data}{A data set that contains at least 1 column of values. A column of taxa levels is not required.}
  \item{countThreshold}{A cut off threshold for reads - all trees with fewer than this number of reads will be removed.}
  \item{normalizeThreshold}{All the trees that are not removed will be normalized to this many reads.}
}

\value{A new data set that trimmed based on the parameters, and the new data is also reordered alphabetically.}
\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
data(throat)

### This removes the first 2 columns which contain
### site and region identifiers for the data
throat <- throat[,c(-1,-2)]

### This will remove all trees with less than 1000 reads 
### and normalize to 7000 reads
throat <- formatData(throat, 1000, 7000)
}