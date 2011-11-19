\name{transformHMPtoHMPTree}
\alias{transformHMPtoHMPTree}

\title{Transform data from HMP to HMPTrees}
\description{This function will take data in the format needed in the package 'HMP' to the format used by 'HMPTrees'.}
\usage{
transformHMPtoHMPTree(data)
}

\arguments{
  \item{data}{A data set of values in which each column is a separate taxa level and each row is a separate sample.}
}
\details{This function does the opposite of 'transformHMPTreetoHMP'.}
\value{A data set that contains the taxa levels in the first column and each additional column is for a single sample and is a filled 
with values for the taxa levels.}

\author{Berkley Shands, Patricio S. La Rosa, Elena Deych, William D. Shannon}

\examples{
data(saliva)

### This transforms the saliva data set but retains
### any zero rows that may exist. 
transSaliva <- transformHMPTreetoHMP(saliva, FALSE, 0)

### saliva2 should be the same as saliva
saliva2 <- transformHMPtoHMPTree(transSaliva)
}