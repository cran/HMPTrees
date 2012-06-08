\name{throat}
\alias{throat}
\docType{data}
\title{Throat Data Set}
\description{A data set containing all taxa from 24 subjects.}
\usage{data(throat)}
\format{
The format is a data frame of 529 rows by 25 columns, with the each column being a separate subject and each row being a different taxa, 
with the exception of the first three columns. The first column is the body site column, which in this case has already been filtered to 
only include 'throat'. The second column is the region column, which in this case has already been filtered to only include "v3-v5" and the 
third column contains the taxanomical name separated by '.', while every other column contains the number of times that sample was recorded. 
The Throat data has also not been normalized and should be used with 'formatData'.
}
\examples{
data(throat)
}
\keyword{datasets}