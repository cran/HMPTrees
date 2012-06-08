\name{saliva}
\alias{saliva}
\docType{data}
\title{Saliva Data Set}
\description{A data set containing all taxa from 24 subjects.}
\usage{data(saliva)}
\format{
The format is a data frame of 454 rows by 25 columns, with the each column being a separate subject and each row being a different taxa, 
with the exception of the first column. The first column contains the taxanomical name separated by '.', while every other column contains 
the number of times that sample was recorded. It should also be noted that the samples are already normalized to 7000 reads.
}
\examples{
data(saliva)
}
\keyword{datasets}