\name{trimToTaxaLevel}
\alias{trimToTaxaLevel}

\title{Trim a Tree to a Given Level}
\description{This function will take a tree and either remove all nodes lower than the given 
level or will remove all nodes not of the given level.}
\usage{
trimToTaxaLevel(data, myTaxaLevel = "genus", eliminateParentNodes = FALSE)
}

\arguments{
  \item{data}{A data set that contains 1 column containing the taxa levels and at least 1 column of values for those levels.}
  \item{myTaxaLevel}{The depth the tree will go down to (kingdom, phylum, class, order, family, genus, species). Genus is the default.}
  \item{eliminateParentNodes}{
  	If 'TRUE' the data set returned will only contain rows at the level specified by 'myTaxaLevel'.
  	If 'FALSE' the data set returned will contain all the nodes up to the level specified by 'myTaxaLevel'.}
}
\details{For 'myTaxaLevel' k, p, c, o, f, g, and s can be used in place of kingdom, phylum, class, order, family, genus, and species respectively.}
\value{A new data set that has been trimmed to the level selected.}

\author{Berkley Shands, Patricio S. La Rosa, Elena Deych, William D. Shannon}

\examples{
data(saliva)

### Trims saliva to the class level such that it still 
### contains the kingdom and phylum levels
trimSaliva <- trimToTaxaLevel(saliva, "class", FALSE)
}