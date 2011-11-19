\name{createTrees}
\alias{createTrees}
\title{Create a Tree Object}
\description{This function creates a list tree objects of type 'phylo' for use in plotting the tree.}
\usage{
createTrees(data = NULL, sampNum = 2, allSamp = FALSE, 
maxTaxaLevel = "genus")
}

\arguments{
  \item{data}{A data set that contains 1 column containing the taxa levels and at least 1 column of values for those levels.}
  \item{sampNum}{A numeric vector that is used to select a single (or several) tree(s) out of a data set.}
  \item{allSamp}{If 'TRUE' then the 'sample' argument will be ignored and every tree in the data set will be turned into a 'phylo' object.}
  \item{maxTaxaLevel}{The depth the tree creation will go down to (kingdom, phylum, class, order, family, genus, species). Genus is the default.}
}

\details{For 'maxTaxaLevel' k, p, c, o, f, g, and s can be used in place of kingdom, phylum, class, order, family, genus, and species respectively.}

\value{A list of 'phylo' objects that can be passed to plotTree to plot them.}
\author{Berkley Shands, Patricio S. La Rosa, Elena Deych, William D. Shannon}

\examples{
data(saliva)

mySalivaTree <- createTrees(saliva, 4)
}