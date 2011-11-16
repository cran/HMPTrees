\name{plotTree}
\alias{plotTree}
\title{Plots a Tree Object}
\description{This function takes one or more 'phylo' objects and plots them.}
\usage{
plotTree(trees, myColors, myDivisions, myTitle, mySubTitle, 
showTipLabel = TRUE, showNodeLabel = FALSE, displayLegend = TRUE)
}

\arguments{
  \item{trees}{A list that contains at least one tree of type 'phylo'.}
  \item{myColors}{A vector of colors to be used in the plot from lowest ranking to highest ranking.}
  \item{myDivisions}{A vector of numbers from lowest to highest to separate the tree branches into the color ranking.}
  \item{myTitle}{A custom title for the plot.}
  \item{mySubTitle}{A custom subtitle for the plot.}
  \item{showTipLabel}{Hides the tip labels if 'FALSE' otherwise it shows all non-zero tip labels.}
  \item{showNodeLabel}{Hides the interior node labels if 'FALSE' otherwise it shows all non-zero node labels.}
  \item{displayLegend}{Enables the display of a legend of the branch colors and divisions when 'TRUE'.}
}
\details{
The 'phylo' type is a product of the 'ape' package and the 'createTrees' function in this package produces a list of 'phylo' type objects 
for use with this function.  The values for division should directly relate to the values of your data, i.e. if your data ranges from 
0 to 50000 reads you should adjust the divisions to fit your data.
}

\value{A plot of the tree(s).}
\author{Berkley Shands, Patricio S. La Rosa, Elena Deych, William D. Shannon}

\examples{
data(saliva)

mySalivaTree <- createTrees(saliva, 4)
plotTree(mySalivaTree)
}