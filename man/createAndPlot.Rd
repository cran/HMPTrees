\name{createAndPlot}
\alias{createAndPlot}
\title{Create and Plot a Tree from a Data Set}
\description{This function combines the createTrees and plotTree function to create and plot a set of trees.}
\usage{
createAndPlot(data = NULL, sampNum = 2, allSamp = FALSE, 
maxTaxaLevel = "genus", myColors, myDivisions, myTitle, 
mySubTitle, showTipLabel = TRUE, showNodeLabel = FALSE, 
displayLegend = TRUE, onePerPage = FALSE)
}

\arguments{
  \item{data}{A data set that contains 1 column containing the taxa levels and at least 1 column of values for those levels.}
  \item{sampNum}{A numeric vector that is used to select a single (or several) tree(s) out of a data set.}
  \item{allSamp}{If 'TRUE' then the 'sample' argument will be ignored and every tree in the data set will be plotted.}
  \item{maxTaxaLevel}{The depth the tree creation will go down to (kingdom, phylum, class, order, family, genus, species). Genus is the default.}
  \item{myColors}{A vector of colors to be used in the plot from lowest ranking to highest ranking.}
  \item{myDivisions}{A vector of numbers from lowest to highest to separate the tree branches into the color ranking.}
  \item{myTitle}{A custom title for the plot.}
  \item{mySubTitle}{A custom subtitle for the plot.}
  \item{showTipLabel}{Hides the tip labels if 'FALSE' otherwise it shows all non-zero tip labels.}
  \item{showNodeLabel}{Hides the interior node labels if 'FALSE' otherwise it shows all non-zero node labels.}
  \item{displayLegend}{Enables the display of a legend of the branch colors and divisions when 'TRUE'.}
  \item{onePerPage}{If 'TRUE' one tree will be plotted per page, if 'FALSE' four will be displayed per page.}
}
\details{For 'maxTaxaLevel' k, p, c, o, f, g, and s can be used in place of kingdom, phylum, class, order, family, genus, and species respectively.
The values for division should directly relate to the values of your data, i.e. if your data ranges from 0 to 50000
reads you should adjust the divisions to fit your data.}

\value{A plot of the tree(s).}
\author{Berkley Shands, Patricio S. La Rosa, Elena Deych, William D. Shannon}

\examples{
data(saliva)

### Plots the trees in column 2 and 3 in 'Saliva'
createAndPlot(saliva, c(2:3))
}