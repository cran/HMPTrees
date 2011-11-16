\name{compareTwoDataSets}
\alias{compareTwoDataSets}
\title{Likelihood-Ratio-Test Statistics to Compare the Distribution of 2 Sets of RDP-Based Taxonomic Trees}
\description{This functions compares the distribution of 2 sets of RDP-based taxonomic trees using Likelihood-Ratio-Test statistics. 
A p-value is computed using boostraping. This procedure allows for parallel computation.}
\usage{
compareTwoDataSets(data1, data2, numBootStraps = 1000, 
enableMC = FALSE, cores = 8)
}

\arguments{
  \item{data1, data2}{A data set that contains at least 1 column of values.  A column of taxa levels is not required.}
  \item{numBootStraps}{The number of times to run the bootstrapping, the default is 1000.}
  \item{enableMC}{When this is 'TRUE' it allows for parallel calculation of the bootstraps. (See Details).}
  \item{cores}{The number of parallel processes to run if enableMC is 'TRUE'.}
}
\details{If the data sets do not contain the same number of rows, then they must all be able to merge on the first column in the data set, otherwise
an error will be produced.
Enabling parallel calculation requires the packages 'doMC', 'foreach', 'mutlicore' and 'doSMP',
and because of the way 'doMC' works a linux based system is also required.\cr 

We are interested in assessing whether the distributions from two metagenomic populations are the same or different, which is equivalent to evaluating 
whether their respective parameters are the same or different. The corresponding hypothesis is given as follows:
\deqn{H_{\mathrm{o}}: (g_{1}^{*},\tau_{1}) =  (g_{2}^{*},\tau_{2}) = (g_{0}^{*},\tau_{0}) vs H_{\mathrm{A}}: (g_{1}^{*},\tau_{1}) \neq (g_{2}^{*},\tau_{2}) ,}
where \eqn{(g_{0}^{*},\tau_{0})} is the unknown common parameter vector. To evaluate this hypothesis we use the likelihood-ratio test (LRT) which is given by,
\deqn{\lambda = -2 \log\left(\frac{L(g_{o}^{*},\tau_{o};{S_{1n},S_{2m}})}{L(g_{1}^{*},\tau_{1};{S_{1n}})+L(g_{2}^{*},\tau_{2};{S_{2m}})} \right),}
where \eqn{S_{1n}} and \eqn{S_{2m}} are the sets containing \eqn{n} and \eqn{m} random samples of trees from each metagenomic population, respectively. 
We assume that the model parameters are unknown under both the null and alternative hypothesis, therefore, we estimate these using the MLE procedure proposed 
in (La Rosa et al, under review), and compute the corresponding p-value using non-parametric bootstrap.
}

\value{A p-value for the similarity of the two data sets based on the bootstrapping.}
\references{
La Rosa P S, Shands B, Deych E, Zhou Y, Sodergren E, Weinstock G, and Shannon W. D. Object Data Analysis for Taxonomic Trees from 
Human Microbiome Data. Under Review for Stat Med. 
}
\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
data(saliva)
data(stool)

test <- compareTwoDataSets(saliva, stool, 1)
test
}