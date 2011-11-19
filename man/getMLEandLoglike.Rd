\name{getMLEandLoglike}
\alias{getMLEandLoglike}
\title{Get MLE and Log Likelihood of a Data Set}
\description{This function takes a data set and computes the MLE tree and its Log Likelihood values.}
\usage{
getMLEandLoglike(data, maxSteps = 50, weight = NULL)
}

\arguments{
  \item{data}{A data set that contains at least one tree.}
  \item{maxSteps}{The maximum number of times to bootstrap for the MLE.}
  \item{weight}{A vector to weight the MLE tree.}
}
\details{
A unimodal probability model for graph-valued random objects has been derived and applied previously to several types of graphs 
(cluster trees, digraphs, and classification and regression trees) (For example, Banks and Constantine, 1998; Shannon and Banks, 1999). 
Here we apply this model is applied to HMP trees constructed from RDP matches.  Let \eqn{G} be the finite set of taxonomic trees with elements 
\eqn{g}, and \eqn{d: G \times G \to R^{+}} an arbitrary metric of distance on \eqn{G}. We have the probability measure \eqn{H(g^{*},\tau)} defined by 
\deqn{P(g;g^{*},\tau) = c(g^{*},\tau) \exp(-\tau d(g^{*},g) ), for all g \in G,}
where \eqn{g^{*}}  is the modal or central tree, \eqn{\tau} is a concentration parameter, and \eqn{c(g^{*},\tau)} is the normalization constant.  
The distance measure between two trees is the Euclidean norm of the difference between their corresponding adjacency-vectors. To estimate the parameters 
\eqn{(g^{*},\tau)}, we use the maximum likelihood estimate (MLE) procedure described in (La Rosa et al., in revision.)}

\value{A list containing the MLE tree, log likelihood, and tau.}
\references{
Banks D, Constantine GM. Metric Models for Random Graphs. Journal of Classification 1998; 15: 199-223. \cr
Shannon WD, Banks D. Combining classification trees using MLE. Stat Med 1999; 18: 727-740. \cr
La Rosa P S, Shands B, Deych E, Zhou Y, Sodergren E, Weinstock G, and Shannon W. D. Object Data Analysis for Taxonomic Trees from Human Microbiome Data. 
In revision for Stat Med. 
}
\author{Patricio S. La Rosa, Elena Deych, Berkley Shands, William D. Shannon}

\examples{
data(saliva)

MLE <- getMLEandLoglike(saliva, 1)$mleTree
}