\name{checkTreeValidity}
\alias{checkTreeValidity}
\title{Check Validity of an RDP-Based Taxonomic Tree}
\description{This function recursively goes through every node in the tree and for each node it checks that the sum of that nodes children 
are less than or equal to the value of that node.}
\usage{
checkTreeValidity(data, sample = 2, checkAll = FALSE, epsilon = 0.0001)
}
\arguments{
  \item{data}{A data set that contains 1 column containing the taxa levels and at least 1 column of values for those levels.}
  \item{sample}{A numeric vector that is used to select a single (or several) tree(s) out of a data set.}
  \item{checkAll}{If 'TRUE' then the 'sample' argument will be ignored and every tree in the data set will be checked.}
  \item{epsilon}{This value allows for rounding problems or other such small errors in the data such that the (parent + epsilon > sum(children)).}
}
\details{
HMP metagenomic sequences in a sample can be represented as a rooted taxonomic tree. Using supervised taxonomic methods a sequence is matched to a 
hierarchical taxa or taxonomy bins defined in a bacterial-taxonomy library such as, for example, the Ribosomal Database Project (RDP) (Cole, 2005). The 
supervised taxonomic analysis allows us to represent each sample (set of sequences) by a rooted taxonomic tree where the root corresponds to taxon at the 
Kingdom level, i.e., bacteria, and the leaves correspond to the taxa at the Genus level, and the width of the edges (paths) between taxonomic levels correspond 
to the 'abundances' of the descending taxon. In particular, we combine RDP matches by adding RDP values of common taxon, which allows us to provide a measure 
of taxa abundance weighting on the confidence of each taxa assignment. The resulting taxonomic trees satisfy the following conditions: i) branches closer to 
the root have higher 'abundance' values than branches closer to leaves, and ii) the sum of the 'abundances' of all descending taxa under a common parent taxon 
cannot be larger than the 'abundance' of the corresponding parent taxon.
}

\value{A boolean vector that indicates the validity of every tree tested.}
\references{
Cole JR, Chai B, Farris RJ, Wang Q, Kulam SA, McGarrell DM, Garrity GM, Tiedje JM. The Ribosomal Database Project (RDP-II): 
sequences and tools for high-throughput rRNA analysis. Nucleic Acids Research 2005; 33: D294-D296.
}
\author{Berkley Shands, Patricio S. La Rosa, Elena Deych, William D. Shannon}

\examples{
data(saliva) 

validTree <- checkTreeValidity(saliva, 2, FALSE)
validTree
}