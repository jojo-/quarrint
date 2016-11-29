\name{compute.ann}
\alias{compute.ann}
\alias{compute.ann.default}
\alias{compute.ann.quarry}
\alias{ann.interactions}
\title{
Neural Network-based Interaction Index for a Quarry
}
\description{
Given an object of type quarry, a neural network computes the interaction index
(low, medium, high or very high).
}
\usage{
\method{compute.ann}{quarry}(x, ann = NULL, rep = 1, ...)
}
\arguments{
  \item{x}{
    A quarry object.
}
  \item{ann}{
    The neural network used to estimate the interaction index. By default, if
    set to \code{NULL}, it uses a neural network provided by the package. The 
    neural network is an object of class \code{nn}.
}
  \item{rep}{
    The repetition of ann to be used.
}
  \item{\dots}{
    Further arguments passed to or from other methods. For instance, see
    \code{\link[neuralnet:compute]{compute}} of the package \pkg{neuralnet}.
}
}
\details{
The neural network provided with the package has a feed-forward design and
has a hidden layer of 7 nodes. It takes as an input a quarry constructed
with the function "quarry" using the default parameters. This neural
network is fully detailed in "Interaction prediction between groundwater
and quarry extension using discrete choice models and artificial neural
networks" (Barthelemy et al., 2016).

It is possible to use another neural network that has been trained with the
function \code{\link[quarrint:train.ann]{train.ann}}.
}
\value{
A list whose elements are:
\item{low}{The output of the ann for a low interaction level.}
\item{medium}{The output of the ann for a medium interaction level.}
\item{high}{The output of the ann for a high interaction level.}
\item{very.high}{The output of the ann for a very high interaciton level.}
\item{idx}{a string with the level of interaction (\code{low}, \code{medium}, \code{high}
or \code{very high}).}
}
\references{
  Barthelemy, J., Carletti, T., Collier L., Hallet, H., Moriame, M.,
  Sartenaer, A. (2016)
  Interaction prediction between groundwater and quarry extension using discrete
  choice models and artificial neural networks
  \emph{Environmental Earth Sciences} (in press)

  Collier, L., Barthelemy, J., Carletti, T., Moriame, M., Sartenaer, A.,
  Hallet, H. (2015)
  Calculation of an Interaction Index between the Extractive Activity and
  Groundwater Resources
  \emph{Energy Procedia} 76, 412-420

  Krieselm, D. (2007)
  A Brief Introduction to Neural Networks. On-line
  available at \url{http://www.dkriesel.com}

  Ripley, B. (1996)
  \emph{Pattern recognition and neural networks}
  Cambridge university press
}
\author{
Johan Barthelemy.

Maintainer: Johan Barthelemy \email{johan@uow.edu.au}.
}
\note{
The quarry \code{x} must have been created by the
\code{\link[quarrint:quarry]{quarry}}. It can accept custom ranges for the
parameters values but they must be consistent with what has been used to train
the neural network \code{ann}.
}
\seealso{
\code{\link[quarrint:compute.dc]{compute.dc}} to compute an interaction index
based on a discrete choice model and
\code{\link[quarrint:compute.interaction]{compute.interaction}} to predict the
interaction between between the quarry and the groundwater using both the
discrete choice-based index and the neural network-based index.

\code{\link[quarrint:train.ann]{train.ann}} to train a neural network and use it
as an input for this function.

\code{\link[neuralnet:compute]{compute}} and
\code{\link[neuralnet:neuralnet]{neuralnet}} of the package \pkg{neuralnet} for
optional additional parameters and details about objects of class \code{nn}.
}
\examples{
# creating a quarry
q <- quarry(geological.context = 2, hydrogeological.context = 4,
            piezometric.context = 1, quarry.position = 4,
            production.catchment = 4, quality.catchment = 3)

# computing the interaction index using the default neural network
inter.idx <- compute.ann(q)
}
\keyword{neural}

