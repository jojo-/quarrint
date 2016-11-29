\name{compute.dc}
\alias{compute.dc}
\alias{compute.dc.default}
\alias{compute.dc.quarry}
\title{
Discrete Choice Model-based Interaction Index for a Quarry
}
\description{
Given an object of type \code{quarry}, the function computes the probabilities
of each level of interaction (low, medium, high and very high) using a Logit
discrete choice model.

}
\usage{
\method{compute.dc}{quarry}(x, ...)
}
\arguments{
  \item{x}{An object of type quarry.
}
  \item{\dots}{
    Further arguments passed to or from other methods.
}
}
\details{
The model parameters have been estimated with BIOGEME and has an adjusted
\eqn{\rho^2}{\rho-square} of 0.609. The model is fully detailed in the paper "Interaction
prediction between groundwater and quarry extension using discrete choice
models and artificial neural networks" (Barthelemy et al., 2016).
}
\value{
A list whose elements are:
\item{p.low}{The probability of a low interaction level.}
\item{p.medium}{The probability of a medium interaction level.}
\item{p.high}{The probability of a high interaction level.}
\item{p.very.high}{The probability of a very high interaciton level.}
}
\references{
  Barthelemy, J., Carletti, T., Collier L., Hallet, V., Moriame, M.,
  Sartenaer, A. (2016)
  Interaction prediction between groundwater and quarry extension using discrete
  choice models and artificial neural networks
  \emph{Environmental Earth Sciences} (in press)

  Collier, L., Barthelemy, J., Carletti, T., Moriame, M., Sartenaer, A.,
  Hallet, V. (2015)
  Calculation of an Interaction Index between the Extractive Activity and
  Groundwater Resources
  \emph{Energy Procedia} 76, 412-420

  Bierlaire, M. (2003)
  BIOGEME: a free package for the estimation of discrete choice models.
  \emph{Swiss Transport Research Conference} TRANSP-OR-CONF-2006-048
}
\author{
Johan Barthelemy.

Maintainer: Johan Barthelemy \email{johan@uow.edu.au}.
}
\note{
In order to use the function, the quarry must have been constructed with the
function \code{\link[quarrint:quarry]{quarry}} using the default parameters.
}
\seealso{
\code{\link[quarrint:compute.ann]{compute.ann}} to compute an interaction index
based on an artificial neural network and
\code{\link[quarrint:compute.interaction]{compute.interaction}} to predict the
interaction between between the quarry and the groundwater using both the
discrete choice-based index and the neural network-based index.
}
\examples{
# creating a quarry
q <- quarry(geological.context = 2, hydrogeological.context = 4,
            piezometric.context = 1, quarry.position = 4,
            production.catchment = 4, quality.catchment = 3)

# computing the interaction index
inter.idx <- compute.dc(q)
}
\keyword{models}
\keyword{neural}
\concept{discrete choice}
