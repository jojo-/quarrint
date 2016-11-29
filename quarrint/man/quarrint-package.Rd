\name{quarrint-package}
\alias{quarrint-package}
\alias{quarrint}
\docType{package}
\title{
Interaction Prediction Between Groundwater and Quarry Extension Using Discrete
Choice Models and Artificial Neural Networks
}
\description{
An implementation of two interaction indices between extractive activity and
groundwater resources based on hazard and vulnerability parameters used in the
assessment of natural hazards. One index is based on a discrete choice model and
the other is relying on an artificial neural network.
}
\details{
\tabular{ll}{
Package: \tab quarrint\cr
Type: \tab Package\cr
Version: \tab 1.0.0\cr
Date: \tab 2016-11-23\cr
Depends: \tab R(>= 2.10.0), neuralnet\cr
License: \tab GPL-2\cr
}

This package provides two interactions indices between quarries (extractive
activity) and groundwater ressources using two different methodologies, namely
the discrete choice models and artificial neural networks. The design of those
indices is fully detailed in Barthelemy et al. (2016).

The quarries and the groudwater ressources are described by 6 parameters,
each classified into 4 modalities. These parameters are grouped into 2 distinct
categories:
\itemize{
  \item{
    the geological, hydrogeological and piezometric contexts defining the hazard
    that a quarry represents;
  }
  \item{
    the relative position of the quarry and the water catchments, the production
    of the catchments and the potential quality of the groundwater
    characterising the vulnerability of the groundwater resources.
  }
}

Each of the resulting 3327 physically feasible combinations of these parameters
(out of a theoretical number of 4 6 = 4096 possible combinations) determines one
particular quarry site type. These feasible combination are provided in the
data frame \code{\link[quarrint:quarries]{quarries}}.

Depending on the values of the parameters, the interaction index can then be
low, medium, high or very high. The interaction level can then be used to
inform a quarry operator on the required level of investigation before
considering any extension of the quarry.

The method \code{\link[quarrint:compute.interaction]{compute.interaction}}
provides an interface to compute the 2 interaction indices. It takes as an input
an object of type \code{quarry} that can be constructed with the method
\code{\link[quarrint:quarry]{quarry}}.

The discrete choice-based and the neural network-based indices can be
respectively be computed with the functions
\code{\link[quarrint:compute.dc]{compute.dc}} and
\code{\link[quarrint:compute.ann]{compute.ann}} also taking as an input an
object of type \code{quarry}.

The package also includes the function
\code{\link[quarrint:train.ann]{train.ann}} to allow the training of custom
artificial neural network that can be used with the functions
\code{\link[quarrint:compute.ann]{compute.ann}} and
\code{\link[quarrint:compute.interaction]{compute.interaction}}.

Finally an auxillary function \code{\link[quarrint:int.in.range]{int.in.range}}
is also provided to determine if a given integer is within a specified range.
}
\author{
Johan Barthelemy, Timoteo Carletti, Louise Collier, Vincent Hallet, Marie
Moriame, M. and Annick Sartenaer.

Maintainer: Johan Barthelemy \email{johan@uow.edu.au}.
}
\references{
  Barthelemy, J., Carletti, T., Collier L., Hallet, V., Moriame, M.,
  Sartenaer, A. (2016)
  Interaction prediction between groundwater and quarry extension using discrete
  choice models and artificial neural networks
  \emph{Environmental Earth Sciences} (in press)

  Collier, L., Barthelemy, J., Carletti, T., Moriame, M., Sartenaer, A.,
  Hallet, H. (2015)
  Calculation of an Interaction Index between the Extractive Activity and
  Groundwater Resources
  \emph{Energy Procedia} 76, 412-420
}
\seealso{
\code{\link[neuralnet:neuralnet]{neuralnet}} for training and using artifical
neural network and BIOGEME to estimate discrete choice models
(\url{http://biogeme.epfl.ch/home.html}).
}
\examples{
# creating a quarry
q <- quarry(geological.context = 2, hydrogeological.context = 4,
            piezometric.context = 1, quarry.position = 4,
            production.catchment = 4, quality.catchment = 3)
print(q)
# computing the interaction index
inter.idx <- compute.interaction(q)
print(inter.idx)
}
\keyword{package}
\keyword{neural}
\keyword{models}
\concept{discrete choice}
\concept{geology}
\concept{hydrogeology}
\concept{interaction}
