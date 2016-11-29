\name{compute.interaction}
\alias{compute.interaction}
\alias{compute.interaction.default}
\alias{compute.interaction.quarry}
\alias{print.interaction.index}
\alias{interaction.index}
\title{
Compute the Interaction Index for a Quarry
}
\description{
Computes the interaction indices between a quarry and groundwater and returns
an object of class \code{interaction.index}. The function implements 2 indices
based on hazard and vulnerability parameters. The levels of each index are low,
medium, high and very high.
}
\usage{
\method{compute.interaction}{quarry}(x, method = "all", fun.ann = compute.ann,
                    fun.dc = compute.dc, ...)
}
\arguments{
  \item{x}{
    A quarry object.
}
  \item{method}{
    A string specifying the method to use for computing the index.
    Possible values are: \code{dc} for the discrete choice, \code{ann} for the
    neural network, \code{all} for both methods. Default is \code{all}.
}
  \item{fun.ann}{
    Specify which function to be used for the neural network method.
    By default it is \code{compute.ann}, but it can be replaced by any
    function returning the same outputs.
}
  \item{fun.dc}{
    Specify which function to be used for the discrete choice method.
    By default it is \code{compute.dc}, but it can be replaced by any
    function returning the same outputs.
}
  \item{\dots}{
    Further arguments passed to or from other methods. For instance,
    For instance the parameter \code{ann} of the function
    \code{\link[quarrint:compute.ann]{compute.ann}} can be passed to use a
    neural network different from the default one.
}
}
\value{
An interaction.index object consisting of a list whose elements are
  \item{method}{
    The selected method for computing the interaction index.
}
  \item{dc}{
    The discrete choice-based interaction index computed by the function
    specified in \code{fun.dc} (if \code{method = "all" or "dc"}).
}
  \item{ann}{
    The neural network-based interaction index computed by the function
    specified in \code{fun.ann} (if \code{method = "all" or "ann"}).
}
  \item{call}{
    The matched call.
}
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
\seealso{
\code{\link[quarrint:compute.ann]{compute.ann}} and
\code{\link[quarrint:compute.dc]{compute.dc}} for more details about the
different interaction indices and the outputs of the corresponding functions.

\code{\link[quarrint:train.ann]{train.ann}} to train a neural network that
can be used with this function.

\code{\link[base:print]{print}} to print a \code{interaction.index} object.
}
\examples{
# creating a quarry
q <- quarry(geological.context = 2, hydrogeological.context = 4,
            piezometric.context = 1, quarry.position = 4,
            production.catchment = 4, quality.catchment = 3)

# computing the interaction index
inter.idx <- compute.interaction(q)
print(inter.idx)
}
\keyword{models}
\keyword{neural}
\concept{discrete choice}



