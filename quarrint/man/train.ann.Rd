\name{train.ann}
\alias{train.ann}
\title{
Training an Artificial Neural Network for Interaction Prediction.
}
\description{
The function trains a neural network to be used with the functions
\code{\link[quarrint:compute.interaction]{compute.interaction}} and
\code{\link[quarrint:compute.ann]{compute.ann}}. The neural network can then
be used to predict whether the level of interaction between a quarry and the
groundwater is \emph{low}, \emph{medium}, \emph{high} or \emph{very high}.

The user can specify:
\itemize{
\item the explanatory variables to be used;
\item the data frame used to train and validate the network;
\item the structure of the hidden layers;
\item the number of repetitions for the neural network training.
}
}
\usage{
train.ann(var = c("H", "Z", "G", "C", "T", "L"), data = quarrint::quarries, 
          hidden = 7, rep = 1, ...)
}
\arguments{
  \item{var}{
    The explanatory variable to be used. By default, all the variables in the
    default data frame are used. Note that the variables must be categorical
    (coded with integers) and will be transformed in dummy variables. For
    instance if \code{X} has 5 possible values (1,2,3,4,5), then it will be
    replaced by the binary variables \code{X1}, \code{X2}, \code{X3}, \code{X4},
    \code{X5}.
}
  \item{data}{
    The training and validation dataframe. It must contain the
    variables listed in \code{"var"} and the dependent binary variables
    \code{"low"}, \code{"medium"}, \code{"high"} and \code{"very.high"}
    representing the interaction level. By default, it uses the dataframe
    \code{"quarries"} from the package.
}
  \item{hidden}{
    A vector of integer detailing the structure of the hidden layers. For instance
    if we want 2 hidden layers with 4 and 2 nodes respectively, then it must be
    it to (2, 4). The default is 7, i.e. 1 hidden layer of 7 nodes.
}
  \item{rep}{
    The number of repetition of the neural network to be computed.
}
  \item{\dots}{
    Further arguments passed to or from other methods. See the  documentation of
    "neuralnet" from the package "neuralnet".
}
}
\value{
A list whose elements are:
\item{ann}{
  A \code{nn} object containing the trained neural network.
}
\item{prop.correct.prediction}{
  A list detailing for each repetition of the neural network the proportion of
  correct predictions.
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

The function relies on the function \code{\link[neuralnet:neuralnet]{neuralnet}}
of the \pkg{neuralnet} package to generate an object of type \code{nn}
containing the trained neural network.

\code{\link[quarrint:compute.interaction]{compute.interaction}} and
\code{\link[quarrint:compute.ann]{compute.ann}} to use the trained neural
network.

The data frame \code{\link[quarrint:quarries]{quarries}}.
}
\examples{
\dontrun{
# training a neural network using the attribues H and T as predictors,
# with 2 hidden layers of 2 nodes each and computing 2 replications
r.ann <- train.ann(var = c("H", "T", "L", "Z"), hidden = c(2, 2), rep = 2)

# using the ann to compute the interaction
q <- quarry(geological.context = 2, hydrogeological.context = 4,
            piezometric.context = 1, quarry.position = 4,
            production.catchment = 4, quality.catchment = 3)
compute.interaction(q, method="ann", ann = r.ann$ann)
}
}
\keyword{neural}

