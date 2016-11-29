\name{as.data.frame.quarry}
\alias{as.data.frame.quarry}

\title{
Coerce a Quarry to a Data Frame
}
\description{
Coerce an object of type \code{quarry} to a \code{data.frame}. The attributes
to be stored in the data frame can be specified (default: keeping every
attributes).
}
\usage{
\method{as.data.frame}{quarry}(x, row.names = NULL, optional = NULL, 
              attr = "all", ...)
}

\arguments{
  \item{x}{
    An object of type \code{quarry}.
}
  \item{row.names}{
    Not used.
}
  \item{optional}{
    Not used.
}
  \item{attr}{
    A vector listing the dummy variables to be retained in the data frame.
}
  \item{\dots}{
    Further arguments passed to or from other methods.
}
}
\value{
A data frame with the columns listed in \code{attr}.
}
\author{
Johan Barthelemy.

Maintainer: Johan Barthelemy \email{johan@uow.edu.au}.
}
\note{
The data frame will only stores the dummy variables of the quarry.
}
\seealso{
\code{\link[base:as.data.frame]{as.data.frame}} for the generic method.
}
\examples{
# creating a quarry
q <- quarry(geological.context = 2, hydrogeological.context = 4,
            piezometric.context = 1, quarry.position = 4,
            production.catchment = 4, quality.catchment = 3)

# coercing to a data frame
q.df <- as.data.frame(q)

# coercing to a data frame, keeping only "H1", "Z1" and "C2"
q.df.subset <- as.data.frame(q, attr = c("H1","Z1","C2"))
}
\keyword{data}
\keyword{manip}
\concept{data frame}

