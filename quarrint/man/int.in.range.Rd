\name{int.in.range}
\alias{int.in.range}

\title{
Check if an object is an integer in a given range
}
\description{
This function checks if an object is an integer in the range
\code{[low.bound, up.bound]}.
}
\usage{
int.in.range(x, low.bound = 1, up.bound = 4)
}
\arguments{
  \item{x}{
    An object to be tested.
}
  \item{low.bound}{
    The lower bound. Default value is 1.
}
  \item{up.bound}{
    The upper bound. Default value is 4.
}
}
\value{
\code{TRUE} if x is an integer in \code{[low.bound, up.bound]},
\code{FALSE} otherwise.
}
\author{
Johan Barthelemy.

Maintainer: Johan Barthelemy \email{johan@uow.edu.au}.
}
\examples{
# test if x in [0,10]
x <- 5
int.in.range(x, low.bound = 0, up.bound = 10)
# test if y in [1,4]
y <- 10
int.in.range(y)
}
\keyword{math}
\concept{range}
\concept{integer}
