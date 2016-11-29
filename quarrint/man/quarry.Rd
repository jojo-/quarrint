\name{quarry}
\alias{quarry}
\alias{print.quarry}
\title{
Create a Quarry Object
}
\description{
This function create a S3 object of type \code{quarry} characterizing a quarry
and the groundwater in the quarry's area.

A \code{quarry} object has 6 attributes, each classified (by default) in 4
modalities coded as an integer in the range [1, 4]. These attributes are grouped
in 2 distinct categories:
\itemize{
  \item the geological (\code{geological.context}), hydrological
        (\code{hydrogeological.context}) and piezometric
        (\code{piezometric.context}) contexts defining the hazard that a quarry
        represents;
  \item the relative position of the quarry and the water catchments
        (\code{quarry.position}), the production of the catchments
        (\code{production.catchment}) and the potential quality
        (\code{quality.catchment})of the groundwater characterising the
        vulnerability of the groundwater ressources.
}
The possible values for each attribue are described in the Argument Section.

}
\usage{
quarry(geological.context = 1, hydrogeological.context = 1,
       piezometric.context = 1, quarry.position = 1, production.catchment = 1,
       quality.catchment = 1, ...)

\method{print}{quarry}(x, verbose = FALSE, ...)

}
\arguments{
  \item{geological.context}{
    The geological context of the quarry. Characterizes the lithology and
    extension of the geological formation exploited in the quarry and those of
    the neighbouring geological formations that will govern the groundwater flow
    directions. By default the possible values are:
    \itemize{
      \item 1: completely isolated by other formations with low permeability
            (default);
      \item 2: limited extension and partly compartmentalized;
      \item 3: local extension;
      \item 4: regional extension.
    }
}
  \item{hydrogeological.context}{
    The hydrogeological context of the quarry. Refers to the combinations of
    geological formations according to their hydrodynamic characteristics. By
    default the possible values are:
    \itemize{
      \item 1: aquiclude formation (default);
      \item 2: aquitard formation;
      \item 3: aquifer formation;
      \item 4: carbonate aquifer formation.
    }
}
  \item{piezometric.context}{
    The piezometric context of the quarry, i.e. the altimetric level of the
    quarry floor. Characterizes the relative position between the quarry pit
    bottom and the groundwater piezometric level. By default the possible values
    are:
    \itemize{
      \item 1: higher than the piezometric level of the water table (default);
      \item 2: lower than the piezometric level of the water table but higher
               than the river thalweg which is the regional base level;
      \item 3: lower than the piezometric level of the water table and the
               altimetric level of the river thalweg which is the regional base
               level;
      \item 4: lower than the piezometric level of the water table and the
               altimetric level of the river thalweg which is not the regional
               level any more (the river is perched).
    }
}
  \item{quarry.position}{
    Relative position of the quarry and the water catchments. By default the
    possible values are:
    \itemize{
      \item 1: outside the drainage zone of a catchment (default);
      \item 2: in the drainage zone of a catchment;
      \item 3: in the distant prevention area of a catchment;
      \item 4: in the close prevention area of a catchment.
    }
}
  \item{production.catchment}{
    Production of the catchments. Volume exploited in catchments for public
    distribution in the hydrogeological formation near the quarry. By default
    the possible values are:
    \itemize{
      \item 1: lower than 2 m3/h (default);
      \item 2: between 2 and 10 m3/h;
      \item 3: between 10 and 30 m3/h;
      \item 4: greater than 30 m3/h.
    }
}
  \item{quality.catchment}{
    Potential quality of the catchments. Quality and the potability of the
    groundwater. By default the possible values are:
    \itemize{
      \item 1: poor quality (default);
      \item 2: water potabilisable with minor treatment
      \item 3: good quality water;
      \item 4: water of exceptional quality (mineral water).
    }
}
  \item{verbose}{
    If set to TRUE, then the \code{\link[base:print]{print}} method will
    print the description of parameters' values instead of the integer value.
    Default is FALSE. Note that this parameter is only meaningfull when the
    parameters have values in the default range [1, 4].
}
  \item{\dots}{
    Further arguments passed to or from other methods. For instance if
    the values of the variables must be in the range \code{[l, u]} instead of
    \code{[1, 4]}, then it can be achieved using \code{low.bound = l} and
    \code{up.bound = u} as the function relies on
    \code{\link[quarrint:int.in.range]{int.in.range}}.
}
  \item{x}{
    An object of type \code{quarry}.
}
}
\value{
A quarry object consisting of a list whose elements are
\item{G}{
  The geological context of the quarry.
}
\item{H}{
  The hydrogeological context of the quarry.
}
\item{Z}{
  The piezometric context of the quarry.
}
\item{C}{
  The relative position of the quarry and the water catchment.
}
\item{T}{
  The production of the water catchments.
}
\item{L}{
  The potential quality of the water catchments.
}
\item{G.dummy}{
  A vector of binary components for the dummy variable encoding of \code{G}.
}
\item{H.dummy}{
  A vector of binary components for the dummy variable encoding of \code{H}.
}
\item{Z.dummy}{
  A vector of binary components for the dummy variable encoding of \code{Z}.
}

\item{C.dummy}{
  A vector of binary components for the dummy variable encoding of \code{C}.
}
\item{T.dummy}{
  A vector of binary components for the dummy variable encoding of \code{T}.
}
\item{L.dummy}{
  A vector of binary components for the dummy variable encoding of \code{L}.
}
}
\references{
  Barthelemy, J., Carletti, T., Collier L., Hallet, V., Moriame, M.,
  Sartenaer, A. (2016)
  Interaction prediction between groundwater and quarry extension using discrete
  choice models and artificial neural networks
  \emph{Environmental Earth Sciences} (sumbitted)

  Collier, L., Barthelemy, J., Carletti, T., Moriame, M., Sartenaer, A.,
  Hallet, V. (2015)
  Calculation of an Interaction Index between the Extractive Activity and
  Groundwater Resources
  \emph{Energy Procedia} 76, 412-420
}
\author{
Johan Barthelemy.

Maintainer: Johan Barthelemy \email{johan@uow.edu.au}.
}
\note{
By default, the modalities are integer in [1,4], but that can be changed
to be an integer in any given range by passing in \code{...} the arguments
\code{low.bound} and \code{up.bound} of the function
\code{\link[quarrint:int.in.range]{int.in.range}}.
}
\seealso{
\code{\link[quarrint:int.in.range]{int.in.range}} to a use custom range for the
values of the paramters.

\code{\link[base:print]{print}} to print a \code{quarry} object.

\code{\link[quarrint:as.data.frame.quarry]{as.data.frame}} to coerce a quarry to
a data frame.

\code{\link[quarrint:compute.interaction]{compute.interaction}} to predict the
interaction between between the quarry and the groundwater.
}
\examples{
# creating a quarry for which every parameter is within the default range
q1 <- quarry(geological.context = 2, hydrogeological.context = 4,
             piezometric.context = 1, quarry.position = 4,
             production.catchment = 4, quality.catchment = 3)
print(q1, verbose = TRUE)

# creating a quarry for which the parameters are within a custom range [0, 10]
q2 <- quarry(geological.context = 8, hydrogeological.context = 4,
             piezometric.context = 0, quarry.position = 4,
             production.catchment = 6, quality.catchment = 3,
             low.bound = 0, up.bound = 10)
print(q2)
}
\concept{quarry}
