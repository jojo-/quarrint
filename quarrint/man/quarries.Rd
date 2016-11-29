\name{quarries}
\alias{quarries}
\docType{data}
\title{
Quarries and Groundwater Interaction
}
\description{
Each record of the data frame details a quarry and a groudwater as well as their
level of interaction. This level of interaction correspond to the environmental
risk, or potential impact of the quarry on the regional hydrology.

Depending on the parameters of the quarry and the groundwater, there will be a
\emph{low}, \emph{medium}, \emph{high} or \emph{very high} level of interaction.
Hence, the interaction determines the level of investigation of the feasibility
study to undertake before considering any extension of the quarry:
\itemize{
  \item{low}{
    hydrogeology characterization;
  }
  \item{medium}{
    \code{low} + piezometric monitoring;
  }
  \item{high}{
    \code{medium} + steady state (static) mathematical model;
  }
  \item{very high}{
    \code{high} + transient state (dynamic) mathematical model.
  }
}

The 6 retained parameters are grouped in 2 categories:
\itemize{
  \item the geological, hydrogeological and piezometric contexts defining the
    hazard that a quarry represents;
  \item the relative position of the quarry and the water catchments, the
    production of the catchments and the potential quality of the groundwater
    characterising the vulnerability of the groundwater resources.
}
Each of those parameters is classified into 4 modalities.


}
\usage{data("quarries")}
\format{
  A data frame with 3327 observations on the following 36 variables.
  \describe{
    \item{\code{ID}}{
      The id of the observation.
    }
    \item{\code{INTERACTION}}{
      An integer in [1, 4] giving the interaction level.
      \enumerate{
        \item{Low.}
        \item{Medium.}
        \item{High.}
        \item{Very high.}
      }
    }
    \item{\code{H}}{
      An integer in [1, 4]. Hydrogeological context. Refers to the combinations
      of geological formations according to their hydrodynamic characteristics.
      \enumerate{
        \item{Aquiclude formation.}
        \item{Aquitard formation.}
        \item{Aquifer formation.}
        \item{Carbonate aquifer formation.}
      }
    }
    \item{\code{Z}}{
      An integer in [1, 4]. Piezometric context: altimetric level of the quarry
      floor. Characterizes the relative position between the quarry pit bottom
      and the groundwater piezometric level.
      \enumerate{
        \item{Higher than the piezometric level of the water table.}
        \item{Lower than the piezometric level of the water table but higher than
              the river thalweg which is the regional base level.}
        \item{Lower than the piezometric level of the water table and the
              altimetric level of the river thalweg which is the regional base
              level.}
        \item{Lower than the piezometric level of the water table and the
              altimetric level of the river thalweg which is not the regional
              level any more (the river is perched).}
      }
    }
    \item{\code{G}}{
      An integer in [1, 4]. Geological context. Characterizes the lithology and
      extension of the geological formation exploited in the quarry and those of
      the neighbouring geological formations that will govern the groundwater
      flow directions.
      \enumerate{
        \item{Completely isolated by other formations with low permeability.}
        \item{Limited extension and partly compartmentalized.}
        \item{Local extension.}
        \item{Regional extension.}
      }
    }
    \item{\code{C}}{
      An integer in [1, 4]. Relative position of the quarry and the water
      catchments. Catchments (well, spring, gallery, etc.) for public
      distribution of drinking water are threatened by various sources of
      pollution. Closer a quarry gets to the catchment, greater its impact may
      be important. Consequently, 4 successive perimeters, within which the
      activities and facilities are regulated, are set up around the catchment
      based on the velocity of groundwater (transfert time).
      \enumerate{
        \item{Outside the drainage zone of a catchment.}
        \item{In the drainage zone of a catchment.}
        \item{In the distant prevention area of a catchment (50 days of delay in
              case of aquifer contamination).}
        \item{In the close prevention area of a catchment (24 hours of delay in
              case of aquifer contamination).}
      }
    }
    \item{\code{T}}{
      An integer in [1, 4]. Production of the catchments. Volume exploited in
      catchments for public distribution in the hydrogeological formation near
      the quarry.
      \enumerate{
        \item{Lower than 2 m3/h.}
        \item{Between 2 and 10 m3/h.}
        \item{Between 10 and 30 m3/h.}
        \item{Greater than 30 m3/h.}
      }
    }
    \item{\code{L}}{
      An integer in [1, 4]. Potential quality of the catchments. Quality and the
      potability of the groundwater.
      \enumerate{
        \item{Poor quality.}
        \item{Water potabilisable with minor treatment.}
        \item{Good quality water.}
        \item{Water of exceptional quality (mineral water).}
      }
    }
    \item{\code{low}}{
      An integer set to 1 if \code{INTERACTION} = 1, 0 otherwhise.
    }
    \item{\code{medium}}{
      An integer set to 1 if \code{INTERACTION} = 2, 0 otherwhise.
    }
    \item{\code{high}}{
      An integer set to 1 if \code{INTERACTION} = 3, 0 otherwhise.
    }
    \item{\code{very.high}}{
      An integer set to 1 if \code{INTERACTION} = 4, 0 otherwhise.
    }
    \item{\code{H1}}{
      An integer set to 1 if \code{H} = 1, 0 otherwhise.
    }
    \item{\code{H2}}{
      An integer set to 1 if \code{H} = 2, 0 otherwhise.
    }
    \item{\code{H3}}{
      An integer set to 1 if \code{H} = 3, 0 otherwhise.
    }
    \item{\code{H4}}{
      An integer set to 1 if \code{H} = 4, 0 otherwhise.
    }
    \item{\code{Z1}}{
      An integer set to 1 if \code{Z} = 1, 0 otherwhise.
    }
    \item{\code{Z2}}{
      An integer set to 1 if \code{Z} = 2, 0 otherwhise.
    }
    \item{\code{Z3}}{
      An integer set to 1 if \code{Z} = 3, 0 otherwhise.
    }
    \item{\code{Z4}}{
      An integer set to 1 if \code{Z} = 4, 0 otherwhise.
    }
    \item{\code{G1}}{
      An integer set to 1 if \code{G} = 1, 0 otherwhise.
    }
    \item{\code{G2}}{
      An integer set to 1 if \code{G} = 2, 0 otherwhise.
    }
    \item{\code{G3}}{
      An integer set to 1 if \code{G} = 3, 0 otherwhise.
    }
    \item{\code{G4}}{
      An integer set to 1 if \code{G} = 4, 0 otherwhise.
    }
    \item{\code{C1}}{
      An integer set to 1 if \code{C} = 1, 0 otherwhise.
    }
    \item{\code{C2}}{
      An integer set to 1 if \code{C} = 2, 0 otherwhise.
    }
    \item{\code{C3}}{
      An integer set to 1 if \code{C} = 3, 0 otherwhise.
    }
    \item{\code{C4}}{
      An integer set to 1 if \code{C} = 4, 0 otherwhise.
    }
    \item{\code{T1}}{
      An integer set to 1 if \code{T} = 1, 0 otherwhise.
    }
    \item{\code{T2}}{
      An integer set to 1 if \code{T} = 2, 0 otherwhise.
    }
    \item{\code{T3}}{
      An integer set to 1 if \code{T} = 3, 0 otherwhise.
    }
    \item{\code{T4}}{
      An integer set to 1 if \code{T} = 4, 0 otherwhise.
    }
    \item{\code{L1}}{
      An integer set to 1 if \code{L} = 1, 0 otherwhise.
    }
    \item{\code{L2}}{
      An integer set to 1 if \code{L} = 2, 0 otherwhise.
    }
    \item{\code{L3}}{
      An integer set to 1 if \code{L} = 3, 0 otherwhise.
    }
    \item{\code{L4}}{
      An integer set to 1 if \code{L} = 4, 0 otherwhise.
    }
  }
}
\source{
  Deparment of Geology, University of Namur, Namur, Belgium -
  \url{https://www.unamur.be/en/sci/geology}
}
\references{
  Barthelemy, J., Carletti, T., Collier L., Hallet,V., Moriame, M.,
  Sartenaer, A. (2016)
  Interaction prediction between groundwater and quarry extension using discrete
  choice models and artificial neural networks
  \emph{Environmental Earth Sciences} (in press)

  Collier, L., Barthelemy, J., Carletti, T., Moriame, M., Sartenaer, A.,
  Hallet, V. (2015)
  Calculation of an Interaction Index between the Extractive Activity and
  Groundwater Resources
  \emph{Energy Procedia} 76, 412-420
}
\examples{
data(quarries)
# histogram of INTERACTION levels
barplot(table(quarries$INTERACTION))
}
\keyword{datasets}
