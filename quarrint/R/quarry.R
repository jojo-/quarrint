# File quarrint/R/quarry.R
# by Johan Barthelemy
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

# ------------------------------------------------------------------------------
# This file provides the functions to generate and print an object of type
# quarry, to compute the interaction indices and train the artificial neural
# network.
#
# Acknowledgements:
# This study is part of a project with the support and funding of the Public
# Service of Wallonia (SPW), in collaboration with the FEDeration of the
# Extractive Industry of Belgium (FEDIEX) and the professional union of the
# water public operators of Wallonia (Aquawal).
# ------------------------------------------------------------------------------

int.in.range <- function(x, low.bound = 1, up.bound = 4) {
  # Function to check if an object is an integer in the range
  # [low.bound, up.bound].
  #
  # Author: J. Barthelemy
  #
  # Args:
  #   x: An object.
  #   low.bound: The lower bound.
  #   up.bound: The upper bound.
  #
  # Returns: TRUE if x is an integer in [low.bound, up.bound], FALSE otherwise.

  if (length(x) > 1 | x != round(x) | x < low.bound | x > up.bound) {
    return(FALSE)
  }
  return(TRUE)
}

quarry <- function(geological.context = 1, hydrogeological.context = 1,
                   piezometric.context = 1, quarry.position = 1,
                   production.catchment = 1, quality.catchment = 1, ...) {
  # Create a quarry object.
  #
  # Author: J. Barthelemy
  #
  # Args:
  #   geological.context: The geological context of the quarry. Characterizes
  #                       the lithology and extension of the geological
  #                       formation exploited in the quarry and those of the
  #                       neighbouring geological formations that will govern
  #                       the groundwater flow directions.
  #                       The possible values are:
  #                       - 1: completely isolated by other formations with low
  #                            permeability;
  #                       - 2: limited extension and partly compartmentalized;
  #                       - 3: local extension;
  #                       - 4: regional extension.
  #   hydrogeological.context: The hydrogeological context of the quarry.
  #                            Refers to the combinations of geological
  #                            formations according to their hydrodynamic
  #                            characteristics. The possible values are:
  #                            - 1: aquiclude formation;
  #                            - 2: aquitard formation;
  #                            - 3: aquifer formation;
  #                            - 4: carbonate aquifer formation.
  #   piezometric.context: The piezometric context of the quarry, i.e. the
  #                        altimetric level of the quarry floor. Characterizes
  #                        the relative position between the quarry pit bottom
  #                        and the groundwater piezometric level.
  #                        The possible values are:
  #                        - 1: higher than the piezometric level of the water
  #                             table;
  #                        - 2: lower than the piezometric level of the water
  #                             table but higher than the river thalweg which is
  #                             the regional base level;
  #                        - 3: lower than the piezometric level of the water
  #                             table and the altimetric level of the river
  #                             thalweg which is the regional base level;
  #                        - 4: lower than the piezometric level of the water
  #                             table and the altimetric level of the river
  #                             thalweg which is not the regional level any more
  #                             (the river is perched).
  #   quarry.position: Relative position of the quarry and the water catchments.
  #                    The possible values are:
  #                    - 1: outside the drainage zone of a catchment;
  #                    - 2: in the drainage zone of a catchment;
  #                    - 3: in the distant prevention area of a catchment;
  #                    - 4: in the close prevention area of a catchment.
  #   production.catchment: Production of the catchments. Volume exploited in
  #                         catchments for public distribution in the
  #                         hydrogeological formation near the quarry.
  #                         The possible values are:
  #                         - 1: lower than 2 m3/h;
  #                         - 2: between 2 and 10 m3/h;
  #                         - 3: between 10 and 30 m3/h;
  #                         - 4: greater than 30 m3/h.
  #   quality.catchment: Potential quality of the catchments. Quality and the
  #                      potability of the groundwater.
  #                      The possible values are:
  #                         - 1: poor quality;
  #                         - 2: water potabilisable with minor treatment
  #                         - 3: good quality water;
  #                         - 4: water of exceptional quality (mineral water).
  #   ...: Further arguments passed to or from other methods. For instance if
  #        the values of the variable must be in the range [l, u] instead of
  #        [1, 4], then it can be achieved using low.bound = l and up.bound = u
  #        as the function relies on "int.in.range".
  #
  # Returns: A quarry object consisting of a list whose elements are
  #   G: The geological context of the quarry (in [0,4]).
  #   H: The hydrogeological context of the quarry (in [0,4]).
  #   Z: The piezometric context of the quarry (in [0,4]).
  #   C: The relative position of the quarry and the water catchment (in [0,4]).
  #   T: The production of the water catchments (in [0,4]).
  #   L: The potential quality of the water catchments (in [0,4]).
  #   G.dummy: A vector of binary components for the dummy variable coding of G.
  #   H.dummy: A vector of binary components for the dummy variable coding of H.
  #   Z.dummy: A vector of binary components for the dummy variable coding of Z.
  #   C.dummy: A vector of binary components for the dummy variable coding of C.
  #   T.dummy: A vector of binary components for the dummy variable coding of T.
  #   L.dummy: A vector of binary components for the dummy variable coding of L.

  # Check if the parameters are all of type "numeric"
  if (is.numeric(geological.context)      == FALSE |
      is.numeric(hydrogeological.context) == FALSE |
      is.numeric(piezometric.context)     == FALSE |
      is.numeric(quarry.position)         == FALSE |
      is.numeric(production.catchment)    == FALSE |
      is.numeric(quality.catchment)       == FALSE ) {

    stop("Error: each quarry parameter must be an integer [1,4]!")

  }

  # Checking if each parameter is an integer in [1,4]
  if (int.in.range(geological.context, ...)      == FALSE |
      int.in.range(hydrogeological.context, ...) == FALSE |
      int.in.range(piezometric.context, ...)     == FALSE |
      int.in.range(quarry.position, ...)         == FALSE |
      int.in.range(production.catchment, ...)    == FALSE |
      int.in.range(quality.catchment, ...)       == FALSE ) {

    stop("Error: some parameters are out of range!")

  }

  # Creating the quarry object
  result <- list("G" = geological.context, "H" = hydrogeological.context,
                 "Z" = piezometric.context, "C" = quarry.position,
                 "T" = production.catchment, "L" = quality.catchment)

  # Creating dummy variables
  # ... upper bound
  u <- 4
  args <- list(...)
  if ("up.bound" %in% names(args)) {
    u <- args$up.bound
  }
  # ... dummy variables
  result$G.dummy <- rep(0, u)
  result$H.dummy <- rep(0, u)
  result$Z.dummy <- rep(0, u)
  result$C.dummy <- rep(0, u)
  result$T.dummy <- rep(0, u)
  result$L.dummy <- rep(0, u)
  # ... updating the dummy variables
  result$G.dummy[result$G] <- 1
  result$H.dummy[result$H] <- 1
  result$Z.dummy[result$Z] <- 1
  result$C.dummy[result$C] <- 1
  result$T.dummy[result$T] <- 1
  result$L.dummy[result$L] <- 1

  # Updating the class attribute
  class(result) <- c("quarry", "list")

  # Returning the result
  return(result)

}

print.quarry <- function(x, verbose = FALSE, ...) {
  # S3 print method for class quarry. This method prints a brief description
  # of the input argument.
  #
  # Author: J. Barthelemy
  #
  # Args:
  #   x: The quarry object to be printed.
  #   verbose: Indicates whether the print shows the integer coding of the
  #            attributes (FALSE) or the full text description (TRUE). Default
  #            is FALSE.
  #   ...: Further arguments passed to or from other methods.

  # Formatting the output

  # ... integer coding
  G.text <- x$G
  H.text <- x$H
  Z.text <- x$Z
  C.text <- x$C
  T.text <- x$T
  L.text <- x$L

  # ... text descrition if requested
  if (verbose == TRUE) {
    G.text <- switch(x$G,
                     "completely isolated by other formations with low
                      permeability",
                     "limited extension and partly compartmentalized",
                     "local extension",
                     "regional extension")
    if (is.null(G.text)) {
      G.text <- "unknown"
    }
    H.text <- switch(x$H,
                     "aquiclude formation",
                     "aquitard formation",
                     "aquifer formation",
                     "carbonate aquifer formation")
    if (is.null(H.text)) {
      H.text <- "unknown"
    }
    Z.text <- switch(x$Z,
           "higher than the piezometric level of the water table",
           "lower than the piezometric level of the water table but higher than
            the river thalweg which is the regional base level",
           "lower than the piezometric level of the water table and the
            altimetric level of the river thalweg which is the regional base
            level",
           "lower than the piezometric level of the water table and the
            altimetric level of the river thalweg which is not the regional
            level any more (the river is perched)")
    if (is.null(Z.text)) {
      Z.text <- "unknown"
    }
    C.text <- switch(x$C,
           "outside the drainage zone of a catchment",
           "in the drainage zone of a catchment",
           "in the distant prevention area of a catchment",
           "in the close prevention area of a catchment")
    if (is.null(C.text)) {
      C.text <- "unknown"
    }
    T.text <- switch(x$T,
           "lower than 2 m3/h",
           "between 2 and 10 m3/h",
           "between 10 and 30 m3/h",
           "greater than 30 m3/h")
    if (is.null(T.text)) {
      T.text <- "unknown"
    }
    L.text <- switch(x$L,
           "poor quality",
           "water potabilisable with minor treatment",
           "good quality water",
           "water of exceptional quality (mineral water)")
    if (is.null(L.text)) {
      L.text <- "unknown"
    }
  }

  # Printing the quarry and water catchment details
  cat("Details of the quarry and the water catchment:\n")
  cat("* Hazard parameters (quarry):\n")
  cat("    Geological context             :", G.text, "\n")
  cat("    Hydrogeological context        :", H.text, "\n")
  cat("    Piezometric context            :", Z.text, "\n")
  cat("* Vulnerability parameters (water catchment)\n")
  cat("    Relative position of the quarry:", C.text, "\n")
  cat("    Production                     :", T.text, "\n")
  cat("    Potential quality              :", L.text, "\n")

  invisible(x)

}

as.data.frame.quarry <- function(x, row.names = NULL, optional = NULL,
                                 attr = "all", ...) {
  # Coerce a quarry object to a data frame.
  #
  # Author: J. Barthelemy
  #
  # Args:
  #   x: A quarry object.
  #   attr: The attributes to be retained in the data frame.
  #   row.names: Not used.
  #   optional: Not used.
  #   ...: Further arguments passed to or from other methods.
  #
  # Returns: A dataframe representing the input quarry object.

  quarry.data <- t(c(x$H.dummy, x$Z.dummy, x$G.dummy,
                     x$C.dummy, x$T.dummy, x$L.dummy))

  colnames(quarry.data) <- c("H1", "H2", "H3", "H4", "Z1", "Z2", "Z3", "Z4",
                             "G1", "G2", "G3", "G4", "C1", "C2", "C3", "C4",
                             "T1", "T2", "T3", "T4", "L1", "L2", "L3", "L4")

  quarry.df <- as.data.frame(quarry.data)

  # Subsetting the data frame
  if (attr[1] != "all") {
    quarry.df <- quarry.df[, attr]
  }

  return(quarry.df)

}

print.interaction.index <- function(x, ...) {

  cat("\nCall:\n")
  print(x$call)
  cat("\nMethod: ", x$method, "\n")

  if (x$method == "ann" | x$method == "all") {

    interaction.levels <- x$ann

    cat("\nOutput from the artificial neural network:\n")
    cat("low: ", interaction.levels$low,
        "| medium:", interaction.levels$medium,
        "| high:", interaction.levels$high,
        "| very high:", interaction.levels$very.high, "\n")
    cat("=> Predicted interaction level:", interaction.levels$idx,"\n")

  }

  if (x$method == "dc" | x$method == "all") {

    p.interaction.levels <- x$dc
    cat("\nOutput from the discrete choice model:\n")
    cat("P(low)       =", p.interaction.levels$p.low,"\n")
    cat("P(medium)    =", p.interaction.levels$p.medium,"\n")
    cat("P(high)      =", p.interaction.levels$p.high,"\n")
    cat("P(very high) =", p.interaction.levels$p.very.high,"\n")

  }

}

compute.dc <- function(x, ...) {
  # Generic method to compute the discrete choice model-based interaction index.
  #
  # Author: J. Barthelemy
  #
  # Args:
  #   x: An object.
  #   ...: Further arguments passed to or from other methods.

  UseMethod("compute.dc", x)

}

compute.dc.default <- function(x, ...) {
  # Default method of the compute.dc function. A specific method to
  # compute the interaction index using a discrete choice model has not
  # been implemented yet and the method returns the original object.
  #
  # Author: J. Barthelemy
  #
  # Args:
  #   x: An object.
  #   ...: Further arguments passed to or from other methods.
  #
  # Returns: The original object.

  warning("Cannot compute the discrete choice-based interaction index for this
           object!")
  invisible(x)

}

compute.dc.quarry <- function(x, ...) {
  # Compute the discre choice midel-based interaction index.
  #
  # Given an object of type quarry, the function computes the probabilities of
  # each level of interaction (low, medium, high and very high) using a Logit
  # discrete choice model.
  #
  # The model parameters have been estimated with BIOGEME and has an adjusted
  # rho2 of 0.609. The model is fully detailed in the paper "Interaction
  # prediction between groundwater and quarry extension using discrete choice
  # models and artificial neural networks" (Barthelemy et al., 2016).
  #
  # Note that in order to use the function, the quarry must have been
  # constructed with the function "quarry" using the default parameters.
  #
  # Author: J. Barthelemy
  #
  # Args:
  #   x: An object of type quarry.
  #   ...: Further arguments passed to or from other methods.
  #
  # Returns: A list whose alements are:
  #   p.low: The probability of a low interaction level.
  #   p.medium: The probability of a medium interaction level.
  #   p.high: The probability of a high interaction level.
  #   p.very.high: The probability of a very high interaciton level.

  # Values for the betas (note that since low is the base level, its
  # utility is set to 0, hence the associated betas are also 0)
  # ... betas for medium
  ASC.m <- -9.42
  B.G.m <- c( 0.00, -0.38, -0.39, -0.44)
  B.H.m <- c( 0.00,  0.00,  2.97,  2.97)
  B.Z.m <- c( 0.00,  2.21,  4.65,  4.59)
  B.C.m <- c( 0.00,  3.26,  5.35,  5.34)
  B.T.m <- c( 0.00,  0.45,  1.69,  4.50)
  B.L.m <- c( 0.00,  4.08,  4.64,  4.78)

  # ... betas for high
  ASC.h <- -1.97
  B.G.h <- c( 0.00,  0.00,  0.00,  0.00)
  B.H.h <- c( 0.75,  0.00,  5.11,  5.11)
  B.Z.h <- c(-5.35,  0.00,  3.88,  3.84)
  B.C.h <- c(-6.92,  0.00,  4.99,  5.35)
  B.T.h <- c(-3.57,  0.00,  2.97,  6.47)
  B.L.h <- c(-7.20,  0.00,  1.91,  3.24)

  # ... betas for very high
  ASC.v <- 24.20
  B.G.v <- c( 0.00,  0.00,  0.00,  0.00)
  B.H.v <- c(-7.36, -8.96,  0.00,  0.00)
  B.Z.v <- c(-16.9, -7.69,  0.00,  0.00)
  B.C.v <- c(-23.5, -8.48,  0.00,  0.78)
  B.T.v <- c(-13.0, -6.16,  0.00,  2.53)
  B.L.v <- c(-13.9, -3.66,  0.00,  3.30)

  # Utilities
  V.low       <- 0
  V.medium    <- ASC.m + B.G.m %*% x$G.dummy + B.H.m %*% x$H.dummy +
                 B.Z.m %*% x$Z.dummy + B.C.m %*% x$C.dummy +
                 B.T.m %*% x$T.dummy + B.L.m %*% x$L.dummy
  V.high      <- ASC.h + B.G.h %*% x$G.dummy + B.H.h %*% x$H.dummy +
                 B.Z.h %*% x$Z.dummy + B.C.h %*% x$C.dummy +
                 B.T.h %*% x$T.dummy + B.L.h %*% x$L.dummy
  V.very.high <- ASC.v + B.G.v %*% x$G.dummy + B.H.v %*% x$H.dummy +
                 B.Z.v %*% x$Z.dummy + B.C.v %*% x$C.dummy +
                 B.T.v %*% x$T.dummy + B.L.v %*% x$L.dummy

  # Logit probabilities
  sum.exp.V   <- sum(exp(c(V.low, V.medium, V.high, V.very.high)))
  p.low       <- as.double(exp(V.low) / sum.exp.V)
  p.medium    <- as.double(exp(V.medium) / sum.exp.V)
  p.high      <- as.double(exp(V.high) / sum.exp.V)
  p.very.high <- as.double(exp(V.very.high) / sum.exp.V)

  # Returning the resulting probabilities
  result <- list("p.low" = p.low, "p.medium" = p.medium, "p.high" = p.high,
                 "p.very.high" = p.very.high)
  return(result)

}

compute.ann <- function(x, ...) {
  # Generic method to compute the neural network-based interaction index.
  #
  # Author: J. Barthelemy
  #
  # Args:
  #   x: An object.
  #   ...: Further arguments passed to or from other methods.


  UseMethod("compute.ann", x)

}

compute.ann.default <- function(x, ...) {
  # Default method of the compute.ann function. A specific method to
  # compute the interaction index using an artificial neural network has not
  # been implemented yet and the method returns the original object.
  #
  # Author: J. Barthelemy
  #
  # Args:
  #   x: An object.
  #   ...: Further arguments passed to or from other methods.
  #
  # Returns: The original object.

  warning("Cannot compute the neural network-based interaction index for this
           object!")
  invisible(x)

}

compute.ann.quarry <- function(x, ann = NULL, rep = 1, ...) {
  # Compute the neural network-based interaction index.
  #
  # Given an object of type quarry, a neural network computes the interaction
  # index.
  #
  # The neural network provided with the package has a feed-forward design and
  # has a hidden layer of 7 nodes. It takes as an input a quarry constructed
  # with the function "quarry" using the default parameters. This neural
  # network is fully detailed in "Interaction prediction between groundwater
  # and quarry extension using discrete choice models and artificial neural
  # networks" (Barthelemy et al., 2016).
  #
  # It is possible to use another neural network that has been trained with the
  # function "train.ann" of the package (see the associated documentation for
  # more details).
  #
  # See the documentation of the package "neuralnet" and its function "compute"
  # for more details.
  #
  # Author: J. Barthelemy
  #
  # Args:
  #   x: A quarry object.
  #   ann: The neural network used to estimate the interaction index. By default
  #        (if set to NULL) it uses a neural network provided by the package.
  #   rep: The repetition of ann to be used.
  #   ...: Further arguments passed to or from other methods. For instance, see
  #        the documentation of "compute" in the "neuralnet" package.
  #
  # Returns: A list whose elements are:
  #   low: The output of the ann for a low interaction level.
  #   medium: The output of the ann for a medium interaction level.
  #   high: The output of the ann for a high interaction level.
  #   very.high: The output of the ann for a very high interaciton level.
  #   idx: a string with the level of interaction.

  # Importing the default neural network
  if (is.null(ann) == TRUE){
    ann <- ann.interactions
  }
  
  # Coercing the quarry to a data frame, keeping only the necessary variables
  quarry.df <- as.data.frame(x, attr = ann$model.list$variables, ...)
  ann.out <- compute(ann, quarry.df, rep = rep)$net.result

  # Computing the interaction level
  interaction.idx <- which.max(ann.out)
  interaction.lev <- c("low", "medium", "high", "very high")

  # Returning the result
  result <- list("low" = ann.out[1], "medium" = ann.out[2], "high" = ann.out[3],
                 "very.high" = ann.out[4],
                 "idx" = interaction.lev[interaction.idx])
  return(result)

}

compute.interaction <- function(x, ...) {
  # Generic method to compute the interaction indices.
  #
  # Author: J. Barthelemy
  #
  # Args:
  #   x: An object.
  #   ...: Further arguments passed to or from other methods.

  UseMethod("compute.interaction", x)

}

compute.interaction.default <- function(x, ...) {
  # Default method of the compute.interaction function. A specific method to
  # compute the interaction indices has not been implemented yet and the
  # method returns the original object.
  #
  # Author: J. Barthelemy
  #
  # Args:
  #   x: An object.
  #   ...: Further arguments passed to or from other methods.
  #
  # Returns: The original object.

  warning("Can not compute the quarry-water catchment interaction for this
           object!")
  invisible(x)

}

compute.interaction.quarry <- function(x, method = "all",
                                       fun.ann = compute.ann,
                                       fun.dc = compute.dc, ...) {
  # Computes and prints the interaction indices between a quarry and
  # groundwater. The function implements 2 indices based on hazard and
  # vulnerability parameters. The levels of each index are low, medium, high and
  # very high.
  #
  # One index is based on a discrete choice while the other is based on an
  # artificial neural network. See compute.ann and compute.dc for more details
  # about those indices and the outputs of the function.
  #
  # Author: J. Barthelemy
  #
  # Args:
  #   x: A quarry object.
  #   method: A string specifying the method to use for computing the index.
  #           Possible values are: "dc" for the discrete choice, "ann" for the
  #           neural network, "all" for both methods.
  #   fun.ann: Specify which function to be used for the neural network method.
  #            By default it is "compute.ann", but it can be replaced by any
  #            function returning the same outputs.
  #   dc.ann: Specify which function to be used for the discrete choice method.
  #           By default it is "compute.dc", but it can be replaced by any
  #           function returning the same outputs.
  #   ...: Further arguments passed to or from other methods. For instance,
  #        the "ann" parameter of the function "compute.ann" can be pass to
  #        use a neural network different than the default one. See the
  #        documentation of "compute.ann" for more details.
  #
  # Returns: A list whose elements are:
  #   dc: The output from the "compute.dc" function (if method = "all" or "dc").
  #   ann: The output from the "compute.ann" function (if method = "all"or
  #        "ann").

  result <- list()

  if (method == "ann" | method == "all") {

    if (is.null(fun.ann) == TRUE) {
      warning("Neural network not specified, switching to default!")
      fun.ann <- compute.ann
    }

    interaction.levels <- fun.ann(x, ...)
    result$ann <- interaction.levels

  }

  if (method == "dc" | method == "all") {

    if (is.null(fun.dc) == TRUE) {
      warning("Dicrete choice model not specified, switching to default!")
      fun.dc <- compute.dc
    }

    p.interaction.levels <- fun.dc(x, ...)
    result$dc <- p.interaction.levels

  }

  # Creating the S3 object, adding the call and the method and returning it
  result$call <- match.call()
  result$method <- method
  class(result) <- c("interaction.index", "list")
  return(result)

}

train.ann <- function(var = c("H","Z","G","C","T","L"), 
                      data = quarrint::quarries, hidden = 7, rep = 1, ...) {
  # Training an artificial neural network for interaction prediction.
  # The neural network predict whether the level of interaction is low, medium
  # high or very high. The user can specify:
  # - the explanatory variables to be used;
  # - the data frame used to train and validate the network;
  # - the structure of the hidden layers;
  # - the number of repetitions for the neural network training.
  #
  # The trained neural network can be used in the compute.ann function as a
  # replacement of the default one.
  #
  # The function relies on the function "neuralnet" of the "neuralnet" package
  # for the training.
  #
  # Author: J. Barthelemy
  #
  # Args:
  #   var: The explanatory variable to be used. By default, all the variables
  #        in the default data frame are used. Note that the variables must be
  #        categorical (coded with integers) and will be transformed in dummy
  #        variables. For instance if "X" has 5 possible values (1,2,3,4,5),
  #        then it will be replaced by the binary variables X1, X2, X3, X4, X5.
  #   data: The training and validation dataframe. It must contain the
  #         variables listed in "var" and the dependent binary variables
  #         "low", "medium", "high" and "very.high" representing the interaction
  #         level. By default, it uses the dataframe "quarries" from the
  #         package.
  #   hidden: A vector of integer detailing the structure of the hidden layers.
  #           For instance if we want 2 hidden layers with 4 and 2 nodes
  #           respectively, then it must be set to (2, 4). The default is 7,
  #           i.e. 1 hidden layer of 7 nodes.
  #   rep: The number of repetition of the neural network to be computed.
  #   ...: Further arguments passed to or from other methods. See the
  #        documentation of "neuralnet" from the package "neuralnet".
  #
  # Return: A list whose elements are:
  #   ann: The trained neural network which is an object of class "nn" (see the
  #        package neural net).
  #   prop.correct.prediction: a list containing the proportion of correct
  #                            prediction on the validation data for each
  #                            repetition of the neural network.

  cat("Training the ann...\n")

  # Training and validation data sets
  # ... determining observations index for training
  data.train.idx <- c(sample(1:nrow(data), 0.75*nrow(data)))
  # ... determining observations index for validation
  data.test.idx <- setdiff(1:nrow(data), data.train.idx)

  # Creating the formula for the neural network
  var.txt <- character()
  for (v in var) {
    temp <- paste0(v, 1:max(data[,v]))
    var.txt <- c(var.txt, temp)
  }

  f <- as.formula(paste("low + medium + high + very.high ~ ",
                           paste(var.txt, collapse = "+")))

  cat("Formula:\n")
  print(f, showEnv = FALSE)

  # Training the neural network
  ann <- neuralnet(f, data = data[data.train.idx, ],
                   hidden = hidden, rep = rep, ...)


  # Testing the neural network
  data.test <- data[data.test.idx, ann$model.list$variables]
  prop.correct.pred = list()

  if (is.null(ann$weights)) {
    warning("Could not fit a neural network. Try to increase stepmax
            (see neuralnet documentation). Returning NULL")
    return(NULL)
  }

  for (i in 1:length(ann$weights)) {

    # ... applying repetition i
    ann.out <- compute(ann, data.test, rep = i)

    # ... formatting output and casting it to an integer
    clean.output <- cbind(data$INTERACTION[data.test.idx],
                          as.data.frame(ann.out$net.result))
    colnames(clean.output) <- c("INTERACTION", "low", "medium",
                                "high", "very.high")
    PREDICTED <- apply(cbind(clean.output$low, clean.output$medium,
                             clean.output$high, clean.output$very.high),
                       1, which.max)
    clean.output.df <- as.data.frame(cbind(clean.output$INTERACTION, PREDICTED))
    colnames(clean.output.df) <- c("EXPECTED","OUTPUT")

    # ... computing proportion of model outputs equals to expected
    dif.expected.ann <- abs(clean.output.df$EXPECTED -
                              clean.output.df$OUTPUT) < 1
    prop.correct.pred[[i]] <- as.double(prop.table(table(dif.expected.ann))[2])
    cat("Proportion of correct prediction for repetition", i, ":",
        prop.correct.pred[[i]], "\n")

  }

  cat("Done!\n")

  # Returning the trained neural networks
  result <- list("ann" = ann, "prop.correct.prediction" = prop.correct.pred)
  return(result)

}
