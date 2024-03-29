% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aggr_grid.r
\name{aggr_grid}
\alias{aggr_grid}
\title{A Function to Aggregate Grid Data to a Different Cell Size}
\usage{
aggr_grid(
  data,
  x,
  y,
  t = NULL,
  var,
  cellSize = 2,
  cellOffsetX = 0,
  cellOffsetY = 0,
  FUN,
  ignrNA = TRUE
)
}
\arguments{
\item{data}{A data frame}

\item{x}{Characters for the x coordinate variable name}

\item{y}{Characters for the y coordinate variable name}

\item{t}{Characters for the time variable name; default = NULL}

\item{var}{Characters for the name of the variable whose aggregated value to be computed}

\item{cellSize}{A natural number indicating the size by which a cell is multiplied; default = 2}

\item{cellOffsetX}{A natural number (or zero) indicating how much the starting cell of aggregation is offset along the x coordinate; default = 0}

\item{cellOffsetY}{A natural number (or zero) indicating how much the starting cell of aggregation is offset along the y coordinate; default = 0}

\item{FUN}{A function to apply to aggregate cells, for example, mean, median, or max}

\item{ignrNA}{Ignore missing values when grid cells are aggregated; default = TRUE}
}
\value{
A data frame containing the x and y coordinate (whose colunms are named "x" and "y" respectively), the time variable if any (whose column is named "t"), and the aggregated \code{var} (which will have the same name as the original data)
}
\description{
This function aggregates grid data to a different cell size.
}
\details{
This function aggregates grid data to a different cell size. It works both
for cross-sectional data and for cross-sectional time-series data.

It can handle only one variable to be aggregated each time and returns a
data frame containing the x and y coordinate, the time variable if any,
and the aggregated variable. If there are more than one variable to be
aggregated, apply the function to every one of the variables separately
with the same parameter values for \code{x}, \code{y}, \code{t} (if used),
\code{cellSize}, \code{cellOffsetX}, and \code{cellOffsetY}, while changing
inputs for \code{var} and \code{FUN} (if necessary). Then, combine all
separate data frames into one data frame using the x and y coordinates
(and the time variable if any). See the example below.

By default, \code{FUN} is applied over the non-missing values of all the
grid cells to be aggregated to a larger one (i.e., \code{ignrNA = TRUE}).
If \code{ignrNA = TRUE} and a variable has missing values for all the grid
cells to be aggregated to a larger one, the returned data frame will
not contain this larger cell. If \code{ignrNA = FALSE}, the returned data
frame will assign a missing value to the larger cell, regardless of a
variable having the missing value for only some or all of the grid cells
to be aggregated to a larger one.

For a theoretical rationale and an empirical example, please see the
following paper:

Suzuki, Akisato. 2022. "Uncertainty in Grid Data: A Theory and Comprehensive
Robustness Test." Quality & Quantity. https://doi.org/10.1007/s11135-022-01555-x.

If you use this package, please cite the following items:

Suzuki, Akisato. 2022. "Uncertainty in Grid Data: A Theory and Comprehensive
Robustness Test." Quality & Quantity. https://doi.org/10.1007/s11135-022-01555-x.

Dornschneider-Elkink, Johan A., and Akisato Suzuki. 2022. "rbstgrid: An R
Package to Aggregate Grid Data to a Different Cell Size." R package version 1.0.0. https://github.com/AkisatoSuzuki/rbstgrid.
}
\section{Authors}{

Author: Johan A. Dornschneider-Elkink (\email{jos.elkink@ucd.ie})

Author & Maintainer: Akisato Suzuki (\email{akisato.suzuki@gmail.com})
}

\examples{
\dontrun{
# Aggregate grid data by doubling its cell size and
# computing the mean of the variable over four cells to be aggregated
agg <- aggr_grid(data = data, x = "x", y = "y", t = "t", var = "var", cellSize = 2, FUN = mean)

# Aggregate grid data by tripling its cell size and
# computing the median of the variable over nine cells to be aggregated
agg <- aggr_grid(data = data, x = "x", y = "y", t = "t", var = "var", cellSize = 3, FUN = median)

# Apply the function to several variables within the same grid data and
# then merge each output using the dplyr package (Wickham et al. 2020)
aggvar1 <- aggr_grid(data = data, x = "x", y = "y", t = "t", var = "var1",
                      cellSize = 4, cellOffsetX = 1, cellOffsetY = 1, FUN = mean)
aggvar2 <- aggr_grid(data = data, x = "x", y = "y", t = "t", var = "var2",
                      cellSize = 4, cellOffsetX = 1, cellOffsetY = 1, FUN = median)
aggmerged <- dplyr::left_join(aggvar1, aggvar2, by = c("x", "y", "t"))

# References
# Hadley Wickham, Romain Francois, Lionel Henry and Kirill Muller (2020).
# dplyr: A Grammar of Data Manipulation. R package version 0.8.5.
# https://CRAN.R-project.org/package=dplyr.
}

}
