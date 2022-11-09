#' A Function to Aggregate Grid Data to a Different Cell Size
#'
#' This function aggregates grid data to a different cell size.
#'
#' This function aggregates grid data to a different cell size. It works both
#' for cross-sectional data and for cross-sectional time-series data.
#'
#' It can handle only one variable to be aggregated each time and returns a
#' data frame containing the x and y coordinate, the time variable if any,
#' and the aggregated variable. If there are more than one variable to be
#' aggregated, apply the function to every one of the variables separately
#' with the same parameter values for \code{x}, \code{y}, \code{t} (if used),
#' \code{cellSize}, \code{cellOffsetX}, and \code{cellOffsetY}, while changing
#' inputs for \code{var} and \code{FUN} (if necessary). Then, combine all
#' separate data frames into one data frame using the x and y coordinates
#' (and the time variable if any). See the example below.
#'
#' By default, \code{FUN} is applied over the non-missing values of all the
#' grid cells to be aggregated to a larger one (i.e., \code{ignrNA = TRUE}).
#' If \code{ignrNA = TRUE} and a variable has missing values for all the grid
#' cells to be aggregated to a larger one, the returned data frame will
#' not contain this larger cell. If \code{ignrNA = FALSE}, the returned data
#' frame will assign a missing value to the larger cell, regardless of a
#' variable having the missing value for only some or all of the grid cells
#' to be aggregated to a larger one.
#'
#' For a theoretical rationale and an empirical example, please see the
#' following paper:
#'
#' Suzuki, Akisato. 2022. "Uncertainty in Grid Data: A Theory and Comprehensive
#' Robustness Test." Quality & Quantity. https://doi.org/10.1007/s11135-022-01555-x.
#'
#' If you use this package, please cite the following items:
#'
#' Suzuki, Akisato. 2022. "Uncertainty in Grid Data: A Theory and Comprehensive
#' Robustness Test." Quality & Quantity. https://doi.org/10.1007/s11135-022-01555-x.
#'
#' Dornschneider-Elkink, Johan A., and Akisato Suzuki. 2022. "rbstgrid: An R
#' Package to Aggregate Grid Data to a Different Cell Size." R package version 1.0.0. https://github.com/AkisatoSuzuki/rbstgrid.
#'
#' @param data A data frame
#' @param x Characters for the x coordinate variable name
#' @param y Characters for the y coordinate variable name
#' @param t Characters for the time variable name; default = NULL
#' @param var Characters for the name of the variable whose aggregated value to be computed
#' @param cellSize A natural number indicating the size by which a cell is multiplied; default = 2
#' @param cellOffsetX A natural number (or zero) indicating how much the starting cell of aggregation is offset along the x coordinate; default = 0
#' @param cellOffsetY A natural number (or zero) indicating how much the starting cell of aggregation is offset along the y coordinate; default = 0
#' @param FUN A function to apply to aggregate cells, for example, mean, median, or max
#' @param ignrNA Ignore missing values when grid cells are aggregated; default = TRUE
#' @return A data frame containing the x and y coordinate (whose colunms are named "x" and "y" respectively), the time variable if any (whose column is named "t"), and the aggregated \code{var} (which will have the same name as the original data)
#' @section Authors:
#' Author: Johan A. Dornschneider-Elkink (\email{jos.elkink@@ucd.ie})
#'
#' Author & Maintainer: Akisato Suzuki (\email{akisato.suzuki@@gmail.com})
#'
#' @examples
#' \dontrun{
#' # Aggregate grid data by doubling its cell size and
#' # computing the mean of the variable over four cells to be aggregated
#' agg <- aggr_grid(data = data, x = "x", y = "y", t = "t", var = "var", cellSize = 2, FUN = mean)
#'
#' # Aggregate grid data by tripling its cell size and
#' # computing the median of the variable over nine cells to be aggregated
#' agg <- aggr_grid(data = data, x = "x", y = "y", t = "t", var = "var", cellSize = 3, FUN = median)
#'
#' # Apply the function to several variables within the same grid data and
#' # then merge each output using the dplyr package (Wickham et al. 2020)
#' aggvar1 <- aggr_grid(data = data, x = "x", y = "y", t = "t", var = "var1",
#'                       cellSize = 4, cellOffsetX = 1, cellOffsetY = 1, FUN = mean)
#' aggvar2 <- aggr_grid(data = data, x = "x", y = "y", t = "t", var = "var2",
#'                       cellSize = 4, cellOffsetX = 1, cellOffsetY = 1, FUN = median)
#' aggmerged <- dplyr::left_join(aggvar1, aggvar2, by = c("x", "y", "t"))
#'
#' # References
#' # Hadley Wickham, Romain Francois, Lionel Henry and Kirill Muller (2020).
#' # dplyr: A Grammar of Data Manipulation. R package version 0.8.5.
#' # https://CRAN.R-project.org/package=dplyr.
#' }
#'
#' @importFrom dplyr "%>%"
#' @importFrom rlang "!!"
#'
#' @export


aggr_grid <- function(data, x, y, t = NULL, var,
                      cellSize = 2, cellOffsetX = 0, cellOffsetY = 0,
                      FUN, ignrNA = TRUE){


  if(is.null(t)==TRUE){

    # Identify incorrect inputs
    if(is.character(x)==FALSE | is.character(y)==FALSE | is.character(var)==FALSE){
      stop("The variable name must be put in as characters. Maybe forgot to use quotation marks?")
    }

    if(is.numeric(data[[x]])==FALSE){
      stop("The input for x must be a numeric vector.")
    }

    if(is.numeric(data[[y]])==FALSE){
      stop("The input for y must be a numeric vector.")
    }

    if(is.numeric(data[[var]])==FALSE){
      stop("The input for var must be a numeric vector. If it is a categorical variable,
         numeric values must be assigned to each category.")
    }

    if(is.numeric(cellSize)==FALSE | cellSize < 1 | round(cellSize)!=cellSize){
      stop("The input for cellSize must be a natural number.")
    }

    if(is.numeric(cellOffsetX)==FALSE | cellOffsetX < 0 | round(cellOffsetX)!=cellOffsetX){
      stop("The input for cellOffsetX must be zero or a natural number.")
    }

    if(cellOffsetX >= cellSize){
      stop("cellOffsetX must be smaller than cellSize.")
    }

    if(is.numeric(cellOffsetY)==FALSE | cellOffsetY < 0 | round(cellOffsetY)!=cellOffsetY){
      stop("The input for cellOffsetY must be zero or a natural number.")
    }

    if(cellOffsetY >= cellSize){
      stop("cellOffsetY must be smaller than cellSize.")
    }

    if(length(duplicated(
      data.frame(data[[x]], data[[y]])
    )[duplicated(data.frame(data[[x]], data[[y]]))==TRUE]) > 0){
      stop("There are duplicate rows in the data frame.")
    }

    if(is.logical(ignrNA) == FALSE){
      stop("The input for ignrNA must be TRUE or FALSE.")
    }

    baseGrid <- data.frame(x = data[[x]], y = data[[y]], var = data[[var]])

    baseGrid$val <- baseGrid$var

    # Create newX and newY, the coordinates that count the original x-y coordinates "cellSize" times.
    # E.g., if cellSize=2, newX assigns a value of 1 to the original x-coordinates of 1 and 2
    baseGrid <- baseGrid %>%
      dplyr::mutate(
        x = x - min(x) + cellSize,
        y = y - min(y) + cellSize
      ) %>%
      dplyr::filter(x > cellOffsetX & y > cellOffsetY) %>%
      dplyr::mutate(
        newX = floor((x - cellOffsetX) %/% cellSize),
        newY = floor((y - cellOffsetY) %/% cellSize)
      )

    varMapping <- c("newVal")
    names(varMapping) <- ls(dplyr::select(data, var))

    # Compute the aggregated value of var given newX and newY
    if(ignrNA == TRUE){
    baseGrid <- baseGrid %>%
      dplyr::filter(!is.na(val)) %>%
      dplyr::group_by(newX, newY) %>%
      dplyr::summarise(newVal = FUN(val)) %>%
      dplyr::select(x = newX, y = newY, !!varMapping)
    }

    if(ignrNA == FALSE){
      baseGrid <- baseGrid %>%
        dplyr::group_by(newX, newY) %>%
        dplyr::summarise(newVal = FUN(val)) %>%
        dplyr::select(x = newX, y = newY, !!varMapping)
    }

  }


  if(is.null(t)==FALSE){

    # Identify incorrect inputs
    if(is.character(x)==FALSE | is.character(y)==FALSE |
       is.character(t)==FALSE | is.character(var)==FALSE){
      stop("The variable names must be put in as a character. Maybe forgot to use quotation marks?")
    }

    if(is.numeric(data[[x]])==FALSE){
      stop("The input for x must be a numeric vector.")
    }

    if(is.numeric(data[[y]])==FALSE){
      stop("The input for y must be a numeric vector.")
    }

    if(is.numeric(data[[t]])==FALSE){
      stop("The input for t must be a numeric vector.")
    }

    if(is.numeric(data[[var]])==FALSE){
      stop("The input for var must be a numeric vector. If it is a categorical variable,
         numeric values must be assigned to each category.")
    }

    if(cellSize < 1 | round(cellSize)!=cellSize){
      stop("The input for cellSize must be a natural number.")
    }

    if(cellOffsetX < 0 | round(cellOffsetX)!=cellOffsetX){
      stop("The input for cellOffsetX must be zero or a natural number.")
    }

    if(cellOffsetX >= cellSize){
      stop("cellOffsetX must be a smaller integer than cellSize")
    }

    if(cellOffsetY < 0 | round(cellOffsetY)!=cellOffsetY){
      stop("The input for cellOffsetY must be zero or a natural number.")
    }

    if(cellOffsetY >= cellSize){
      stop("cellOffsetY must be a smaller integer than cellSize")
    }

    if(length(duplicated(
      data.frame(data[[x]], data[[y]], data[[t]])
      )[duplicated(data.frame(data[[x]], data[[y]], data[[t]]))==TRUE]) > 0){
      stop("There are duplicate rows in the data frame.")
    }

    if(is.logical(ignrNA) == FALSE){
      stop("The input for ignrNA must be TRUE or FALSE.")
    }

    baseGrid <- data.frame(x = data[[x]], y = data[[y]], t = data[[t]], var = data[[var]])

    baseGrid$val <- baseGrid$var

    # Create newX and newY, the coordinates that count the original x-y coordinates "cellSize" times.
    # E.g., if cellSize=2, newX assigns a value of 1 to the original x-coordinates of 1 and 2
    baseGrid <- baseGrid %>%
      dplyr::mutate(
        x = x - min(x) + cellSize,
        y = y - min(y) + cellSize
      ) %>%
      dplyr::filter(x > cellOffsetX & y > cellOffsetY) %>%
      dplyr::mutate(
        newX = floor((x - cellOffsetX) %/% cellSize),
        newY = floor((y - cellOffsetY) %/% cellSize)
      )

    varMapping <- c("newVal")
    names(varMapping) <- ls(dplyr::select(data, var))

    # Compute the aggregated value of var given newX and newY
    if(ignrNA == TRUE){
    baseGrid <- baseGrid %>%
      dplyr::filter(!is.na(val)) %>%
      dplyr::group_by(newX, newY, t) %>%
      dplyr::summarise(newVal = FUN(val)) %>%
      dplyr::select(x = newX, y = newY, t = t, !!varMapping)
    }

    if(ignrNA == FALSE){
      baseGrid <- baseGrid %>%
        dplyr::group_by(newX, newY, t) %>%
        dplyr::summarise(newVal = FUN(val)) %>%
        dplyr::select(x = newX, y = newY, t = t, !!varMapping)
    }

  }

  return(baseGrid)
}
