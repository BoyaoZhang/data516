#' @title Data preprocessing
#'
#' @description Data preprocessing for factor, numeric, character and missing values.
#'
#' @param data [\code{character(1)}]\cr
#'   Give the data set to be preprocessing.
#' @param factor [\code{factor(1)}]\cr
#'   A vector of variable names which should be as factor.
#'   Default is \code{NULL}.
#' @param numeric [\code{factor(1)}]\cr
#'   A vector of variable names which should be as numeric.
#'   Default is \code{NULL}.
#' @param character [\code{factor(1)}]\cr
#'   A vector of variable names which should be as character.
#'   Default is \code{NULL}.
#' @param omit.missing [\code{logical(1)}]\cr
#'   Omit the missings values?
#'   Default is \code{FALSE}.
#'
#' @importFrom stats complete.cases
#' @export


preprocess = function(data,
                      factor = NULL,
                      numeric = NULL,
                      character = NULL,
                      omit.missing = FALSE) {
  output = data

  if (!is.null(factor))
    for (i in factor)
      output[, i] = as.factor(output[, i])

  if (!is.numeric(numeric))
    for (i in numeric)
      output[, i] = as.numeric(output[, i])

  if (!is.character(character))
    for (i in character)
      output[, i] = as.character(output[, i])

  if (omit.missing)
    output = output[!complete.cases(output),]
  return(output)
}
