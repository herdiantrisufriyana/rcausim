#' Functions
#'
#' An example of an object class generated by \code{function_from_edge}
#' or \code{function_from_user} functions. The causal structure is a directed
#' acyclic graph (DAG), which means no loops are allowed. A function in the list
#' include 'n' as the only argument. All arguments within any function are
#' defined by their respective functions, except the argument 'n'. The output
#' lengths of vertex functions match the specified length 'n'.
#'
#' @format A list with 5 elements:
#' \describe{
#'   \item{B}{A function with an argument 'n'.}
#'   \item{A}{A function with an argument 'B'.}
#'   \item{D}{A function with an argument 'A'.}
#'   \item{C}{A function with arguments 'A', 'B', and 'D'.}
#'   \item{E}{A function with arguments 'A' and 'C'.}
#' }
#'
#' @source Generated for examples in this package.
#' @keywords dataset
#' @name functions
'functions'
