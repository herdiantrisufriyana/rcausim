#' List functions from user
#'
#' @param func Functions, a list of functions which are defined by a user. The
#' list must be non-empty. All elements of the list must be named. All elements
#' of the list must be functions. The list must construct 1 edge or more.
#'
#' @return A list of functions. It can be an input for generating the simulated
#' data, or redefined by a user using \code{\link{define}} function.
#'
#' @keywords user-specified-function-definition
#'
#' @export
#'
#' @importFrom magrittr %>%
#'
#' @examples
#'
#' function_B <- function(n){ rnorm(n, mean = 90, sd = 5) }
#' function_A <- function(B){ ifelse(B>=95, 1, 0) }
#' functions <- list(A = function_A, B = function_B)
#' functions <- function_from_user(functions)

function_from_user=function(func){
  # Check 'func' is a list
  if(!is.list(func)){
    stop(
      '\n'
      ,'The \'func\' argument must be a list. Please ensure that the provided\n'
      ,'argument is a list before proceeding.'
    )
  }

  # Check if 'func' is non-empty
  if(length(func)==0){
    stop(
      '\n'
      ,'The \'func\' argument must be a non-empty list. Please provide a non-\n'
      ,'empty list to proceed.'
    )
  }

  # Check if all elements of 'func' are named.
  func_names=
    func %>%
    names()

  func_names=
    func_names[func_names!='']

  if(length(func_names)!=length(func)){
    stop(
      '\n'
      ,'All elements of the list must be named. Please ensure each element\n'
      ,'has a unique name assigned using the syntax \'list(name1 = value1,\n'
      ,'name2 = value2, ...)\'.'
    )
  }

  # Check if all elements of 'func' are functions
  if(!all(sapply(func,is.function))){
    stop(
      '\n'
      ,'All elements of the list must be functions. Please ensure that each\n'
      ,'element in the list is a valid function object.'
    )
  }

  # List arguments in each function
  arg=
    func %>%
    lapply(formals) %>%
    lapply(names)

  # Filter arguments of functions from non-terminal vertices
  v_nonterm_arg=
    arg[sapply(arg,\(x)length(x[x!='n'])>0)]

  #  Check if at least one edge exists
  if(length(v_nonterm_arg)==0){
    stop(
      paste0(
        '\n'
        ,'The argument \'func\' must construct 1 edge or more.'
      )
    )
  }

  # Assign a class for the concatenated list
  class(func)=
    'Functions'

  # Return concatenated list
  func
}
