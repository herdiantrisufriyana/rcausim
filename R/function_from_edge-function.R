#' List functions given edges
#'
#' @param e Edge, a data frame that must only include the columns 'from'
#' and 'to in this order. A vertex name 'n' is not allowed.
#'
#' @return A list of character vectors of arguments for function which will be
#' defined by a user using \code{define} function.
#'
#' @keywords edge, function
#'
#' @export
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr select pull filter
#' @importFrom tidyr gather
#'
#' @examples
#'
#' data(edges)
#' function_from_edge(edges)

function_from_edge=function(e){
  # Check if 'e' is a data.frame
  if(!is.data.frame(e)){
    stop(
      paste0(
        '\n'
        ,'The argument \'e\' must be a data frame. Please ensure that the\n'
        ,'provided object is valid data frame and try again.'
      )
    )
  }

  # Check if the data frame has column names as orderly specified
  if(!all(colnames(e)[1]=='from',colnames(e)[2]=='to')){
    stop(
      paste0(
        '\n'
        ,'The data frame must only include the columns \'from\' and \'to\' in\n'
        ,'this order. Please adjust your data frame accordingly.'
      )
    )
  }

  # List vertices
  v=e %>%
    select(from,to) %>%
    gather(position,v) %>%
    pull(v) %>%
    .[!duplicated(.)]

  # Check if a vertex is named 'n'
  if('n'%in%v){
    stop(
      paste0(
        '\n'
        ,'A vertex name \'n\' is not allowed. Please choose a different name\n'
        ,'for the vertex.'
      )
    )
  }

  # Identify terminal vertices
  v_term=
    e %>%
    filter(!from%in%to) %>%
    pull(from) %>%
    .[!duplicated(.)]

  # Define n the only argument of non-terminal vertices
  v_term_arg=
    v_term %>%
    `names<-`(as.character(.)) %>%
    lapply(\(x)'n')

  # Identify non-terminal vertices
  v_nonterm=
    v %>%
    .[!.%in%v_term]

  # Identify arguments to generate non-terminal vertices
  v_nonterm_arg=
    v_nonterm %>%
    `names<-`(as.character(.)) %>%
    lapply(\(x)
           e %>%
             filter(to==x) %>%
             pull(from)
    )

  # Concatenate lists of arguments from terminal and non-terminal vertices
  func=
    v_term_arg %>%
    c(v_nonterm_arg)

  # Assign a class for the concatenated list
  class(func)=
    'Functions'

  # Return concatenated list
  func
}
