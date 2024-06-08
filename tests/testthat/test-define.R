library(testthat)
library(rcausim)

paste0(
    "'define' handles input with a class beyond 'Functions'."
  ) |>
  test_that({
    functions <-
      list(
        B = "n"
        ,A = "B"
      )

    function_B <- function(n){
      B <- rnorm(n, 0, 1)
      return(B)
    }

    expect_error(define(functions, "B", function_B))
  })

paste0(
    "'define' handles non-character input or character input with a length >1 "
    ,"for the argument 'which'."
  ) |>
  test_that({
    data(functions)

    function_B <- function(n){
      B <- rnorm(n, 0, 1)
      return(B)
    }

    expect_error(define(functions, 1, function_B))
    expect_error(define(functions, c("B", "A"), function_B))
  })

paste0(
    "'define' handles non-function input for the argument 'what'."
  ) |>
  test_that({
    data(functions)

    expect_error(define(functions, "B", "rnorm(n, 0, 1)"))
  })

paste0(
    "'define' handles 'which' that is unspecified in 'Functions'."
  ) |>
  test_that({
    data(functions)

    function_X <- function(n){
      X <- rnorm(n, 0, 1)
      return(X)
    }

    expect_error(define(functions, "X", function_X))
  })

paste0(
    "'define' handles 'what' with arguments unspecified in 'Functions', if not "
    ,"previously defined."
  ) |>
  test_that({
    data(edges)

    functions <-
      edges |>
      function_from_edge()

    function_B <- function(A){
      B <- 0.3 * A + rnorm(length(X), 0, 1)
      return(B)
    }

    expect_error(define(functions, "B", function_B))
  })

paste0(
    "'define' handles Functions as a list of either characters or functions."
  ) |>
  test_that({
    data(edges)

    functions <-
      edges |>
      function_from_edge()

    function_B <- function(n){
      B <- rnorm(n, 0, 1)
      return(B)
    }

    functions <-
      functions |>
      define("B", function_B)

    class_in_functions <-
      functions |>
      sapply(class) |>
      unique() |>
      sort()

    expect_equal(class_in_functions, c("character", "function"))
  })
