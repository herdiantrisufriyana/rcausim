library(testthat)
library(rcausim)
library(dplyr)

paste0(
    "'data_from_function' handles 'func' with a class beyond 'Functions'."
  ) |>
  test_that({
    functions <-
      list(
        B = "n"
        ,A = "B"
      )

    expect_error(data_from_function(functions, n = 100))
  })

paste0(
    "'data_from_function' handles 'func' with a vertex has not been defined for "
    ,"its function."
  ) |>
  test_that({
    data(edges)
    data(functions)

    functions2 <-
      edges |>
      function_from_edge()

    for(v in names(functions)[names(functions) != "C"]){
      functions2 <-
        functions2 |>
        define(v, functions[[v]])
    }

    expect_error(data_from_function(functions2, n = 100))
  })

paste0(
    "'data_from_function' handles non-numeric 'n'."
  ) |>
  test_that({
    data(functions)

    expect_error(data_from_function(functions, n = "100"))
  })

paste0(
    "'data_from_function' handles numeric 'n' of a length not equal to 1."
  ) |>
  test_that({
    data(functions)

    expect_error(data_from_function(functions, n = c(100, 100)))
    expect_error(data_from_function(functions, n = numeric()))
  })

paste0(
    "'data_from_function' handles negative, zero, or decimal numeric 'n'."
  ) |>
  test_that({
    data(functions)

    expect_error(data_from_function(functions, n = -100))
    expect_error(data_from_function(functions, n = 0))
    expect_error(data_from_function(functions, n = 0.1))
  })

paste0(
    "'data_from_function' handles non-directed acyclic graph (DAG) 'func'."
  ) |>
  test_that({
    edges <-
      data.frame(
        from = c("X", 'X')
        ,to = c("Y", "X")
      )

    functions <-
      edges |>
      function_from_edge()

    function_X <- function(X){
      X <- 0.3 * X + rnorm(length(X), 0, 1)
      return(X)
    }

    function_Y <- function(X){
      Y <- 0.3 * X + 0.001 * Y + rnorm(length(X), 0.1, 0.001)
      return(Y)
    }

    functions <- define(functions, "X", function_X)
    functions <- define(functions, "Y", function_Y)

    expect_error(data_from_function(functions, n = 100))
  })

paste0(
    "'data_from_function' handles 'func' without the argument 'n' in any "
    ,"functions."
  ) |>
  test_that({
    data(functions)
    functions <- functions[names(functions) != "B"]

    expect_error(data_from_function(functions, n = 100))
  })

paste0(
    "'data_from_function' handles 'func' with the argument 'n', but it is not "
    ,"the only argument in any functions."
  ) |>
  test_that({
    data(edges)

    edges <-
      edges |>
      add_row(data.frame(from = "X", to = "Y"))

    data(functions)

    functions2 <-
      edges |>
      function_from_edge()

    for(v in names(functions)){
      functions2 <-
        functions2 |>
        define(v, functions[[v]])
    }

    function_X <- function(n){
      X <- rnorm(n, 0, 1)
      return(X)
    }

    function_Y <- function(X){
      Y <- 0.7 * X + rnorm(length(X), 0.1, 0.001)
      return(Y)
    }

    functions2 <-
      functions2 |>
      define("X", function_X) |>
      define("Y", function_Y)

    function_X <- function(n, E){
      X <- 0.3 * E + rnorm(n, 0, 1)
      return(X)
    }

    functions2 <- define(functions2, "X", function_X)

    expect_error(data_from_function(functions2, n = 100))
  })

paste0(
    "'data_from_function' handles 'func' where an argument in any functions "
    ,"does not have its own function, except when the argument is 'n'."
  ) |>
  test_that({
    functions <-
      list(
        B = function(n){
          B <- rnorm(n, 0, 1)
          return(B)
        }
        ,X = function(n){
          B <- rnorm(n, 0, 1)
          return(B)
        }
        ,A = function(B){
          A <- 0.3 * B + 0.1
          return(A)
        }
      ) |>
      function_from_user()

    function_B <- function(E){
      B <- 0.1 * G + 0.7
      return(B)
    }

    functions <- define(functions, "B", function_B)

    expect_error(data_from_function(functions, n = 100))
  })

paste0(
    "'data_from_function' handles 'func' where the function of a terminal "
    ,"vertex has an output length that does not match the specified length "
    ,"'n'."
  ) |>
  test_that({
    data(functions)

    function_B <- function(n){
      B <- rnorm(10, 0, 1)
      return(B)
    }

    functions <- define(functions, "B", function_B)

    expect_error(data_from_function(functions, n = 100))
  })

paste0(
    "'data_from_function' handles 'func' where the function of a non-terminal "
    ,"vertex has an output length that does not match the specified length "
    ,"'n'."
  ) |>
  test_that({
    data(functions)

    function_E <- function(A, C){
      E <- A[1:2] + C[1:2]
      return(E)
    }

    functions <- define(functions, "E", function_E)

    expect_error(data_from_function(functions, n = 100))
  })

paste0(
    "'data_from_function' handles 'func' where the derived graph have >1 roots."
  ) |>
  test_that({
    data(edges)

    edges <-
      edges |>
      add_row(data.frame(from = "X", to = "Y")) |>
      add_row(data.frame(from = "B", to = "Y"))

    data(functions)

    functions2 <-
      edges |>
      function_from_edge()

    for(v in names(functions)){
      functions2 <-
        functions2 |>
        define(v, functions[[v]])
    }

    function_X <- function(n){
      X <- rnorm(n, 0, 1)
      return(X)
    }

    function_Y <- function(X, B){
      Y <- X + B
      return(Y)
    }

    functions2 <- define(functions2, "X", function_X)
    functions2 <- define(functions2, "Y", function_Y)

    expect_no_condition(data_from_function(functions2, n = 100))
  })

paste0(
    "'data_from_function' handles 'func' where the classes of the generated "
    ,"data can be any classes."
  ) |>
  test_that({
    data(edges)

    edges <-
      edges |>
      add_row(data.frame(from = "E", to = "X")) |>
      add_row(data.frame(from = "E", to = "Y")) |>
      add_row(data.frame(from = "E", to = "Z"))

    data(functions)

    functions2 <-
      edges |>
      function_from_edge()

    for(v in names(functions)){
      functions2 <-
        functions2 |>
        define(v, functions[[v]])
    }

    function_X <- function(E){
      X <- sample(c(TRUE, FALSE), size = length(E), replace = TRUE)
      return(X)
    }

    function_Y <- function(E){
      Y <- sample(LETTERS[1:2], size = length(E), replace = TRUE)
      return(Y)
    }

    function_Z <- function(E){
      Z <- sample(factor(LETTERS[1:2]), size = length(E), replace = TRUE)
      return(Z)
    }

    functions2 <-
      functions2 |>
      define("X", function_X) |>
      define("Y", function_Y) |>
      define("Z", function_Z)

    expect_no_condition(data_from_function(functions2, n = 100))
  })
