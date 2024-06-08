library(testthat)
library(rcausim)
library(dplyr)

paste0(
    "'time_varying' handles 'func' with a class beyond 'Functions'."
  ) |>
  test_that({
    functions0 <-
      list(
        B = function(n){
          B <- rnorm(n, 0, 1)
          return(B)
        }
        ,A = function(B){
          A <- 0.3 * B + rnorm(length(B), 0.1, 0.001)
        }
      )

    functions <- function_from_user(functions0)

    set.seed(1)
    simulated_data <- data_from_function(functions, n = 100)
    T_max <- rpois(nrow(simulated_data), lambda = 25)

    expect_error(time_varying(functions0, simulated_data, T_max))
  })

paste0(
    "'time_varying' handles 'func' with a vertex has not been defined for its "
    ,"function."
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

    set.seed(1)
    simulated_data <- data_from_function(functions, n = 100)
    T_max <- rpois(nrow(simulated_data), lambda = 25)

    expect_error(time_varying(functions2, simulated_data, T_max))
  })

paste0(
    "'time_varying' handles non-data-frame 'data'."
  ) |>
  test_that({
    data(functions)

    T_max <- rpois(100, lambda = 25)

    expect_error(time_varying(functions, "simulated_data", T_max))
  })

paste0(
    "'time_varying' handles non-numeric 'T_max'."
  ) |>
  test_that({
    data(functions)

    set.seed(1)
    simulated_data <- data_from_function(functions, n = 100)

    expect_error(time_varying(functions, simulated_data, "T_max"))
  })

paste0(
    "'time_varying' handles a numeric vector 'T_max' of a length not "
    ,"equal to 1."
  ) |>
  test_that({
    data(functions)

    set.seed(1)
    simulated_data <- data_from_function(functions, n = 100)
    T_max <- rpois(nrow(simulated_data), lambda = 25)

    expect_error(time_varying(functions, simulated_data, T_max[1]))
    expect_error(time_varying(functions, simulated_data, T_max[-1]))
  })

paste0(
    "'time_varying' handles negative or decimal numeric 'T_max'."
  ) |>
  test_that({
    data(functions)

    set.seed(1)
    simulated_data <- data_from_function(functions, n = 100)
    T_max <- rpois(nrow(simulated_data), lambda = 25)

    T_max[1] <- -1
    expect_error(time_varying(functions, simulated_data, T_max))

    T_max[1] <- 0.1
    expect_error(time_varying(functions, simulated_data, T_max))
  })

paste0(
    "'time_varying' handles non-directed cyclic graph (DCG) 'func'."
  ) |>
  test_that({
    data(functions)

    set.seed(1)
    simulated_data <- data_from_function(functions, n = 100)
    T_max <- rpois(nrow(simulated_data), lambda = 25)

    expect_error(time_varying(functions, simulated_data, T_max))
  })

paste0(
    "'time_varying' handles 'func' with the argument 'n', but it is not "
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

    function_X <- function(E){
      X <- 0.3 * E + rnorm(length(E), 0, 1)
      return(X)
    }

    functions2 <- define(functions2, "X", function_X)

    set.seed(1)
    simulated_data <- data_from_function(functions2, n = 100)
    T_max <- rpois(nrow(simulated_data), lambda = 25)

    function_X <- function(n, Y){
      X <- 0.3 * E + rnorm(n, 0, 1)
      return(X)
    }

    functions2 <- define(functions2, "X", function_X)

    expect_error(time_varying(functions2, simulated_data, T_max))
  })

paste0(
    "'time_varying' handles 'func' where an argument in any functions does not "
    ,"have its own function, except when the argument is 'n'."
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

    set.seed(1)
    simulated_data <- data_from_function(functions, n = 100)
    T_max <- rpois(nrow(simulated_data), lambda = 25)

    function_B <- function(A, E){
      B <- 0.1 * G + 0.7
      return(B)
    }

    functions <- define(functions, "B", function_B)

    expect_error(time_varying(functions, simulated_data, T_max))
  })

paste0(
    "'time_varying' handles 'data' that includes 'i', 't', or 't_max'."
  ) |>
  test_that({
    data(functions)
    set.seed(1)
    simulated_data <- data_from_function(functions, n = 100)

    function_B <- function(B){
      B <- B + 1
      return(B)
    }

    set.seed(1)
    functions <- define(functions, "B", function_B)
    T_max <- rpois(nrow(simulated_data), lambda = 25)

    simulated_data2 <-
      simulated_data |>
      mutate(i = seq(n()))

    expect_error(time_varying(functions, simulated_data2, T_max))

    simulated_data2 <-
      simulated_data |>
      mutate(t = seq(n()))

    expect_error(time_varying(functions, simulated_data2, T_max))

    simulated_data2 <-
      simulated_data |>
      mutate(t_max = T_max)

    expect_error(time_varying(functions, simulated_data2, T_max))
  })

paste0(
    "'time_varying' handles 'data' that does not include all arguments, except "
    ,"n."
  ) |>
  test_that({
    data(functions)
    set.seed(1)
    simulated_data <- data_from_function(functions, n = 100)

    function_B <- function(B){
      B <- B + 1
      return(B)
    }

    functions <- define(functions, "B", function_B)
    set.seed(1)
    T_max <- rpois(nrow(simulated_data), lambda = 25)

    simulated_data <- simulated_data[-2]

    expect_error(time_varying(functions, simulated_data, T_max))
  })

paste0(
    "'time_varying' handles 'func' where the function of a vertex has an "
    ,"output length that does not match the input length."
  ) |>
  test_that({
    data(functions)
    set.seed(1)
    simulated_data <- data_from_function(functions, n = 100)

    function_B <- function(B){
      B <- B[1:2] + 1
      return(B)
    }

    functions <- define(functions, "B", function_B)
    set.seed(1)
    T_max <- rpois(nrow(simulated_data), lambda = 25)

    expect_error(time_varying(functions, simulated_data, T_max))
  })

paste0(
    "'time_varying' handles 'func' where the derived graph have >1 roots."
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

    set.seed(1)
    simulated_data <- data_from_function(functions2, n = 100)

    function_B <- function(B){
      B <- B + 1
      return(B)
    }

    functions <- define(functions, "B", function_B)
    set.seed(1)
    T_max <- rpois(nrow(simulated_data), lambda = 25)

    expect_no_condition(time_varying(functions, simulated_data, T_max))
  })

paste0(
    "'time_varying' handles 'func' where the classes of the generated data can "
    ,"be any classes."
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

    set.seed(1)
    simulated_data <- data_from_function(functions2, n = 100)

    function_B <- function(B){
      B <- B + 1
      return(B)
    }

    functions2 <- define(functions2, "B", function_B)
    set.seed(1)
    T_max <- rpois(nrow(simulated_data), lambda = 25)

    expect_no_condition(time_varying(functions2, simulated_data, T_max))
  })
