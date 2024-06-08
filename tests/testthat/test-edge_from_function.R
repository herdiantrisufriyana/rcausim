library(testthat)
library(rcausim)

paste0(
    "'edge_from_function' handles input with a class beyond 'Functions'."
  ) |>
  test_that({
    functions <-
      list(
        B = "n"
        ,A = "B"
      )

    expect_error(edge_from_function(functions))
  })

paste0(
    "'edge_from_function' handles Functions consisting entirely of characters, "
    ,"entirely of functions, or a mix of both characters and functions."
  ) |>
  test_that({
    data(edges)

    functions <- function_from_edge(edges)

    expect_equal(edge_from_function(functions), edges)

    function_B <- function(n){
      B <- rnorm(n, 0, 1)
      return(B)
    }

    functions <-
      functions |>
      define("B", function_B)

    expect_equal(edge_from_function(functions), edges)

    data(functions)

    expect_equal(edge_from_function(functions), edges)
  })








