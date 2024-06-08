library(testthat)
library(rcausim)

paste0(
    "'function_from_user' handles non-list input."
  ) |>
  test_that({
    functions <-
      data.frame(
        B = "function(n)"
        ,A = "function(B)"
      )

    expect_error(function_from_user(functions))
  })

paste0(
    "'function_from_user' handles empty-list input."
  ) |>
  test_that({
    functions <- list()

    expect_error(function_from_user(functions))
  })

paste0(
    "'function_from_user' handles input with an unnamed element."
  ) |>
  test_that({
    function_B <- function(n){
      B <- rnorm(n, 0, 1)
      return(B)
    }

    functions <- list(function_B)

    expect_error(function_from_user(functions))
  })

paste0(
    "'function_from_user' handles input with a non-function element."
  ) |>
  test_that({
    function_B <- function(n){
      B <- rnorm(n, 0, 1)
      return(B)
    }

    functions <- list(B = function_B, A = "B")

    expect_error(function_from_user(functions))
  })

paste0(
    "'function_from_user' handles input that constructs no edges."
  ) |>
  test_that({
    functions <-
      list(
        B = function(n){
          B <- rnorm(n, 0, 1)
          return(B)
        }
      )

    expect_error(function_from_user(functions))
  })

paste0(
    "The functions of terminal vertices consist of only an argument 'n'."
  ) |>
  test_that({
    functions <-
      list(
        B = function(n){
          B <- rnorm(n, 0, 1)
          return(B)
        }
        ,A = function(B){
          A <- 0.3 * B + 0.1
          return(A)
        }
      ) |>
      function_from_user()

    edges <-
      functions |>
      edge_from_function()

    from_var <- "from"
    to_var <- "to"

    v_term <-
      edges |>
      filter(!get(from_var) %in% get(to_var)) |>
      pull("from") |>
      unique()

    arguments <-
      functions[v_term] |>
      lapply(formals) |>
      lapply(names) |>
      unlist() |>
      unique()

    expect_equal(arguments, "n")
  })

paste0(
    "The functions of non-terminal vertices consist of arguments that include "
    ,"all of their children."
  ) |>
  test_that({
    functions <-
      list(
        B = function(n){
          B <- rnorm(n, 0, 1)
          return(B)
        }
        ,A = function(B){
          A <- 0.3 * B + 0.1
          return(A)
        }
        ,D = function(A){
          D <- 0.7 * A + 0.3
          return(D)
        }
      ) |>
      function_from_user()

    edges <-
      functions |>
      edge_from_function()

    v <-
      edges |>
      gather(key = "position", value = "v") |>
      pull("v") |>
      unique()

    from_var <- "from"
    to_var <- "to"

    v_term <-
      edges |>
      filter(!get(from_var) %in% get(to_var)) |>
      pull("from") |>
      unique()

    v_nonterm <-
      v[!v%in%v_term]

    get_children <- function(v, edges){
      edges |>
        filter(get(to_var) == v) |>
        pull("from")
    }

    v_nonterm_children <-
      v_nonterm |>
      lapply(get_children, edges) |>
      unlist() |>
      unique() |>
      sort()

    arguments <-
      functions[v_nonterm] |>
      lapply(formals) |>
      lapply(names) |>
      unlist() |>
      unique() |>
      sort()

    expect_equal(arguments, v_nonterm_children)
  })

