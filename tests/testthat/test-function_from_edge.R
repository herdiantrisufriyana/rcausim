library(testthat)
library(rcausim)

paste0(
    "'function_from_edge' handles non-data-frame input."
  ) |>
  test_that({
    edges <-
      list(
        B = "n"
        ,A = "B"
      )

    expect_error(function_from_edge(edges))
  })

paste0(
    "'function_from_edge' handles data-frame input beyond that with 'from' and "
    ,"'to' as the first and second columns."
  ) |>
  test_that({
    edges <-
      data.frame(
        from = "X"
        ,v2 = "Y"
      )

    expect_error(function_from_edge(edges))

    edges <-
      data.frame(
        v1 = "X"
        ,to = "Y"
      )

    expect_error(function_from_edge(edges))

    edges <-
      data.frame(
        to = "X"
        ,from = "Y"
      )

    expect_error(function_from_edge(edges))

    edges <-
      data.frame(
        v1 = "X"
        ,v2 = "Y"
      )

    expect_error(function_from_edge(edges))
  })

paste0(
    "function_from_edge' handles input with a vertex named 'n'."
  ) |>
  test_that({
    edges <-
      data.frame(
        from = "n"
        ,to = "X"
      )

    expect_error(function_from_edge(edges))
  })

paste0(
    "The number of functions in the output is equal to the number of vertices."
  ) |>
  test_that({
    data(edges)

    n_vertices <-
      edges |>
      gather(key = "position", value = "v") |>
      pull("v") |>
      unique() |>
      length()

    functions <-
      edges |>
      function_from_edge()

    n_functions <-
      functions |>
      length()

    expect_equal(n_functions, n_vertices)
  })

paste0(
    "The functions of terminal vertices consist of only an argument 'n'."
  ) |>
  test_that({
    data(edges)

    from_var <- "from"
    to_var <- "to"

    v_term <-
      edges |>
      filter(!get(from_var) %in% get(to_var)) |>
      pull("from") |>
      unique()

    functions <-
      edges |>
      function_from_edge()

    arguments <-
      functions[v_term] |>
      unlist() |>
      unique()

    expect_equal(arguments, "n")
  })

paste0(
    "The functions of non-terminal vertices consist of arguments that include "
    ,"all of their children."
  ) |>
  test_that({
    data(edges)

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

    functions <-
      edges |>
      function_from_edge()

    arguments <-
      functions[v_nonterm] |>
      unlist() |>
      unique() |>
      sort()

    expect_equal(arguments, v_nonterm_children)
  })
