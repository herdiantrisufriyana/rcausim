## ----echo=TRUE, results='hide', message=FALSE, warning=FALSE------------------
library(rcausim)

## ----include=FALSE------------------------------------------------------------
library(knitr)
library(kableExtra)

## -----------------------------------------------------------------------------
# Load predefined edge data
data(edges)

## ----echo=FALSE---------------------------------------------------------------
edges %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
# Generate function setups from edge definitions
functions <- function_from_edge(edges)
print(functions)

## -----------------------------------------------------------------------------
# Define a function for vertex B
function_B <- function(n){ rnorm(n, mean = 90, sd = 5) }
functions <- define(functions, 'B', function_B)
print(functions)

## -----------------------------------------------------------------------------
# Define a function for vertex B
function_B <- function(n){ rnorm(n, mean = 90, sd = 5) }

# Define a function for vertex A
function_A <- function(B){ ifelse(B>=95, 1, 0) }

# Combine functions in a list
functions <- list(A = function_A, B = function_B)
functions <- function_from_user(functions)

## ----echo=TRUE, results='hide', message=FALSE, warning=FALSE------------------
library(igraph)

## -----------------------------------------------------------------------------
# Set up edges based on functions
edges <- edge_from_function(functions)

# Check if the resulting edges form a DAG
g <- graph_from_data_frame(edges, directed = TRUE)
is_dag(g)

## -----------------------------------------------------------------------------
# Assume completed functions setup
data(functions)

# Generate simulated data
set.seed(1)
simulated_data <- data_from_function(functions, n = 100)

## ----echo=FALSE---------------------------------------------------------------
simulated_data %>%
  kable() %>%
  kable_classic()

