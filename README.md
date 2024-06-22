# rcausim: An R package to generate causally-simulated data

`rcausim` is an R package designed to generate causally-simulated data to serve as ground truth for evaluating methods in causal discovery and effect estimation. This is particularly useful for researchers in fields such as artificial intelligence, statistics, biology, medicine, epidemiology, economics, and social sciences, who are developing a general or a domain-specific methods to discover causal structures and estimate causal effects.


## Features

- **Define Functions and Edges**: Set up functions based on specified edges and conversely, set up edges based on functions.

- **Data Simulation**: Generate data according to predefined functions and network structures, adhering to principles of structural causal modeling.


## Installation

You can install the development version of `rcausim` from GitHub with:

```r
# install.packages("devtools")
devtools::install_github("herdiantrisufriyana/rcausim")
```


## Quick Start

### Define Functions and Edges

#### Defining Causal Structure

Start by defining the causal structure as a data frame of edges:

```r
library(rcausim)
```

```r
# Load predefined edge data
data(edges)
print(edges)
```

Assist in setting up functions based on these edges:

```r
# Generate function setups from edge definitions
functions <- function_from_edge(edges)
print(functions)
```

Define specific functions:

```r
# Define a function for vertex B
function_B <- function(n){ rnorm(n, mean = 90, sd = 5) }
functions <- define(functions, 'B', function_B)
print(functions)
```

#### Alternatively, Start by Defining Functions

You can also start by defining functions directly:

```r
# Define a function for vertex B
function_B <- function(n){ rnorm(n, mean = 90, sd = 5) }

# Define a function for vertex A
function_A <- function(B){ ifelse(B>=95, 1, 0) }

# Combine functions in a list
functions <- list(A = function_A, B = function_B)
functions <- function_from_user(functions)
```

Ensure the causal structure is a directed acyclic graph (DAG):

```r
library(igraph)
```

```r
# Set up edges based on functions
edges <- edge_from_function(functions)

# Check if the resulting edges form a DAG
g <- graph_from_data_frame(edges, directed = TRUE)
is_dag(g)
```

### Data Simulation

Generate simulated data based on the predefined functions:

```r
# Assume completed functions setup
data(functions)

# Generate simulated data
set.seed(1)
simulated_data <- data_from_function(functions, n = 100)
print(simulated_data)
```


## Vignettes

Explore detailed examples and methodologies in the following vignettes:

- [**Quick Start**](https://htmlpreview.github.io/?https://github.com/herdiantrisufriyana/rcausim/blob/master/doc/quick_start.html): Get started quickly with the `rcausim` package. This vignette offers a practical introduction to the essential features, providing a quick glimpse into how you can effectively use the package to generate causally-simulated data.

- [**Causal Simulation Exemplar**](https://htmlpreview.github.io/?https://github.com/herdiantrisufriyana/rcausim/blob/master/doc/causal_simulation_exemplar.html): A guide through basic causal simulation scenarios demonstrating how to use `rcausim` to set up and simulate data.

- [**Reference Manual**](https://github.com/herdiantrisufriyana/rcausim/blob/master/extras/rcausim_0.1.1.pdf): Comprehensive documentation of all functions and features available in `rcausim`. Ideal for detailed reference and advanced use cases.


## License

`rcausim` is licensed under the GNU General Public License v3.0 (GPL-3), which ensures that all derivatives of the software are free to use under the same terms. See the LICENSE file for more details.


# Citation

If you use `rcausim` in your research, please consider citing it:

```bibtex
@misc{rcausim2024,
  author = {Herdiantri Sufriyana and Emily Chia-Yu Su},
  title = {rcausim: An R package to generate causally-simulated data},
  year = {2024},
  publisher = {GitHub},
  journal = {GitHub repository},
  howpublished = {\\url{https://github.com/herdiantrisufriyana/rcausim}}
}
```


## Contact

For questions or support, please contact herdi[at]tmu.edu.tw.
