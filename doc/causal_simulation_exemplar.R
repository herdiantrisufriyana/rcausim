## ----echo=TRUE, results='hide', message=FALSE, warning=FALSE------------------
library(igraph)
library(ggnetwork)
library(tidyverse)

## -----------------------------------------------------------------------------
# Create a data frame for the X and Y relationship
d <- data.frame(from = "X", to = "Y")
print(d)

## -----------------------------------------------------------------------------
# Convert the data frame into an igraph object.
g <- graph_from_data_frame(d, directed = TRUE)
print(g)

## -----------------------------------------------------------------------------
# Lay out the graph as a tree
g_layout <- layout_as_tree(g)

# Determine the coordinates with ggnetwork
g_coord <- ggnetwork(g, layout = g_layout)
print(g_coord)

## -----------------------------------------------------------------------------
# Define the plot area
g_plot <- ggplot(g_coord, aes(x, y, xend = xend, yend = yend))

# Draw edges with closed, curved arrows to emphasize direction
g_plot <- g_plot + geom_edges(arrow = arrow(type = "closed"), curvature = 0.15)

# Add node labels
g_plot <- g_plot + geom_nodelabel(aes(label = name))

# Make the tree layout horizontal
g_plot <- g_plot + coord_flip()
g_plot <- g_plot + scale_y_reverse()

# Apply a minimal theme
g_plot <- g_plot + theme_void()

# Display the graph
print(g_plot)

## ----echo=FALSE---------------------------------------------------------------
print(g_plot)

## ----echo=FALSE---------------------------------------------------------------
g_coord %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_nodelabel(aes(label = name)) +
  coord_flip() +
  scale_y_reverse() +
  theme_void()

## ----echo=FALSE---------------------------------------------------------------
print(g_plot)

## ----echo=FALSE---------------------------------------------------------------
data.frame(from = "X", to = "M") %>%
  add_row(data.frame(from = "M", to = "Y")) %>%
  graph_from_data_frame(directed = TRUE) %>%
  ggnetwork(layout = layout_as_tree(.)) %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed"), curvature = 0.15) +
  geom_nodelabel(aes(label = name)) +
  coord_flip() +
  scale_y_reverse() +
  theme_void()

## ----echo=FALSE---------------------------------------------------------------
data.frame(from = "C", to = "X") %>%
  add_row(data.frame(from = "C", to = "Y")) %>%
  graph_from_data_frame(directed = TRUE)%>%
  ggnetwork(layout = layout_as_tree(.)) %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed"), curvature = 0.15) +
  geom_nodelabel(aes(label = name)) +
  theme_void()

## ----echo=FALSE---------------------------------------------------------------
data.frame(from = "X", to = "K") %>%
  add_row(data.frame(from = "Y", to = "K")) %>%
  graph_from_data_frame(directed = TRUE) %>%
  ggnetwork(layout = layout_as_tree(.)) %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed"), curvature = 0.15) +
  geom_nodelabel(aes(label = name)) +
  scale_y_reverse() +
  theme_void()

## ----echo=TRUE, results='hide', message=FALSE, warning=FALSE------------------
library(rcausim)
library(broom)

## -----------------------------------------------------------------------------
path1_functions <- function_from_edge(d)
print(path1_functions)

## -----------------------------------------------------------------------------
function_X <- function(n){
  X <- rnorm(n, mean = 0, sd = 5)
  return(X)
}

## -----------------------------------------------------------------------------
function_Y <- function(X){
  Y_num <- 0.7 * X
  Y_prob <- 1/(1 + exp(-Y_num))
  Y_cat <- ifelse(Y_prob>0.5,1,0)
  Y <- factor(Y_cat)
  return(Y)
}

## -----------------------------------------------------------------------------
path1_functions <- define(path1_functions, which = 'X', what = function_X)
path1_functions <- define(path1_functions, which = 'Y', what = function_Y)
print(path1_functions)

## -----------------------------------------------------------------------------
set.seed(1)
path1_data <- data_from_function(path1_functions, n = 50000)
head(path1_data)

## ----echo=TRUE, results='hide', message=FALSE, warning=FALSE------------------
path1_reg <- glm(Y ~ X, family = binomial(), data = path1_data)

## -----------------------------------------------------------------------------
tidy(path1_reg)

## -----------------------------------------------------------------------------
d2 <- data.frame(from = "X", to = "M")
d2 <- add_row(d2, data.frame(from = "M", to = "Y"))
print(d2)

## -----------------------------------------------------------------------------
path2_functions <- function_from_edge(d2)
print(path2_functions)

## -----------------------------------------------------------------------------
function_X <- function(n){
  X <- rnorm(n, mean = 0, sd = 5)
  return(X)
}

## -----------------------------------------------------------------------------
function_M <- function(X){
  M <- rnorm(length(X), mean = 1.5, sd = 1) * X
  return(M)
}

## -----------------------------------------------------------------------------
function_Y <- function(M){
  Y_num <- 0.7 * M
  Y_prob <- 1/(1 + exp(-Y_num))
  Y_cat <- ifelse(Y_prob>0.5,1,0)
  Y <- factor(Y_cat)
  return(Y)
}

## -----------------------------------------------------------------------------
path2_functions <- define(path2_functions, which = 'X', what = function_X)
path2_functions <- define(path2_functions, which = 'M', what = function_M)
path2_functions <- define(path2_functions, which = 'Y', what = function_Y)
print(path2_functions)

## -----------------------------------------------------------------------------
set.seed(1)
path2_data <- data_from_function(path2_functions, n = 50000)
head(path2_data)

## ----echo=TRUE, results='hide', message=FALSE, warning=FALSE------------------
path2_reg <- glm(Y ~ X, family = binomial(), data = path2_data)

## -----------------------------------------------------------------------------
tidy(path2_reg)

## -----------------------------------------------------------------------------
d3 <- data.frame(from = "C", to = "X")
d3 <- add_row(d3, data.frame(from = "C", to = "Y"))
print(d3)

## -----------------------------------------------------------------------------
path3_functions <- function_from_edge(d3)
print(path3_functions)

## -----------------------------------------------------------------------------
function_C <- function(n){
  C <- rnorm(n, mean = 0, sd = 5)
  return(C)
}

## -----------------------------------------------------------------------------
function_X <- function(C){
  X <- rnorm(length(C), mean = 1.5, sd = 1) * C
  return(X)
}

## -----------------------------------------------------------------------------
function_Y <- function(C){
  Y_num <- 0.7 * C
  Y_prob <- 1/(1 + exp(-Y_num))
  Y_cat <- ifelse(Y_prob>0.5,1,0)
  Y <- factor(Y_cat)
  return(Y)
}

## -----------------------------------------------------------------------------
path3_functions <- define(path3_functions, which = 'C', what = function_C)
path3_functions <- define(path3_functions, which = 'X', what = function_X)
path3_functions <- define(path3_functions, which = 'Y', what = function_Y)
print(path3_functions)

## -----------------------------------------------------------------------------
set.seed(1)
path3_data <- data_from_function(path3_functions, n = 50000)
head(path3_data)

## ----echo=TRUE, results='hide', message=FALSE, warning=FALSE------------------
path3_reg <- glm(Y ~ X, family = binomial(), data = path3_data)

## -----------------------------------------------------------------------------
tidy(path3_reg)

## -----------------------------------------------------------------------------
d4 <- data.frame(from = "X", to = "K")
d4 <- add_row(d4, data.frame(from = "Y", to = "K"))
print(d4)

## -----------------------------------------------------------------------------
path4_functions <- function_from_edge(d4)
print(path4_functions)

## -----------------------------------------------------------------------------
function_X <- function(n){
  X <- rnorm(n, mean = 0, sd = 5)
  return(X)
}

## -----------------------------------------------------------------------------
function_Y <- function(n){
  Y_num <- 0.7 * rnorm(n, mean = 0, sd = 5)
  Y_prob <- 1/(1 + exp(-Y_num))
  Y_cat <- ifelse(Y_prob>0.5,1,0)
  Y <- factor(Y_cat)
  return(Y)
}

## -----------------------------------------------------------------------------
function_K <- function(X, Y){
  K_num <- 0.7 * X
  K_prob <- 1/(1 + exp(-K_num))
  Y_num <- 0.3 * as.numeric(Y)
  K_prob <- K_prob + Y_num
  K_cat <- ifelse(K_prob>0.5,1,0)
  K <- factor(K_cat)
  return(K)
}

## -----------------------------------------------------------------------------
path4_functions <- define(path4_functions, which = 'X', what = function_X)
path4_functions <- define(path4_functions, which = 'Y', what = function_Y)
path4_functions <- define(path4_functions, which = 'K', what = function_K)
print(path4_functions)

## -----------------------------------------------------------------------------
set.seed(1)
path4_data <- data_from_function(path4_functions, n = 50000)
head(path4_data)

## ----echo=TRUE, results='hide', message=FALSE, warning=FALSE------------------
path4_reg <- glm(Y ~ X, family = binomial(), data = path4_data)

## -----------------------------------------------------------------------------
tidy(path4_reg)

