## ----echo=TRUE, results='hide', message=FALSE, warning=FALSE------------------
library(igraph)
library(ggnetwork)
library(tidyverse)

## ----include=FALSE------------------------------------------------------------
library(knitr)
library(kableExtra)
library(ggpubr)

## -----------------------------------------------------------------------------
# Create a data frame for the X and Y relationship
d <- data.frame(from = "X", to = "Y")

## ----echo=FALSE---------------------------------------------------------------
d %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
# Convert the data frame into an igraph object.
g <- graph_from_data_frame(d, directed = TRUE)
print(g)

## -----------------------------------------------------------------------------
# Lay out the graph as a tree
g_layout <- layout_as_tree(g)

# Determine the coordinates with ggnetwork
set.seed(1)
g_coord <- ggnetwork(g, layout = g_layout)

## ----echo=FALSE---------------------------------------------------------------
g_coord %>%
  kable() %>%
  kable_classic()

## ----fig.height=3.5, fig.width=7----------------------------------------------
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

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
print(g_plot)

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
g_coord %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_nodelabel(aes(label = name)) +
  coord_flip() +
  scale_y_reverse() +
  theme_void()

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
print(g_plot)

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
set.seed(1)
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

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
set.seed(1)
data.frame(from = "C", to = "X") %>%
  add_row(data.frame(from = "C", to = "Y")) %>%
  graph_from_data_frame(directed = TRUE)%>%
  ggnetwork(layout = layout_as_tree(.)) %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed"), curvature = 0.15) +
  geom_nodelabel(aes(label = name)) +
  theme_void()

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
set.seed(1)
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
  X <- rnorm(n, mean = 0, sd = 1)
  return(X)
}

## -----------------------------------------------------------------------------
function_Y <- function(X){
  Y <- 0.5 * X + rnorm(length(X), mean = 0.1, sd = 0.005)
  return(Y)
}

## -----------------------------------------------------------------------------
path1_functions <- define(path1_functions, which = 'X', what = function_X)
path1_functions <- define(path1_functions, which = 'Y', what = function_Y)
print(path1_functions)

## -----------------------------------------------------------------------------
set.seed(1)
path1_data <- data_from_function(path1_functions, n = 25000)

## ----echo=FALSE---------------------------------------------------------------
path1_data %>%
  head() %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
path1_reg <- lm(Y ~ X, data = path1_data)
path1_results <- tidy(path1_reg)

## ----echo=FALSE---------------------------------------------------------------
path1_results %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
d2 <- data.frame(from = "X", to = "M")
d2 <- add_row(d2, data.frame(from = "M", to = "Y"))

## ----echo=FALSE---------------------------------------------------------------
d2 %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
path2_functions <- function_from_edge(d2)
print(path2_functions)

## -----------------------------------------------------------------------------
function_X <- function(n){
  X <- rnorm(n, mean = 0, sd = 1)
  return(X)
}

## -----------------------------------------------------------------------------
function_M <- function(X){
  M <- 0.7 * X + rnorm(length(X), mean = 0.3, sd = 0.005)
  return(M)
}

## -----------------------------------------------------------------------------
function_Y <- function(M){
  Y <- 0.5 * M + rnorm(length(M), mean = 0.1, sd = 0.005)
  return(Y)
}

## -----------------------------------------------------------------------------
path2_functions <- define(path2_functions, which = 'X', what = function_X)
path2_functions <- define(path2_functions, which = 'M', what = function_M)
path2_functions <- define(path2_functions, which = 'Y', what = function_Y)
print(path2_functions)

## -----------------------------------------------------------------------------
set.seed(1)
path2_data <- data_from_function(path2_functions, n = 25000)

## ----echo=FALSE---------------------------------------------------------------
path2_data %>%
  head() %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
path2_reg <- lm(Y ~ X, data = path2_data)
path2_results <- tidy(path2_reg)

## ----echo=FALSE---------------------------------------------------------------
path2_results %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
d3 <- data.frame(from = "C", to = "X")
d3 <- add_row(d3, data.frame(from = "C", to = "Y"))

## ----echo=FALSE---------------------------------------------------------------
d3 %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
path3_functions <- function_from_edge(d3)
print(path3_functions)

## -----------------------------------------------------------------------------
function_C <- function(n){
  C <- rnorm(n, mean = 0, sd = 1)
  return(C)
}

## -----------------------------------------------------------------------------
function_X <- function(C){
  X <- 0.7 * C + rnorm(length(C), mean = 0.3, sd = 0.005)
  return(X)
}

## -----------------------------------------------------------------------------
function_Y <- function(C){
  Y <- 0.5 * C + rnorm(length(C), mean = 0.1, sd = 0.005)
  return(Y)
}

## -----------------------------------------------------------------------------
path3_functions <- define(path3_functions, which = 'C', what = function_C)
path3_functions <- define(path3_functions, which = 'X', what = function_X)
path3_functions <- define(path3_functions, which = 'Y', what = function_Y)
print(path3_functions)

## -----------------------------------------------------------------------------
set.seed(1)
path3_data <- data_from_function(path3_functions, n = 25000)

## ----echo=FALSE---------------------------------------------------------------
path3_data %>%
  head() %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
path3_reg <- lm(Y ~ X, data = path3_data)
path3_results <- tidy(path3_reg)

## ----echo=FALSE---------------------------------------------------------------
path3_results %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
d4 <- data.frame(from = "X", to = "K")
d4 <- add_row(d4, data.frame(from = "Y", to = "K"))

## ----echo=FALSE---------------------------------------------------------------
d4 %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
path4_functions <- function_from_edge(d4)
print(path4_functions)

## -----------------------------------------------------------------------------
function_X <- function(n){
  X <- rnorm(n, mean = 0, sd = 1)
  return(X)
}

## -----------------------------------------------------------------------------
function_Y <- function(n){
  Y <- rnorm(n, mean = 0.1, sd = 0.005)
  return(Y)
}

## -----------------------------------------------------------------------------
function_K <- function(X, Y){
  K <- 0.7 * X + 0.5 * Y + rnorm(length(X), mean = 0.1, sd = 0.005)
  return(K)
}

## -----------------------------------------------------------------------------
path4_functions <- define(path4_functions, which = 'X', what = function_X)
path4_functions <- define(path4_functions, which = 'Y', what = function_Y)
path4_functions <- define(path4_functions, which = 'K', what = function_K)
print(path4_functions)

## -----------------------------------------------------------------------------
set.seed(1)
path4_data <- data_from_function(path4_functions, n = 25000)

## ----echo=FALSE---------------------------------------------------------------
path4_data %>%
  head() %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
path4_reg <- lm(Y ~ X, data = path4_data)
path4_results <- tidy(path4_reg)

## ----echo=FALSE---------------------------------------------------------------
path4_results %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
d2_with_d <- add_row(d2, d)
d3_with_d <- add_row(d3, d)
d4_with_d <- add_row(d4, d)

## ----include=FALSE------------------------------------------------------------
d234_list <- list(d2_with_d, d3_with_d, d4_with_d)

d234_g <- lapply(d234_list, graph_from_data_frame)

d234_g_layout <- lapply(d234_g, layout_as_tree)
set.seed(1)
d234_g_coord <- mapply(ggnetwork, d234_g, d234_g_layout, SIMPLIFY = FALSE)

d234_g_coord[[4]] <- slice(d234_g_coord[[1]], -3)
d234_g_coord[[5]] <- slice(d234_g_coord[[2]], -3)
d234_g_coord[[6]] <- slice(d234_g_coord[[3]], -2)

d234_g_plots <-
  d234_g_coord %>%
  lapply(\(x)
    x %>%
      ggplot(aes(x, y, xend = xend, yend = yend)) +
      geom_edges(arrow = arrow(type = "closed"), curvature = 0.05) +
      geom_nodelabel(aes(label = name))
  )

d234_g_plots[c(1, 4)] <-
  d234_g_plots[c(1, 4)] %>%
  lapply(\(x)
    x +
      coord_flip() +
      scale_x_reverse()
  )

d234_g_plots[c(2, 5)] <-
  d234_g_plots[c(2, 5)] %>%
  lapply(\(x)
    x +
      scale_x_reverse() +
      scale_y_reverse()
  )

d234_g_plots[c(3, 6)] <-
  d234_g_plots[c(3, 6)] %>%
  lapply(\(x)
    x +
      coord_flip() +
      scale_x_reverse() +
      scale_y_reverse()
  )

d234_g_plots <-
  d234_g_plots %>%
  lapply(\(x) x + theme_void())

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
ggarrange(
  d234_g_plots[[4]],
  d234_g_plots[[1]]
)

## -----------------------------------------------------------------------------
path2_d_functions <- function_from_edge(d2_with_d)
print(path2_d_functions)

## -----------------------------------------------------------------------------
function_X <- function(n){
  X <- rnorm(n, mean = 0, sd = 1)
  return(X)
}

function_M <- function(X){
  M <- 0.7 * X + rnorm(length(X), mean = 0.3, sd = 0.005)
  return(M)
}

function_Y <- function(X, M){
  Y <- 0.25 * X + 0.5 * M + rnorm(length(X), mean = 0.1, sd = 0.005)
  return(Y)
}

path2_d_functions <- define(path2_d_functions, which = "X", what = function_X)
path2_d_functions <- define(path2_d_functions, which = "M", what = function_M)
path2_d_functions <- define(path2_d_functions, which = "Y", what = function_Y)

set.seed(1)
path2_d_data <- data_from_function(path2_d_functions, n = 25000)
path2_d_reg <- lm(Y ~ X, data = path2_d_data)
path2_d_results <- tidy(path2_d_reg)

## -----------------------------------------------------------------------------
path2_cond_reg <- lm(Y ~ X + M, data = path2_data)
path2_cond_results <- tidy(path2_cond_reg)

path2_cond_d_reg <- lm(Y ~ X + M, data = path2_d_data)
path2_cond_d_results <- tidy(path2_cond_d_reg)

## ----echo=FALSE---------------------------------------------------------------
rbind(
  path2_results %>%
    mutate(causal_path = "No", conditioned = "No")
  ,path2_cond_results %>%
    mutate(causal_path = "No", conditioned = "Yes")
  ,path2_d_results %>%
    mutate(causal_path = "Yes", conditioned = "No")
  ,path2_cond_d_results %>%
    mutate(causal_path = "Yes", conditioned = "Yes")
) %>%
  select(causal_path, conditioned, everything()) %>%
  kable() %>%
  kable_classic()

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
ggarrange(
  d234_g_plots[[2]],
  d234_g_plots[[5]]
)

## -----------------------------------------------------------------------------
path3_d_functions <- function_from_edge(d3_with_d)
print(path3_d_functions)

## -----------------------------------------------------------------------------
function_C <- function(n){
  X <- rnorm(n, mean = 0, sd = 1)
  return(X)
}

function_X <- function(C){
  X <- 0.7 * C + rnorm(length(C), mean = 0.3, sd = 0.005)
  return(X)
}

function_Y <- function(X, C){
  Y <- 0.25 * X + 0.5 * C + rnorm(length(C), mean = 0.1, sd = 0.005)
  return(Y)
}

path3_d_functions <- define(path3_d_functions, which = "C", what = function_C)
path3_d_functions <- define(path3_d_functions, which = "X", what = function_X)
path3_d_functions <- define(path3_d_functions, which = "Y", what = function_Y)

set.seed(1)
path3_d_data <- data_from_function(path3_d_functions, n = 25000)
path3_d_reg <- lm(Y ~ X, data = path3_d_data)
path3_d_results <- tidy(path3_d_reg)

## -----------------------------------------------------------------------------
path3_cond_reg <- lm(Y ~ X + C, data = path3_data)
path3_cond_results <- tidy(path3_cond_reg)

path3_cond_d_reg <- lm(Y ~ X + C, data = path3_d_data)
path3_cond_d_results <- tidy(path3_cond_d_reg)

## ----echo=FALSE---------------------------------------------------------------
rbind(
  path3_results %>%
    mutate(causal_path = "No", conditioned = "No")
  ,path3_cond_results %>%
    mutate(causal_path = "No", conditioned = "Yes")
  ,path3_d_results %>%
    mutate(causal_path = "Yes", conditioned = "No")
  ,path3_cond_d_results %>%
    mutate(causal_path = "Yes", conditioned = "Yes")
) %>%
  select(causal_path, conditioned, everything()) %>%
  kable() %>%
  kable_classic()

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
ggarrange(
  d234_g_plots[[3]],
  d234_g_plots[[6]]
)

## -----------------------------------------------------------------------------
path4_d_functions <- function_from_edge(d4_with_d)
print(path4_d_functions)

## -----------------------------------------------------------------------------
function_X <- function(n){
  X <- rnorm(n, mean = 0, sd = 1)
  return(X)
}

function_Y <- function(X){
  Y <- 0.5 * X + rnorm(length(X), mean = 0.1, sd = 0.005)
  return(Y)
}

function_K <- function(X, Y){
  K <- 0.7 * X + 0.5 * Y + rnorm(length(X), mean = 0.1, sd = 0.005)
  return(K)
}

path4_d_functions <- define(path4_d_functions, which = "X", what = function_X)
path4_d_functions <- define(path4_d_functions, which = "Y", what = function_Y)
path4_d_functions <- define(path4_d_functions, which = "K", what = function_K)

set.seed(1)
path4_d_data <- data_from_function(path4_d_functions, n = 25000)
path4_d_reg <- lm(Y ~ X, data = path4_d_data)
path4_d_results <- tidy(path4_d_reg)

## -----------------------------------------------------------------------------
path4_cond_reg <- lm(Y ~ X + K, data = path4_data)
path4_cond_results <- tidy(path4_cond_reg)

path4_cond_d_reg <- lm(Y ~ X + K, data = path4_d_data)
path4_cond_d_results <- tidy(path4_cond_d_reg)

## ----echo=FALSE---------------------------------------------------------------
rbind(
  path4_results %>%
    mutate(causal_path = "No", conditioned = "No")
  ,path4_cond_results %>%
    mutate(causal_path = "No", conditioned = "Yes")
  ,path4_d_results %>%
    mutate(causal_path = "Yes", conditioned = "No")
  ,path4_cond_d_results %>%
    mutate(causal_path = "Yes", conditioned = "Yes")
) %>%
  select(causal_path, conditioned, everything()) %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
d5 <- data.frame(from = "X1", to = "Y")
d5 <- add_row(d5, data.frame(from = "X2", to = "Y"))

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
set.seed(1)
d5 %>%
  graph_from_data_frame() %>%
  ggnetwork(layout = layout_as_tree(.)) %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed"), curvature = 0.05) +
  geom_nodelabel(aes(label = name)) +
  coord_flip() +
  scale_y_reverse() +
  theme_void()

## -----------------------------------------------------------------------------
d5_functions <- function_from_edge(d5)
print(d5_functions)

## -----------------------------------------------------------------------------
function_X1 <- function(n){
  X1 <- sample(c(0, 1), n, replace = T)
  return(X1)
}

function_X2 <- function(n){
  X2 <- sample(c(0, 1), n, replace = T)
  return(X2)
}

function_Y <- function(X1, X2){
  Y <- as.numeric(X1 == 1 & X2 == 1)
  return(Y)
}

d5_functions <- define(d5_functions, which = "X1", what = function_X1)
d5_functions <- define(d5_functions, which = "X2", what = function_X2)
d5_functions <- define(d5_functions, which = "Y", what = function_Y)

set.seed(1)
d5_data <- data_from_function(d5_functions, n = 25000)
d5_reg <- lm(Y ~ X1 + X2, data = d5_data)
d5_results <- tidy(d5_reg)

## ----echo=FALSE---------------------------------------------------------------
d5_results %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
test_data <- expand.grid(X1 = c(0, 1), X2 = c(0, 1))
d5_test <- mutate(test_data, Y = mapply(function_Y, X1, X2))
d5_test <- mutate(d5_test, Y_hat = predict(d5_reg, newdata = d5_test))
d5_test <- mutate(d5_test, Y_hat = round(Y_hat))

## ----echo=FALSE---------------------------------------------------------------
d5_test %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
d6_functions <- function_from_edge(d5)

function_Y <- function(X1, X2){
  Y <- as.numeric(X1 == 1 | X2 == 1)
  return(Y)
}

d6_functions <- define(d6_functions, which = "X1", what = function_X1)
d6_functions <- define(d6_functions, which = "X2", what = function_X2)
d6_functions <- define(d6_functions, which = "Y", what = function_Y)

set.seed(1)
d6_data <- data_from_function(d6_functions, n = 25000)
d6_reg <- lm(Y ~ X1 + X2, data = d6_data)
d6_results <- tidy(d6_reg)

d6_test <- mutate(test_data, Y = mapply(function_Y, X1, X2))
d6_test <- mutate(d6_test, Y_hat = predict(d6_reg, newdata = d6_test))
d6_test <- mutate(d6_test, Y_hat = round(Y_hat))

## ----echo=FALSE---------------------------------------------------------------
d6_results %>%
  kable() %>%
  kable_classic()

## ----echo=FALSE---------------------------------------------------------------
d6_test %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
d7_functions <- function_from_edge(d5)

function_Y <- function(X1, X2){
  Y <- as.numeric((X1 == 1 & X2 == 0) | (X1 == 0 & X2 == 1))
  return(Y)
}

d7_functions <- define(d7_functions, which = "X1", what = function_X1)
d7_functions <- define(d7_functions, which = "X2", what = function_X2)
d7_functions <- define(d7_functions, which = "Y", what = function_Y)

set.seed(1)
d7_data <- data_from_function(d7_functions, n = 25000)
d7_reg <- lm(Y ~ X1 + X2, data = d7_data)
d7_results <- tidy(d7_reg)

d7_test <- mutate(test_data, Y = mapply(function_Y, X1, X2))
d7_test <- mutate(d7_test, Y_hat = predict(d7_reg, newdata = d7_test))
d7_test <- mutate(d7_test, Y_hat = round(Y_hat))

## ----echo=FALSE---------------------------------------------------------------
d7_results %>%
  kable() %>%
  kable_classic()

## ----echo=FALSE---------------------------------------------------------------
d7_test %>%
  kable() %>%
  kable_classic()

## -----------------------------------------------------------------------------
d8 <- data.frame(from = "X", to = "Y")
d8 <- add_row(d8, data.frame(from = "X", to = "Xm"))
d8 <- add_row(d8, data.frame(from = "UX", to = "Xm"))
d8 <- add_row(d8, data.frame(from = "Y", to = "Ym"))
d8 <- add_row(d8, data.frame(from = "UY", to = "Ym"))

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
set.seed(1)
d8 %>%
  filter(from %in% c("X","Y") & to %in% c("X","Y")) %>%
  graph_from_data_frame() %>%
  ggnetwork(layout = layout_as_tree(.)) %>%
  add_row(data.frame(x = 0.5, y = 1, name = "X", xend = 0.975, yend = 1)) %>%
  add_row(data.frame(x = 1, y = 1, name = "X'", xend = 1, yend = 1)) %>%
  add_row(data.frame(x = 0.5, y = 0, name = "Y", xend = 0.975, yend = 0)) %>%
  add_row(data.frame(x = 1, y = 0, name = "Y'", xend = 1, yend = 0)) %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed"), curvature = 0.05) +
  geom_nodelabel(aes(label = name)) +
  coord_flip() +
  scale_y_reverse() +
  theme_void()

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
d8_nondif <- list()

set.seed(1)
d8_nondif[[1]] <- d8 %>%
  filter(from %in% c("X","Y") & to %in% c("X","Y")) %>%
  graph_from_data_frame() %>%
  ggnetwork(layout = layout_as_tree(.)) %>%
  add_row(data.frame(x = 0.5, y = 1, name = "X", xend = 0.975, yend = 1)) %>%
  add_row(data.frame(x = 1, y = 1, name = "X'", xend = 1, yend = 1)) %>%
  add_row(data.frame(x = 1.5, y = 1, name = "UX", xend = 1.025, yend = 1)) %>%
  add_row(data.frame(x = 0.5, y = 0, name = "Y", xend = 0.975, yend = 0)) %>%
  add_row(data.frame(x = 1, y = 0, name = "Y'", xend = 1, yend = 0)) %>%
  add_row(data.frame(x = 1.5, y = 0, name = "UY", xend = 1.025, yend = 0)) %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed"), curvature = 0.05) +
  geom_nodelabel(aes(label = name)) +
  coord_flip() +
  scale_y_reverse() +
  theme_void()

set.seed(1)
d8_nondif[[2]] <- d8 %>%
  filter(from %in% c("X","Y") & to %in% c("X","Y")) %>%
  graph_from_data_frame() %>%
  ggnetwork(layout = layout_as_tree(.)) %>%
  add_row(data.frame(x = 0.5, y = 1, name = "X", xend = 0.975, yend = 1)) %>%
  add_row(data.frame(x = 1, y = 1, name = "X'", xend = 1, yend = 1)) %>%
  add_row(data.frame(x = 0.5, y = 0, name = "Y", xend = 0.975, yend = 0)) %>%
  add_row(data.frame(x = 1, y = 0, name = "Y'", xend = 1, yend = 0)) %>%
  add_row(data.frame(x = 1, y = 0.5, name = "UXY", xend = 1, yend = 0.975)) %>%
  add_row(data.frame(x = 1, y = 0.5, name = "UXY", xend = 1, yend = 0.025)) %>%
  add_row(data.frame(x = 1, y = 0.5, name = "UXY", xend = 1, yend = 0.5)) %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed"), curvature = 0.05) +
  geom_nodelabel(aes(label = name)) +
  coord_flip() +
  scale_y_reverse() +
  theme_void()

ggarrange(
  d8_nondif[[1]],
  d8_nondif[[2]]
)

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
d8_ind_dif <- list()

set.seed(1)
d8_ind_dif[[1]] <- d8 %>%
  filter(from %in% c("X","Y") & to %in% c("X","Y")) %>%
  graph_from_data_frame() %>%
  ggnetwork(layout = layout_as_tree(.)) %>%
  add_row(data.frame(x = 0.5, y = 1, name = "X", xend = 0.975, yend = 1)) %>%
  add_row(data.frame(x = 1, y = 1, name = "X'", xend = 1, yend = 1)) %>%
  add_row(data.frame(x = 1.5, y = 1, name = "UX", xend = 1.025, yend = 1)) %>%
  add_row(data.frame(x = 0.5, y = 0, name = "Y", xend = 0.975, yend = 0)) %>%
  add_row(data.frame(x = 1, y = 0, name = "Y'", xend = 1, yend = 0)) %>%
  add_row(data.frame(x = 1.5, y = 0, name = "UY", xend = 1.025, yend = 0)) %>%
  add_row(data.frame(x = 0.5, y = 1, name = "X", xend = 1.5, yend = 0.025)) %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed"), curvature = 0.05) +
  geom_nodelabel(aes(label = name)) +
  coord_flip() +
  scale_y_reverse() +
  theme_void()

set.seed(1)
d8_ind_dif[[2]] <- d8 %>%
  filter(from %in% c("X","Y") & to %in% c("X","Y")) %>%
  graph_from_data_frame() %>%
  ggnetwork(layout = layout_as_tree(.)) %>%
  add_row(data.frame(x = 0.5, y = 1, name = "X", xend = 0.975, yend = 1)) %>%
  add_row(data.frame(x = 1, y = 1, name = "X'", xend = 1, yend = 1)) %>%
  add_row(data.frame(x = 1.5, y = 1, name = "UX", xend = 1.025, yend = 1)) %>%
  add_row(data.frame(x = 0.5, y = 0, name = "Y", xend = 0.975, yend = 0)) %>%
  add_row(data.frame(x = 1, y = 0, name = "Y'", xend = 1, yend = 0)) %>%
  add_row(data.frame(x = 1.5, y = 0, name = "UY", xend = 1.025, yend = 0)) %>%
  add_row(data.frame(x = 0.5, y = 0, name = "Y", xend = 1.5, yend = 0.975)) %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed"), curvature = 0.05) +
  geom_nodelabel(aes(label = name)) +
  coord_flip() +
  scale_y_reverse() +
  theme_void()

ggarrange(
  d8_ind_dif[[1]],
  d8_ind_dif[[2]]
)

## -----------------------------------------------------------------------------
I <- seq(10)
UX_epsilon <- seq(0.005, 4, length = length(I))
UY_epsilon <- seq(0.005, 8, length = length(I))

d8_results <- list()

for(i in I){
  d8_functions <- function_from_edge(d8)
  
  function_X <- function(n){
    X <- rnorm(n, mean = 0, sd = 1)
    return(X)
  }
  
  function_UX <- function(n){
    UX <- rnorm(n, mean = 0, sd = UX_epsilon[[i]])
    return(UX)
  }
  
  function_Xm <- function(X, UX){
    Xm <- X + UX
    return(Xm)
  }
  
  function_Y <- function(X){
    Y <- 0.5 * X + rnorm(length(X), mean = 0.1, sd = 0.005)
    return(Y)
  }
  
  function_UY <- function(n){
    UY <- rnorm(n, mean = 0, sd = UY_epsilon[[i]])
    return(UY)
  }
  
  function_Ym <- function(Y, UY){
    Ym <- Y + UY
    return(Ym)
  }
  
  d8_functions <- define(d8_functions, which = "X", what = function_X)
  d8_functions <- define(d8_functions, which = "UX", what = function_UX)
  d8_functions <- define(d8_functions, which = "Xm", what = function_Xm)
  d8_functions <- define(d8_functions, which = "Y", what = function_Y)
  d8_functions <- define(d8_functions, which = "UY", what = function_UY)
  d8_functions <- define(d8_functions, which = "Ym", what = function_Ym)
  
  set.seed(1)
  d8_data <- data_from_function(d8_functions, n = 25000)
  d8_reg <- lm(Ym ~ Xm, data = d8_data)
  d8_results[[i]] <- tidy(d8_reg)
}

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
d8_results_rbind <- d8_results %>%
  do.call(rbind,.) %>%
  group_by(term) %>%
  mutate(i = seq(n())) %>%
  ungroup() %>%
  left_join(
    data.frame(term = "(Intercept)", i = I, epsilon = UY_epsilon) %>%
      rbind(
        data.frame(term = "Xm", i = I, epsilon = UX_epsilon)
      ),
    by = join_by(term, i)
  ) %>%
  mutate(lb = estimate - qnorm(0.975) * std.error) %>%
  mutate(ub = estimate + qnorm(0.975) * std.error)

d8_plot <- list()

d8_plot[[1]] <- d8_results_rbind %>%
  ggplot(aes(epsilon, estimate)) +
  geom_hline(aes(yintercept = ifelse(term == "Xm", 0.5, 0.1)), lty = 2) +
  geom_point() +
  geom_line() +
  facet_wrap(~ term, scales = "free")

d8_plot[[2]] <- d8_results_rbind %>%
  ggplot(aes(epsilon, p.value)) +
  geom_hline(yintercept = 0.05, lty = 2) +
  geom_point() +
  geom_line() +
  facet_wrap(~ term, scales = "free")

ggarrange(
  d8_plot[[1]],
  d8_plot[[2]],
  ncol = 1,
  nrow = 2
)

## -----------------------------------------------------------------------------
d9 <- data.frame(from = "X", to = "Y")
d9 <- add_row(d9, data.frame(from = "X", to = "Xm"))
d9 <- add_row(d9, data.frame(from = "UX", to = "Xm"))
d9 <- add_row(d9, data.frame(from = "RX", to = "Xm"))
d9 <- add_row(d9, data.frame(from = "Y", to = "Ym"))
d9 <- add_row(d9, data.frame(from = "UY", to = "Ym"))
d9 <- add_row(d9, data.frame(from = "RY", to = "Ym"))

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
set.seed(1)
d9 %>%
  filter(from %in% c("X","Y") & to %in% c("X","Y")) %>%
  graph_from_data_frame() %>%
  ggnetwork(layout = layout_as_tree(.)) %>%
  add_row(data.frame(x = 0.5, y = 1, name = "X", xend = 0.975, yend = 1)) %>%
  add_row(data.frame(x = 1, y = 1, name = "X'", xend = 1, yend = 1)) %>%
  add_row(data.frame(x = 1.5, y = 1, name = "UX", xend = 1.025, yend = 1)) %>%
  add_row(data.frame(x = 1.5, y = 0.7, name = "RX", xend = 1.025, yend = 1)) %>%
  add_row(data.frame(x = 0.5, y = 0, name = "Y", xend = 0.975, yend = 0)) %>%
  add_row(data.frame(x = 1, y = 0, name = "Y'", xend = 1, yend = 0)) %>%
  add_row(data.frame(x = 1.5, y = 0, name = "UY", xend = 1.025, yend = 0)) %>%
  add_row(data.frame(x = 1.5, y = 0.3, name = "RY", xend = 1.025, yend = 0)) %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed"), curvature = 0.05) +
  geom_nodelabel(aes(label = name)) +
  coord_flip() +
  scale_y_reverse() +
  theme_void()

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
set.seed(1)
d9 %>%
  filter(from %in% c("X","Y") & to %in% c("X","Y")) %>%
  graph_from_data_frame() %>%
  ggnetwork(layout = layout_as_tree(.)) %>%
  add_row(data.frame(x = 0.5, y = 1, name = "X", xend = 0.975, yend = 1)) %>%
  add_row(data.frame(x = 1, y = 1, name = "X'", xend = 1, yend = 1)) %>%
  add_row(data.frame(x = 1.5, y = 1, name = "UX", xend = 1.025, yend = 1)) %>%
  add_row(data.frame(x = 1.5, y = 0.7, name = "RX", xend = 1.025, yend = 1)) %>%
  add_row(data.frame(x = 0.5, y = 0, name = "Y", xend = 1.5, yend = 0.675)) %>%
  add_row(data.frame(x = 0.5, y = 0, name = "Y", xend = 0.975, yend = 0)) %>%
  add_row(data.frame(x = 1, y = 0, name = "Y'", xend = 1, yend = 0)) %>%
  add_row(data.frame(x = 1.5, y = 0, name = "UY", xend = 1.025, yend = 0)) %>%
  add_row(data.frame(x = 1.5, y = 0.3, name = "RY", xend = 1.025, yend = 0)) %>%
  add_row(data.frame(x = 0.5, y = 1, name = "X", xend = 1.5, yend = 0.325)) %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed"), curvature = 0.05) +
  geom_nodelabel(aes(label = name)) +
  coord_flip() +
  scale_y_reverse() +
  theme_void()

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
set.seed(1)
d9 %>%
  filter(from %in% c("X","Y") & to %in% c("X","Y")) %>%
  graph_from_data_frame() %>%
  ggnetwork(layout = layout_as_tree(.)) %>%
  add_row(data.frame(x = 0.5, y = 1, name = "X", xend = 0.975, yend = 1)) %>%
  add_row(data.frame(x = 1, y = 1, name = "X'", xend = 1, yend = 1)) %>%
  add_row(data.frame(x = 1.5, y = 1, name = "UX", xend = 1.025, yend = 1)) %>%
  add_row(data.frame(x = 1.5, y = 0.7, name = "RX", xend = 1.025, yend = 1)) %>%
  add_row(data.frame(x = 0.5, y = 1, name = "X", xend = 1.475, yend = 0.7)) %>%
  add_row(data.frame(x = 0.5, y = 0, name = "Y", xend = 0.975, yend = 0)) %>%
  add_row(data.frame(x = 1, y = 0, name = "Y'", xend = 1, yend = 0)) %>%
  add_row(data.frame(x = 1.5, y = 0, name = "UY", xend = 1.025, yend = 0)) %>%
  add_row(data.frame(x = 1.5, y = 0.3, name = "RY", xend = 1.025, yend = 0)) %>%
  add_row(data.frame(x = 0.5, y = 0, name = "Y", xend = 1.475, yend = 0.3)) %>%
  add_row(data.frame(x = 0.5, y = 1, name = "X", xend = 1.5, yend = 0.325)) %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed"), curvature = 0.05) +
  geom_nodelabel(aes(label = name)) +
  coord_flip() +
  scale_y_reverse() +
  theme_void()

## -----------------------------------------------------------------------------
I <- seq(10)
RX_p <- seq(0, 0.95, length = length(I))
RX_p <- mapply(c, 1 - RX_p, RX_p)
RY_p <- seq(0, 0.95, length = length(I))
RY_p <- mapply(c, 1 - RY_p, RY_p)

d9_results <- list()

for(i in I){
  d9_functions <- function_from_edge(d9)
  
  function_X <- function(n){
    X <- rnorm(n, mean = 0, sd = 1)
    return(X)
  }
  
  function_UX <- function(n){
    UX <- rnorm(n, mean = 0, sd = 0.01)
    return(UX)
  }
  
  function_RX <- function(n){
    RX <- sample(c(0, 1), size = n, replace = TRUE, prob = RX_p[, i])
    return(RX)
  }
  
  function_Xm <- function(X, UX, RX){
    Xm <- ifelse(RX == 1, NA, X + UX)
    return(Xm)
  }
  
  function_Y <- function(X){
    Y <- 0.5 * X + rnorm(length(X), mean = 0.1, sd = 0.005)
    return(Y)
  }
  
  function_UY <- function(n){
    UY <- rnorm(n, mean = 0, sd = 0.01)
    return(UY)
  }
  
  function_RY <- function(n){
    RY <- sample(c(0, 1), size = n, replace = TRUE, prob = RY_p[, i])
    return(RY)
  }
  
  function_Ym <- function(Y, UY, RY){
    Ym <- ifelse(RY == 1, NA, Y + UY)
    return(Ym)
  }
  
  d9_functions <- define(d9_functions, which = "X", what = function_X)
  d9_functions <- define(d9_functions, which = "UX", what = function_UX)
  d9_functions <- define(d9_functions, which = "RX", what = function_RX)
  d9_functions <- define(d9_functions, which = "Xm", what = function_Xm)
  d9_functions <- define(d9_functions, which = "Y", what = function_Y)
  d9_functions <- define(d9_functions, which = "UY", what = function_UY)
  d9_functions <- define(d9_functions, which = "RY", what = function_RY)
  d9_functions <- define(d9_functions, which = "Ym", what = function_Ym)
  
  set.seed(1)
  d9_data <- data_from_function(d9_functions, n = 25000)
  d9_reg <- lm(Ym ~ Xm, data = d9_data)
  d9_results[[i]] <- tidy(d9_reg)
}

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
d9_results_rbind <- d9_results %>%
  do.call(rbind,.) %>%
  group_by(term) %>%
  mutate(i = seq(n())) %>%
  ungroup() %>%
  left_join(
    data.frame(term = "(Intercept)", i = I, p = RY_p[2,]) %>%
      rbind(
        data.frame(term = "Xm", i = I, p = RX_p[2,])
      ),
    by = join_by(term, i)
  ) %>%
  mutate(lb = estimate - qnorm(0.975) * std.error) %>%
  mutate(ub = estimate + qnorm(0.975) * std.error)

d9_plot <- list()

d9_plot[[1]] <- d9_results_rbind %>%
  ggplot(aes(p, estimate)) +
  geom_hline(aes(yintercept = ifelse(term == "Xm", 0.5, 0.1)), lty = 2) +
  geom_point() +
  geom_line() +
  facet_wrap(~ term, scales = "free")

d9_plot[[2]] <- d9_results_rbind %>%
  ggplot(aes(p, p.value)) +
  geom_hline(yintercept = 0.05, lty = 2) +
  geom_point() +
  geom_line() +
  facet_wrap(~ term, scales = "free")

ggarrange(
  d9_plot[[1]],
  d9_plot[[2]],
  ncol = 1,
  nrow = 2
)

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
data.frame(from = "X(t)", to = "Y(t)") %>%
  graph_from_data_frame() %>%
  ggnetwork(layout = layout_as_tree(.)) %>%
  add_row(data.frame(x = 0.5, y = 1, name = "X(t)", xend = 0.975, yend = 1)) %>%
  add_row(data.frame(x = 0.5, y = 0, name = "Y(t)", xend = 0.975, yend = 0)) %>%
  add_row(data.frame(x = 1, y = 1, name = "X(t+1)", xend = 1, yend = 0.035)) %>%
  add_row(data.frame(x = 1, y = 0, name = "Y(t+1)", xend = 1, yend = 0)) %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed"), curvature = 0.05) +
  geom_nodelabel(aes(label = name)) +
  coord_flip() +
  scale_y_reverse() +
  theme_void()

## -----------------------------------------------------------------------------
d10 <- data.frame(from = "X", to = "Y")

d10_functions <- function_from_edge(d10)

function_X <- function(n){
  X <- rnorm(n, mean = 0, sd = 1)
  X <- abs(X)
  return(X)
}

function_Y <- function(X){
  Y <- 0.5 * X + rnorm(length(X), mean = 0.1, sd = 0.005)
  return(Y)
}

d10_functions <- define(d10_functions, which = "X", what = function_X)
d10_functions <- define(d10_functions, which = "Y", what = function_Y)

d10_data <- data_from_function(d10_functions, n = 10000)

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
set.seed(1)
data.frame(from = "X", to = "Y") %>%
  graph_from_data_frame() %>%
  ggnetwork(layout = layout_as_tree(.)) %>%
  add_row(data.frame(x = 0.5, y = 1, name = "X", xend = 0.975, yend = 0.025)) %>%
  add_row(data.frame(x = 0.5, y = 0, name = "Y", xend = 0.975, yend = 0)) %>%
  add_row(data.frame(x = 1, y = 0, name = "Y", xend = 1, yend = 0)) %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed"), curvature = 0.05) +
  geom_nodelabel(aes(label = name)) +
  coord_flip() +
  scale_y_reverse() +
  theme_void()

## -----------------------------------------------------------------------------
d10 <- add_row(d10, data.frame(from = "Y", to = "Y"))

function_Y <- function(X, Y){
  Y <- 0.5 * X + 1.1 * Y
  return(Y)
}

d10_functions <- define(d10_functions, which = "Y", what = function_Y)

set.seed(1)
d10_T_max <- rpois(nrow(d10_data), lambda = 25)
d10_data <- time_varying(d10_functions, data = d10_data, T_max = d10_T_max)

## ----echo=FALSE, fig.height=7, fig.width=7------------------------------------
d10_plot <- list()

set.seed(1); d10_plot[[1]] <- d10_data %>%
  filter(i %in% sample(unique(i), 5)) %>%
  gather(variable, value, -i, -t, -t_max) %>%
  ggplot(aes(t, value)) +
  geom_line() +
  facet_grid(i ~ variable, scales = "free_y")

d10_plot[[2]] <- d10_data %>%
  group_by(i) %>%
  filter(t == max(t)) %>%
  ungroup() %>%
  gather(variable, value, -i, -t, -t_max) %>%
  group_by(t, variable) %>%
  summarize(value = mean(value), .groups = "drop") %>%
  ggplot(aes(t, value)) +
  geom_line() +
  facet_wrap(~ variable, scales = "free_y", ncol = 2)

ggarrange(
  d10_plot[[1]],
  d10_plot[[2]],
  ncol = 1,
  nrow = 2
)

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
data.frame(from = "X", to = "Y") %>%
  graph_from_data_frame() %>%
  ggnetwork(layout = layout_as_tree(.)) %>%
  add_row(data.frame(x = 0.5, y = 0, name = "Y", xend = 0.5, yend = 0.975)) %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed"), curvature = 0.15) +
  geom_nodelabel(aes(label = name)) +
  coord_flip() +
  scale_y_reverse() +
  theme_void()

## ----echo=FALSE, fig.height=3.5, fig.width=7----------------------------------
data.frame(from = "X(t)", to = "Y(t)") %>%
  graph_from_data_frame() %>%
  ggnetwork(layout = layout_as_tree(.)) %>%
  add_row(data.frame(x = 0.5, y = 0, name = "Y(t)", xend = 0.975, yend = 0.975)) %>%
  add_row(data.frame(x = 1, y = 1, name = "X(t+1)", xend = 1, yend = 0.035)) %>%
  add_row(data.frame(x = 1, y = 0, name = "Y(t+1)", xend = 1, yend = 0)) %>%
  ggplot(aes(x, y, xend = xend, yend = yend)) +
  geom_edges(arrow = arrow(type = "closed"), curvature = 0.05) +
  geom_nodelabel(aes(label = name)) +
  coord_flip() +
  scale_y_reverse() +
  theme_void()

## -----------------------------------------------------------------------------
d11 <- data.frame(from = "X", to = "Y")

d11_functions <- function_from_edge(d11)

function_X <- function(n){
  X <- rnorm(n, mean = 0, sd = 1)
  return(X)
}

function_Y <- function(X){
  Y <- 0.5 * X + rnorm(length(X), mean = 0, sd = 0.00001)
  return(Y)
}

d11_functions <- define(d11_functions, which = "X", what = function_X)
d11_functions <- define(d11_functions, which = "Y", what = function_Y)

d11_data <- data_from_function(d11_functions, n = 10000)

## -----------------------------------------------------------------------------
d11 <- add_row(d11, data.frame(from = "Y", to = "X"))

function_X <- function(Y){
  X <- 2 * Y + rnorm(length(Y), mean = 0, sd = 0.00001)
  return(X)
}

d11_functions <- define(d11_functions, which = "X", what = function_X)

set.seed(1)
d11_T_max <- rpois(nrow(d11_data), lambda = 25)
d11_data <- time_varying(d11_functions, data = d11_data, T_max = d11_T_max)

## ----echo=FALSE, fig.height=7, fig.width=7------------------------------------
d11_plot <- list()

set.seed(1); d11_plot[[1]] <- d11_data %>%
  filter(i %in% sample(unique(i), 5)) %>%
  gather(variable, value, -i, -t, -t_max) %>%
  group_by(i, variable) %>%
  mutate(z_value = scale(value)) %>%
  ggplot(aes(t, z_value)) +
  geom_line() +
  facet_grid(i ~ variable, scales = "free_y")

d11_plot[[2]] <- d11_data %>%
  group_by(i) %>%
  filter(t == max(t)) %>%
  ungroup() %>%
  gather(variable, value, -i, -t, -t_max) %>%
  group_by(t, variable) %>%
  summarize(value = mean(value), .groups = "drop") %>%
  ggplot(aes(t, value)) +
  geom_line() +
  facet_wrap(~ variable, scales = "free_y", ncol = 2)

ggarrange(
  d11_plot[[1]],
  d11_plot[[2]],
  ncol = 1,
  nrow = 2
)

