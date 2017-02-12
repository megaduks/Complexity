# 
# Function to compute entropic and algorithmic complexity of a graph
#
# author: Mikołaj Morzy
# date: 11.02.2017
#
# The purpose of this function is to compute the entropy and Kolmogorov complexity
# of several properties of a graph:
#
# - adjacency matrix
# - Laplacian of the graph
# - vertex degree list
# - degree distribution

compute_complexity <- function(g) {
  
  # compute properties of a graph
  A <- as.vector(as_adjacency_matrix(g))
  L <- as.vector(laplacian_matrix(g))
  d <- degree(g, normalized = TRUE)
  D <- degree_distribution(g)
  
  # transform properties of a graph into strings
  sA <- paste(A, collapse = '')
  sL <- paste(L, collapse = '')
  sd <- paste(d, collapse = '')
  sD <- gsub('\\.', '', paste(D, collapse = ''))
  
  # extend sD string to at least 10 (span) characters
  
  if (length(sD) < 10)
    sD <- paste(paste(rep('0', 10 - length(sD)), collapse = ''), sD)
  
  # compute entropy of graph properties
  eA <- entropy(sA)[[1]]
  eL <- entropy(sL)[[1]]
  ed <- entropy(sd)[[1]]
  eD <- entropy(sD)[[1]]
  
  # compute Kolmogorov complexity of graph properties
  kA <- mean(unlist(local_complexity(sA, span = 10)), na.rm = TRUE)
  kL <- mean(unlist(local_complexity(sL, span = 10)), na.rm = TRUE)
  kd <- mean(unlist(local_complexity(sd, span = 10)), na.rm = TRUE)
  kD <- mean(unlist(local_complexity(sD, span = 10)), na.rm = TRUE)
  
  result <- c(eA, eL, ed, eD, kA, kL, kd, kD)
  
  return(result) 
}
