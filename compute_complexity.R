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

normalize_complexity <- function(value, str_length, type) {
  n <- str_length
  
  # normalize the results by comparing them with maximal entropy/complexity of strings of similar length
  max_string <- paste(letters[runif(n,1,10)], collapse = '')
  min_string <- paste(rep('a', n), collapse = '')
  
  min_complexity <- sum(unlist(local_complexity(min_string, span = 10)), na.rm = TRUE)
  max_complexity <- sum(unlist(local_complexity(max_string, span = 10)), na.rm = TRUE)
  min_entropy <- entropy(min_string)[[1]]
  max_entropy <- entropy(max_string)[[1]]
  
  if (type == 0) {
    result <- (value - min_entropy) / (max_entropy - min_entropy)
  }
  else {
    result <- (value - min_complexity) / (max_complexity - min_complexity)
  }
  
  result
}

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
  kA <- sum(unlist(local_complexity(sA, span = 10)), na.rm = TRUE)
  kL <- sum(unlist(local_complexity(sL, span = 10)), na.rm = TRUE)
  kd <- sum(unlist(local_complexity(sd, span = 10)), na.rm = TRUE)
  kD <- sum(unlist(local_complexity(sD, span = 10)), na.rm = TRUE)
  
  eA <- normalize_complexity(eA, nchar(sA), 0)
  eL <- normalize_complexity(eL, nchar(sL), 0)
  ed <- normalize_complexity(ed, nchar(sd), 0)
  eD <- normalize_complexity(eD, nchar(sD), 0)
  
  kA <- normalize_complexity(kA, nchar(sA), 1)
  kL <- normalize_complexity(kL, nchar(sL), 1)
  kd <- normalize_complexity(kd, nchar(sd), 1)
  kD <- normalize_complexity(kD, nchar(sD), 1)
  
  result <- c(eA, eL, ed, eD, kA, kL, kd, kD)
  
  return(result) 
}
