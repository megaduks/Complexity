library(igraph)
library(dplyr)
library(reshape2)
library(acss)
library(ggplot2)
library(ggnet)
library(numbers)

source('compute_complexity.R')
source('to_powerlaw.R')
source('to_random.R')

# size of input graphs
num.vertices <- 100

# number of graphs sampled for each value of the edge rewiring probability
num.graphs <- 50

# possible values of edge rewiring probability
#edge.rewiring.probability <- 0:num.vertices/num.vertices
edge.rewiring.probability <- seq(0, 1, by=0.05)

# flag to mark whether the transition is from small world to random network
transition.to.random <- TRUE

results <- data.frame()
graphs <- list()

for (transition.to.random in c(TRUE)) {

  for (i in 1:num.graphs) {
    
    # the initial perfect WS graph with no edge rewiring
    g <- watts.strogatz.game(dim = 1, size = num.vertices, p = 0, nei = 2)
    E(g)$rewired <- FALSE
    
    for (erp in edge.rewiring.probability) {
    
      if (transition.to.random) {
        g <- to_random(graph = g, p = 0.05)
      } else {
        g <- to_powerlaw(graph = g, p = 0.05)
      }
      
      results <- rbind(results, c(erp, compute_complexity(g)))
    }
  }
  
  names(results) <- c('p','eA','eL','ed','eD','kA','kL','kd','kD')
  
  # group by edge rewiring probability and compute mean entropies
  results <- results %>%
    group_by(p) %>%
    summarise_all(funs(mean))
  
  if (transition.to.random) { 
    file.name <- 'results.100.50.ws.er.csv' 
  } else {
    file.name <- 'results.100.50.ws.ba.csv' 
  }
    
  write.csv(x = results, file = file.name, row.names = FALSE)

}