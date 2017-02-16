library(igraph)
library(dplyr)
library(reshape2)
library(acss)
library(ggplot2)
library(ggnet)

source('compute_complexity.R')
source('to_powerlaw.R')

normalize <- function(v) {
  if ((max(v) - min(v) == 0))
    return(1)
  else
    return((v-min(v))/(max(v)-min(v)))
}

# size of input graphs
num.vertices <- 100

# number of graphs sampled for each value of the edge rewiring probability
num.graphs <- 50

# possible values of edge rewiring probability
edge.rewiring.probability <- 0:num.vertices/num.vertices

# flag to mark whether the transition is from small world to random network
transition.to.random <- TRUE

results <- data.frame()

for (erp in edge.rewiring.probability) {
  for (i in 1:num.graphs) {
    if (transition.to.random) {
      g <- watts.strogatz.game(dim = 1, size = num.vertices, p = erp, nei = 2)
    } else {
      g <- to_powerlaw(dim = 1, size = num.vertices, p = erp, nei = 2)
    }
    
    results <- rbind(results, c(erp, compute_complexity(g)))
  }
}

names(results) <- c('p','eA','eL','ed','eD','kA','kL','kd','kD')

# group by edge rewiring probability and compute mean entropies
results <- results %>%
  group_by(p) %>%
  summarise_all(funs(mean))

results <- as.data.frame(apply(results, 2, normalize))

if (transition.to.random) { 
  file.name <- 'results.100.50.ws.er.csv' 
} else {
  file.name <- 'results.100.50.ws.b.csv' 
}
  
write.csv(x = results, file = file.name, row.names = FALSE)



