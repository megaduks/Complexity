library(dplyr)
library(reshape2)
library(igraph)
library(acss)

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

results <- data.frame()

for (erp in edge.rewiring.probability) {
  for (i in 1:num.graphs) {
    g <- watts.strogatz.game(dim = 1, size = num.vertices, p = erp, nei = 2)
    #g <- to_powerlaw(dim = 1, size = num.vertices, p = erp, nei = 2)
    results <- rbind(results, c(erp, compute_complexity(g)))
  }
}

names(results) <- c('p','eA','eL','ed','eD','kA','kL','kd','kD')

# group by edge rewiring probability and compute mean entropies
results <- results %>%
  group_by(p) %>%
  summarise_all(funs(mean))

results <- as.data.frame(apply(results, 2, normalize))

print(results)

results_long <- melt(results, id = 'p')

plt <- ggplot(results_long, aes(x = p, y = value, color = variable)) + geom_line()
plt


