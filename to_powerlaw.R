# 
# Function to gradually transform a small world graph into a powerlaw graph
#
# author: Mikołaj Morzy
# date: 11.02.2017
#
# The purpose of this function is to slowly change a perfectly ordered small world graph
# into a powerlaw graph. The procedure is as follows:
#
# We start with a given small world graph with no edge rewiring
# Next, for each edge we check if it is rewired, but when rewiring the edge
# we select the destination node proportionally to the current degree of all
# nodes in the graph
#

to_powerlaw <- function(dim, size, p, nei) {
  
  g <- watts.strogatz.game(dim = dim, size = size, p = 0, nei = nei)
  edge.list <- get.edgelist(g)
  
  for (i in 1:nrow(edge.list)) {
    if (runif(1) < p) {
      # find the source vertex of the edge
      from.vertex <- edge.list[i,1]
      to.vertex <- edge.list[i,2]
      
      # remove the edge from the graph
      g <- g - edge(paste(from.vertex,"|",to.vertex))
      
      # select new target vertex
      new.vertex <- as.numeric(sample(V(g), 1, prob = degree(g, normalized = TRUE)))
      
      # add new edge to the graph
      g <- g + edge(from.vertex, new.vertex)
    }
  }
  
  
  return(g)
}