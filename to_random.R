# 
# Function to gradually transform a small world graph into a random network
#
# author: Mikołaj Morzy
# date: 8.03.2017
#
# The purpose of this function is to slowly change a small world graph
# into a random network. The procedure is as follows:
#
# TODO
#

to_random <- function(graph, p) {
  
  g <- graph
  edge.list <- get.edgelist(g)
  
  for (i in 1:nrow(edge.list)) {
    if (runif(1) < p) {
      # find the source vertex of the edge
      from.vertex <- edge.list[i,1]
      to.vertex <- edge.list[i,2]
      
      # remove the edge from the graph
      g <- g - edge(paste(from.vertex,"|",to.vertex))
      
      # select new target vertex
      new.vertex <- as.numeric(sample(V(g), 1))
      
      # add new edge to the graph
      g <- g + edge(from.vertex, new.vertex)
    }
  }
  
  return(g)
}