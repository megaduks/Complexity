# 
# Function to gradually transform a small world graph into a powerlaw graph
#
# author: Mikołaj Morzy
# date: 11.02.2017
#
# The purpose of this function is to slowly change a small world graph
# into a powerlaw graph. The procedure is as follows:
#
# TODO
#

to_powerlaw <- function(graph, p) {
  
  g <- graph
  edge.list <- get.edgelist(g)
  # coefficient of the preferential attachment power law
  alph <- 2.5
  
  for (i in 1:nrow(edge.list)) {
    #if (runif(1) < p) {
    if ((!E(g)[i]$rewired) & runif(1) < p) {
      # find the source vertex of the edge
      from.vertex <- edge.list[i,1]
      to.vertex <- edge.list[i,2]
      
      # remove the edge from the graph
      g <- g - edge(paste(from.vertex,"|",to.vertex))
      
      # select new target vertex
      new.vertex <- as.numeric(sample(V(g), 1, prob = (degree(g, normalized = TRUE)^alph/sum(degree(g, normalized = TRUE)^alph))))
      
      # add new edge to the graph
      e <- edge(from.vertex, new.vertex)
      e$rewired <- TRUE
      g <- g + e
      #g <- g + edge(from.vertex, new.vertex)
    }
  }
  
  
  return(g)
}