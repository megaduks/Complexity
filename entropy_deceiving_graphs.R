##### 2-Clique graph

# number of vertices
num.vertices <- 21

# initialize the graph with a single pair
g <- graph(c(1,2), directed = FALSE)

V(g)[1]$color <- 'red'
V(g)[2]$color <- 'blue'

# add vertices alternatingly to the RED and the BLUE cliques
for (i in 3:num.vertices) {
  
  g <- add_vertices(g, 1)
  
  if (i%%2 == 0) {
    V(g)[i]$color <- 'blue'
    friends <- V(g)[V(g)$color == 'blue']
  }
  else {
    V(g)[i]$color <- 'red'
    friends <- V(g)[V(g)$color == 'red']
  }
    
  for (n in friends)
    g <- add_edges(g, c(i,n))
}

# remove multiple edges and loops
g <- simplify(g)

# plot the graph
ggnet2(g, size = "degree", alpha = 0.8, color = "color") + theme(legend.position = "none") 

###### Ladder graph

# number of vertices in the graph
num.vertices <- 21

# initialize the graph
g <- graph(c(1,2,1,3), directed = FALSE)

# add pairs of vertices and construct the ladder
for (i in 4:num.vertices) {
  g <- add_vertices(g, 1)
  g <- add_edges(g, c(i,i-2))
  
  if (i%%2==0) 
    g <- add_edges(g, c(i,i-1))
}
g <- add_edges(g, c(num.vertices-1,2))

# add an attribute to each vertex with its betweenness
b <- igraph::betweenness(g)
V(g)$betweenness <- igraph::betweenness(g)

# draw the final graph
ggnet2(simplify(g), size = "betweenness", alpha = 0.8, color = "maroon")

###### Degree sequence graph (by Adrian Szymczak)

# number of vertices in the graph
n <- 20

# create an initially empty graph with twice as many vertices as assumed
g <- make_empty_graph(2*n, directed = FALSE)

# create initial linked list of all vertices (a chain)
for (i in 1:(2*n-1))
  g <- add_edges(g, c(i, i+1))

# wire additional edges to create a network with each unique degree
# appearing exactly twice in the graph
for (i in 3:n)
  for (j in 1:(i-2))
    g <- add_edges(g, c(i, n+j))

# draw the graph
ggnet2(simplify(g), mode = "circle", size = "degree", alpha = 0.8, color = "lightsalmon")

###### Copeland-Erdos graph (concatenation of primes)

num.primes <- 1000

# generate the list of primes
list.of.primes <- Primes(1, num.primes)

# concatenate all digits into a single string
ce.constant <- substr(paste(list.of.primes, collapse = ''), 1, 400)

# split the string into individual digits
ce.list <- as.numeric(unlist(strsplit(ce.constant, split = '')))

# create an adjacency matrix from the string by computing (n div 5)
adjacency.matrix <- matrix(ce.list %/% 5, nrow = 20, ncol = 20)

# remove loops for the sake of drawing the graph (ggnet2 cannot draw loops)
g <- simplify(graph_from_adjacency_matrix(adjacency.matrix, mode = 'undirected'))

# draw the graph
ggnet2(simplify(g), mode = "segeo", size = "degree", alpha = 0.8, color = "royalblue")

###### Block graph (concatenation of several transposed 3x3 matrices)

# create initial 3x3 matrix
m <- matrix(c(0,1,0,1,0,1,0,0,1), nrow = 3, ncol = 3)

l <- list()

# create several copies of the original and permuted matrix
for (i in 1:64) {
  if (runif(1) > 0.5) 
    l[[i]] <- m
  else
    l[[i]] <- t(m)
}

# combine individual matrices into a single square matrix
M <- m
for (i in 2:length(l))
  M <- cbind(M, l[[i]])
dim(M) <- c(24,24)

# create a graph based on the generated adjacency matrix
g <- simplify(graph_from_adjacency_matrix(M, mode = 'undirected'))

# draw the graph
ggnet2(simplify(g), mode = "spring", alpha = 0.8, color = "yellowgreen")
