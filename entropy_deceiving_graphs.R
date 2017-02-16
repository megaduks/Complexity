library(igraph)
library(acss)

num.vertices <- 21

g <- graph(c(1,2))

V(g)[1]$color <- 'red'
V(g)[2]$color <- 'blue'

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

g <- as.undirected(simplify(g))
plot(g, vertex.size = 5, vertex.label = NA, layout = layout.kamada.kawai)

b <- paste(betweenness(g), collapse = '')
lcb <- unname(local_complexity(b)[[1]])
mean(lcb)

leb <- unname(entropy(b)[[1]])
mean(leb)

d <- paste(degree(g), collapse = '')
lcd <- unname(local_complexity(d)[[1]])
mean(lcd)

led <- unname(entropy(d)[[1]])
mean(led)

A <- paste(as_adjacency_matrix(g), collapse = '')
lcA <- unname(local_complexity(A)[[1]])
mean(lcA)

leA <- unname(entropy(A)[[1]])
mean(leA)

L <- paste(laplacian_matrix(g), collapse = '')
lcL <- unname(local_complexity(L)[[1]])
mean(lcL)

leL <- unname(entropy(L)[[1]])
mean(leL)

num.vertices <- 21

g <- graph(c(1,2,1,3))

for (i in 4:num.vertices) {
  g <- add_vertices(g, 1)
  g <- add_edges(g, c(i,i-2))
  
  if (i%%2==0) 
    g <- add_edges(g, c(i,i-1))
}
#g <- add_edges(g, c(num.vertices-1,1))
g <- add_edges(g, c(num.vertices-1,2))

g <- as.undirected(g)
plot(g, vertex.size = 7, vertex.label = NA, layout = layout.kamada.kawai)

#table(degree(g))
#entropy(table(degree(g)))

#table(betweenness(g))
#entropy(betweenness(g))

b <- paste(betweenness(g), collapse = '')
lcb <- unname(local_complexity(b)[[1]])
mean(lcb)

d <- paste(degree(g), collapse = '')
lcd <- unname(local_complexity(d)[[1]])
mean(lcd)


n <- 20

g <- make_empty_graph(2*n, directed = FALSE)

for (i in 1:(2*n-1))
  g <- add_edges(g, c(i, i+1))

for (i in 3:n)
  for (j in 1:(i-2))
    g <- add_edges(g, c(i, n+j))

ggnet2(simplify(g), mode = "circle")
table(igraph::degree(g))

d <- igraph::degree(g)
entropy(paste(d, collapse = ''))
