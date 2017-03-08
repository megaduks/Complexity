library(igraph)
library(acss)
library(ggplot2)
library(reshape2)

library(intergraph)
library(network)

num.graphs <- 50

models <- c('random.network','small.world','preferential.attachment')

results <- data.frame(id = numeric(), a = numeric(), b = numeric(), c = numeric(), d = numeric(), e = numeric(), f = numeric(), m = character())

for (model in models) {
  
  for (i in 1:num.graphs) {
    
    switch(model,
           random.network          = { g <- erdos.renyi.game(n = runif(1,90,110), p.or.m = runif(1,0.01, 0.05)) },
           small.world             = { g <- watts.strogatz.game(dim = 1, size = runif(1, 90, 110), nei = 4, p = runif(1, 0.01, 0.02)) },
           preferential.attachment = { g <- barabasi.game(n = runif(1, 90, 110), power = runif(1, 1, 2), 2) }
    )
    
    am <- as_adjacency_matrix(g, sparse = FALSE)
    im <- as.matrix(asNetwork(g), matrix.type = 'incidence')
    lap <- laplacian_matrix(g, sparse = FALSE)
    
    s_am <- paste(am, collapse = '')
    s_im <- paste(t(im), collapse = '')
    s_lap <- paste(lap, collapse = '')
    
    res <- data.frame(i, entropy(s_am)[[1]], entropy(s_im)[[1]], entropy(s_lap)[[1]], 
             mean(unlist(local_complexity(s_am, span = 10)), na.rm = TRUE),
             mean(unlist(local_complexity(s_im, span = 10)), na.rm = TRUE),
             mean(unlist(local_complexity(s_lap, span = 10)), na.rm = TRUE),
             model
             )
    
    results <- rbind(results, res)
  }
}

max_string <- paste(letters[runif(10000,1,10)], collapse = '')
min_string <- paste(rep('a', 10000), collapse = '')

min_complexity <- mean(unlist(local_complexity(min_string, span = 10)), na.rm = TRUE)
max_complexity <- mean(unlist(local_complexity(max_string, span = 10)), na.rm = TRUE)
min_entropy <- entropy(min_string)
max_entropy <- entropy(max_string)

results[, c(2,3,4)] <- (results[, c(2,3,4)] - min_entropy) / (max_entropy - min_entropy)
results[, c(5,6,7)] <- (results[, c(5,6,7)] - min_complexity) / (max_complexity - min_complexity)

results <- cbind(results, rowMeans(results[,2:4]), rowMeans(results[,5:7]))

colnames(results) <- c('id','e_am','e_im','e_lap','k_am','k_im','k_lap','model','avg_e','avg_k')

plot <- ggplot(melt(results[,c(1,8,9,10)], id = c('id','model')), aes(x = id, y = value, color = variable, linetype = variable)) + geom_smooth(se = FALSE) + facet_wrap(~ model, ncol = 1)
plot

plot <- ggplot(melt(results[,c(1:8)], id = c('id','model')), aes(x = id, y = value, color = variable, linetype = variable)) + geom_smooth(se = FALSE) + facet_wrap(~ model, ncol = 1)
plot
