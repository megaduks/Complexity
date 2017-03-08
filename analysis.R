library(dplyr)
library(reshape2)
library(ggplot2)

# read text file with results
results <- read.csv(file = 'results.100.5.ws.er.csv')
results <- read.csv(file = 'results.100.5.ws.b.csv')

# normalize the results by comparing them with maximal entropy/complexity of strings of similar length
max_string <- paste(letters[runif(10000,1,10)], collapse = '')
min_string <- paste(rep('a', 10000), collapse = '')

min_complexity <- mean(unlist(local_complexity(min_string, span = 10)), na.rm = TRUE)
max_complexity <- mean(unlist(local_complexity(max_string, span = 10)), na.rm = TRUE)
min_entropy <- entropy(min_string)
max_entropy <- entropy(max_string)

results[, c(2,3,4,5)] <- (results[, c(2,3,4,5)] - min_entropy) / (max_entropy - min_entropy)
results[, c(6,7,8,9)] <- (results[, c(6,7,8,9)] - min_complexity) / (max_complexity - min_complexity)


# transform the data into the long format
results_long <- melt(results, id = 'p')

# add a variable for faceting
results_long$type <- ''

results_long[results_long$variable == 'eA' | results_long$variable == 'kA',]$type <- 'adjacency matrix'
results_long[results_long$variable == 'eL' | results_long$variable == 'kL',]$type <- 'Laplacian matrix'
results_long[results_long$variable == 'ed' | results_long$variable == 'kd',]$type <- 'degree list'
results_long[results_long$variable == 'eD' | results_long$variable == 'kD',]$type <- 'degree distribution'

# add a class variable to distinguish between entropy and complexity
results_long$class <- ''

results_long[results_long$variable == 'eA',]$class <- 'entropy'
results_long[results_long$variable == 'eL',]$class <- 'entropy'
results_long[results_long$variable == 'ed',]$class <- 'entropy'
results_long[results_long$variable == 'eD',]$class <- 'entropy'
results_long[results_long$variable == 'kA',]$class <- 'K-complexity'
results_long[results_long$variable == 'kL',]$class <- 'K-complexity'
results_long[results_long$variable == 'kd',]$class <- 'K-complexity'
results_long[results_long$variable == 'kD',]$class <- 'K-complexity'

# factorize the variable to have a more readable labels in the legend
results_long$variable <- factor(results_long$variable, levels = c('eA','eL','ed','eD','kA','kL','kd','kD'), 
                                labels = c('entropy of adjacency matrix','entropy of Laplacian matrix','entropy of degree list','entropy of degree distribution',
                                           'K-complexity of adjacency matrix','K-complexity of Laplacian matrix','K-complexity of degree list','K-complexity of degree distribution'))

# generate the 2D plot factorized by the type of network measure
plt <- ggplot(results_long, aes(x = p, y = value, color = variable, linetype = variable)) + geom_line() + facet_wrap(~ type, ncol = 2) + theme_bw()
plt

# generate the plot of the average entropy and complexity across network features
results <- cbind(results, eMean = rowMeans(results[,2:5]), KMean = rowMeans(results[,6:9]))

plt <- ggplot(melt(results[,c(1,10,11)], id = 'p'), aes(x = p, y = value, color = variable, linetype = variable)) + geom_smooth(alpha=0.2) + theme_bw()
plt

# save the plot
#ggsave(filename = 'results.100.100.ws.er.png')
#ggsave(filename = 'results.100.100.ws.ba.png')
