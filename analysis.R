library(dplyr)
library(reshape2)
library(ggplot2)


# read text file with results
results <- read.csv(file = 'results.100.5.ws.er.csv')
results <- read.csv(file = 'results.100.50.ws.ba.csv')

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
plt <- ggplot(results_long[results_long$type == 'adjacency matrix',], aes(x = p, y = value, color = variable, linetype = variable)) + geom_line() + facet_wrap(~ type, ncol = 2) + theme_bw()
plt
plt <- ggplot(results_long[results_long$type == 'Laplacian matrix',], aes(x = p, y = value, color = variable, linetype = variable)) + geom_line() + facet_wrap(~ type, ncol = 2) + theme_bw()
plt
plt <- ggplot(results_long[results_long$type == 'degree list',], aes(x = p, y = value, color = variable, linetype = variable)) + geom_line() + facet_wrap(~ type, ncol = 2) + theme_bw()
plt
plt <- ggplot(results_long[results_long$type == 'degree distribution',], aes(x = p, y = value, color = variable, linetype = variable)) + geom_line() + facet_wrap(~ type, ncol = 2) + theme_bw()
plt

# generate the plot of the average entropy and complexity across network features
# results <- cbind(results, eMean = rowMeans(results[,2:5]), KMean = rowMeans(results[,6:9]))

# plt <- ggplot(melt(results[,c(1,10,11)], id = 'p'), aes(x = p, y = value, color = variable, linetype = variable)) + geom_smooth(alpha=0.2) + theme_bw()
# plt

# save the plot
#ggsave(filename = 'results.100.100.ws.er.png')
#ggsave(filename = 'results.100.100.ws.ba.png')
