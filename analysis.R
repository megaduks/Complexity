library(dplyr)
library(reshape2)
library(ggplot2)

# read text file with results
results <- read.csv(file = 'results.100.50.ws.er.csv')
# transform the data into the long format
results_long <- melt(results, id = 'p')

# add a variable for faceting
results_long$type <- ''

results_long[results_long$variable == 'eA' | results_long$variable == 'kA',]$type <- 'adjacency matrix'
results_long[results_long$variable == 'eL' | results_long$variable == 'kL',]$type <- 'laplacian'
results_long[results_long$variable == 'ed' | results_long$variable == 'kd',]$type <- 'degree list'
results_long[results_long$variable == 'eD' | results_long$variable == 'kD',]$type <- 'degree distribution'

# factorize the variable to have a more readable labels in the legend
results_long$variable <- factor(results_long$variable, levels = c('eA','eL','ed','eD','kA','kL','kd','kD'), 
                                labels = c('entropy of adjacency matrix','entropy of laplacian','entropy of degree list','entropy of degree distribution',
                                           'complexity of adjacency matrix','complexity of laplacian','complexity of degree list','complexity of degree distribution'))

# generate the plot
plt <- ggplot(results_long, aes(x = p, y = value, color = variable)) + geom_line() + facet_wrap(~ type, ncol = 2)
plt

# save the plot
ggsave(filename = 'results.100.50.ws.er.png')
