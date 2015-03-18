
# Code for generate plots in Chapters 2 and 3

# Install devtools
# Use devtools to install schedulr from GitHub
install.packages(devtools)
devtools::install_github('niranjv/schedulr', ref='develop')

# load schedulr to use its functions
library(schedulr)


# Setup training set data
data(m3xlarge.runtimes.expdist)
setup.trainingset.runtimes('m3xlarge', m3xlarge.runtimes.expdist)



# Schedule 3 tasks on 1 instance
job <- c(1,60,100)
deadline <- 300
cluster.instance.type <- 'm3xlarge'
cluster.size <- 1
max.iter <- 100
max.temp <- 0.5
reset.score.pct <- 10

best.schedule <- schedule(job, deadline, cluster.instance.type, cluster.size,
max.iter, max.temp, reset.score.pct, debug=TRUE)



# Schedule 3 tasks on 2 instances
job <- c(1,60,100)
deadline <- 300
cluster.instance.type <- 'm3xlarge'
cluster.size <- 2
max.iter <- 2
max.temp <- 0.5
reset.score.pct <- 10

best.schedule <- schedule(job, deadline, cluster.instance.type, cluster.size,
max.iter, max.temp, reset.score.pct, debug=TRUE)
