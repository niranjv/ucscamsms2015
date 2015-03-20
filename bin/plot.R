
# Code for generate plots in Chapters 2 and 3

# ---- init ----
# install.packages(devtools)
# devtools::install_github('niranjv/schedulr', ref='develop')

# load schedulr & setup training set data
library(schedulr)

data(m3xlarge.runtimes.expdist)
setup.trainingset.runtimes('m3xlarge', m3xlarge.runtimes.expdist)


# ---- sched-1task-1inst ----
job <- c(1)
deadline <- 300
cluster.instance.type <- 'm3xlarge'
cluster.size <- 1
max.iter <- 10
max.temp <- 0.5
reset.score.pct <- 10

best.schedule <- schedule(job, deadline, cluster.instance.type, cluster.size,
max.iter, max.temp, reset.score.pct, debug=TRUE)
scores.ts <- attr(best.schedule, 'scores.ts')

imgTitle <- 'Probability of completing job by deadline'
plot(scores.ts[,1], scores.ts[,2], type='l', ylim=c(0,1), xlab='Iteration',
ylab='Score', main=imgTitle)
lines(scores.ts[,1], scores.ts[,5], type='l', col='#e41a1c')
grid()


# ---- sched-3task-2inst ----
job <- c(1,60,100)
deadline <- 300
cluster.instance.type <- 'm3xlarge'
cluster.size <- 2
max.iter <- 10
max.temp <- 0.5
reset.score.pct <- 10

best.schedule <- schedule(job, deadline, cluster.instance.type, cluster.size,
max.iter, max.temp, reset.score.pct, debug=TRUE)
scores.ts <- attr(best.schedule, 'scores.ts')

imgTitle <- 'Probability of completing job by deadline'
plot(scores.ts[,1], scores.ts[,2], type='l', ylim=c(0,1), xlab='Iteration',
ylab='Score', main=imgTitle)
lines(scores.ts[,1], scores.ts[,5], type='l', col='#e41a1c')
grid()
