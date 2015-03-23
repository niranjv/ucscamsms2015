
# Code for generate plots in Chapters 2 and 3

# ---- init ----
# Install devtools and use devtools to install schedulr from GitHub
# install.packages(devtools)
# devtools::install_github('niranjv/schedulr', ref='develop')

# load schedulr & setup training set data
library(schedulr)

data(m3xlarge.runtimes.expdist)
setup.trainingset.runtimes('m3xlarge', m3xlarge.runtimes.expdist)


# ---- bootstrap-95pct-ci ----

# establishing that we have a good method to get 95% CI for job runtime
# using either bootstrap re-sampling or Normal approx. via CLT
# Need a plot like the one in ucsc/MS/2014-aug/test_jobs/1-pred-ind.sum.png
get.95pct.ci <- function(s) {
  mydf <- data.frame(size=s[1], reps=1)
  boot.dist <- bootstrap.get.job.runtime.dist(mydf, 1000, m3xlarge.runtimes.expdist)
  ci.95pct <- quantile(boot.dist, prob=c(0.025, 0.975))
  result <- ci.95pct

  return(result)
}

num.trials <- 100
idx <- sample(1:NROW(m3xlarge.runtimes.expdist), num.trials)
sz <- m3xlarge.runtimes.expdist[idx,]
result <- apply(sz, 1, get.95pct.ci)
result <- t(result)

result <- cbind(sz[,2], result)
colnames(result) <- c('actual.mean', 'lo.ci.95pct', 'hi.ci.95pct')
result <- result[order(result[,1]),]

plot(1:num.trials, result[,1], pch=16, ylim=range(result), xlab='Trial',
  ylab='Runtime (s)', main='95% CI for job runtimes \n (Bootstrap re-sampling)')
lines(1:num.trials, result[,2], lty='dotted', col='red', lwd=2)
lines(1:num.trials, result[,3], lty='dotted', col='red', lwd=2)



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
