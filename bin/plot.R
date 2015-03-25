
# Code for generate plots in Chapters 2 and 3

# ---- init ----
# Install devtools and use devtools to install schedulr from GitHub
# install.packages(devtools)
# devtools::install_github('niranjv/schedulr', ref='develop')

# load schedulr & setup training set data
library(schedulr)

data(m3xlarge.runtimes.expdist)
data.env = setup.trainingset.runtimes('m3xlarge', m3xlarge.runtimes.expdist)
rt <- get('m3xlarge.runtimes', envir=data.env)
rts <- get('m3xlarge.runtimes.summary', envir=data.env)


# ---- bootstrap-95pct-ub-1processor-1task ----

# establishing that we have a good method to get 95% CI for job runtime
# using either bootstrap re-sampling or Normal approx. via CLT
# Need a plot like the one in ucsc/MS/2014-aug/test_jobs/1-pred-ind.sum.png

num.trials <- 100
num.tasks <- 1
num.bootstrap.reps <- 1000

# 1st col = actual job runtime
# 2nd col = .95 quantile of job runtime dist.
result <- matrix(nrow=num.trials, ncol=2)

for(i in 1:num.trials) {
  idx <- sample(1:NROW(m3xlarge.runtimes.expdist), num.tasks)
  s <- m3xlarge.runtimes.expdist[idx,]

  # get actual job runtime & assignment
  if (NCOL(s) == 1) {
    result[i,1] <- s[2]
    a <- get.initial.assignment(1, s[1], rts)
  } else {
    result[i,1] <- sum(s[,2])
    a <- get.initial.assignment(1, s[,1], rts)
  }

  # get 0.95 quantile of job runtime; deadline and score don't matter
  score <- get.score(a, rt, rts, 100)
  result[i,2] <- attr(score, 'runtime95pct')

} # end for - loop over all trials


result <- result[order(result[,2]),]
result <- cbind(1:num.trials, result)
colnames(result) <- c('idx', 'actual.job.runtime', 'q.95pct')


outliers.idx <- result[,2] > result[,3]
outliers <- result[outliers.idx,]

imgTitle <- paste('95% upper bound for job runtimes via Bootstrap re-sampling
(1 task/job; ', NROW(result), ' trials; ', NROW(outliers), ' outliers)', sep='')
plot(result[,1], result[,2], ylim=range(result), xlab='Trial #',
  ylab='Runtime (s)', main=imgTitle)
lines(result[,1], result[,3], lty='dotted', col='red', lwd=2)
if (is.matrix(outliers)) {
  points(outliers[,1], outliers[,2], pch=16, col='red')
} else {
  points(outliers[1], outliers[2], pch=16, col='red')
} # end if - outliers is a matrix?




# ---- bootstrap-95pct-ub-1processor-10tasks ----

num.trials <- 100
num.tasks <- 10
num.bootstrap.reps <- 1000

result <- matrix(nrow=num.trials, ncol=2)

for(i in 1:num.trials) {
  idx <- sample(1:NROW(m3xlarge.runtimes.expdist), num.tasks)
  s <- m3xlarge.runtimes.expdist[idx,]

  # get actual job runtime & assignment
  if (NCOL(s) == 1) {
    result[i,1] <- s[2]
    a <- get.initial.assignment(1, s[1], rts)
  } else {
    result[i,1] <- sum(s[,2])
    a <- get.initial.assignment(1, s[,1], rts)
  }

  # 0.95 quantile of job runtime dist.
  score <- get.score(a, rt, rts, 100) # deadline does not matter
  result[i,2] <- attr(score, 'runtime95pct')

} # end for - loop over all trials


result <- result[order(result[,2]),]
result <- cbind(1:num.trials, result)
colnames(result) <- c('idx', 'actual.job.runtime', 'q.95pct')


outliers.idx <- result[,2] > result[,3]
outliers <- result[outliers.idx,]

imgTitle <- paste('95% upper bound for job runtimes via Bootstrap re-sampling
(', num.tasks, ' tasks/job; ', NROW(result), ' trials; ', NROW(outliers), ' outliers)', sep='')
plot(result[,1], result[,2], ylim=range(result), xlab='Trial #',
  ylab='Runtime (s)', main=imgTitle)
lines(result[,1], result[,3], lty='dotted', col='red', lwd=2)
points(outliers[,1], outliers[,2], pch=16, col='red')


# ---- bootstrap-95pct-ub-1processor-50tasks ----

num.trials <- 100
num.tasks <- 50
num.bootstrap.reps <- 1000

result <- matrix(nrow=num.trials, ncol=2)

for(i in 1:num.trials) {
  idx <- sample(1:NROW(m3xlarge.runtimes.expdist), num.tasks)
  s <- m3xlarge.runtimes.expdist[idx,]

  # get actual job runtime & assignment
  if (NCOL(s) == 1) {
    result[i,1] <- s[2]
    a <- get.initial.assignment(1, s[1], rts)
  } else {
    result[i,1] <- sum(s[,2])
    a <- get.initial.assignment(1, s[,1], rts)
  }

  # 0.95 quantile of job runtime dist.
  score <- get.score(a, rt, rts, 100) # deadline does not matter
  result[i,2] <- attr(score, 'runtime95pct')

} # end for - loop over all trials


result <- result[order(result[,2]),]
result <- cbind(1:num.trials, result)
colnames(result) <- c('idx', 'actual.job.runtime', 'q.95pct')


outliers.idx <- result[,2] > result[,3]
outliers <- result[outliers.idx,]

imgTitle <- paste('95% upper bound for job runtimes via Bootstrap re-sampling
(', num.tasks, ' tasks/job; ', NROW(result), ' trials; ', NROW(outliers), ' outliers)', sep='')
plot(result[,1], result[,2], ylim=range(result), xlab='Trial #',
  ylab='Runtime (s)', main=imgTitle)
lines(result[,1], result[,3], lty='dotted', col='red', lwd=2)
points(outliers[,1], outliers[,2], pch=16, col='red')


# ---- bootstrap-95pct-ub-1processor-100tasks ----

num.trials <- 100
num.tasks <- 100
num.bootstrap.reps <- 1000

result <- matrix(nrow=num.trials, ncol=2)

for(i in 1:num.trials) {
  idx <- sample(1:NROW(m3xlarge.runtimes.expdist), num.tasks)
  s <- m3xlarge.runtimes.expdist[idx,]

  # get actual job runtime & assignment
  if (NCOL(s) == 1) {
    result[i,1] <- s[2]
    a <- get.initial.assignment(1, s[1], rts)
  } else {
    result[i,1] <- sum(s[,2])
    a <- get.initial.assignment(1, s[,1], rts)
  }

  # 0.95 quantile of job runtime dist.
  score <- get.score(a, rt, rts, 100) # deadline does not matter
  result[i,2] <- attr(score, 'runtime95pct')

} # end for - loop over all trials


result <- result[order(result[,2]),]
result <- cbind(1:num.trials, result)
colnames(result) <- c('idx', 'actual.job.runtime', 'q.95pct')


outliers.idx <- result[,2] > result[,3]
outliers <- result[outliers.idx,]

imgTitle <- paste('95% upper bound for job runtimes via Normal dist. approx.
(', num.tasks, ' tasks/job; ', NROW(result), ' trials; ', NROW(outliers), ' outliers)', sep='')
plot(result[,1], result[,2], ylim=range(result), xlab='Trial #',
  ylab='Runtime (s)', main=imgTitle)
lines(result[,1], result[,3], lty='dotted', col='red', lwd=2)
points(outliers[,1], outliers[,2], pch=16, col='red')


# ---- bootstrap-95pct-ub-1processor-150tasks ----

num.trials <- 100
num.tasks <- 150
num.bootstrap.reps <- 1000

result <- matrix(nrow=num.trials, ncol=2)

for(i in 1:num.trials) {
  idx <- sample(1:NROW(m3xlarge.runtimes.expdist), num.tasks)
  s <- m3xlarge.runtimes.expdist[idx,]

  # get actual job runtime & assignment
  if (NCOL(s) == 1) {
    result[i,1] <- s[2]
    a <- get.initial.assignment(1, s[1], rts)
  } else {
    result[i,1] <- sum(s[,2])
    a <- get.initial.assignment(1, s[,1], rts)
  }

  # 0.95 quantile of job runtime dist.
  score <- get.score(a, rt, rts, 100) # deadline does not matter
  result[i,2] <- attr(score, 'runtime95pct')

} # end for - loop over all trials


result <- result[order(result[,2]),]
result <- cbind(1:num.trials, result)
colnames(result) <- c('idx', 'actual.job.runtime', 'q.95pct')


outliers.idx <- result[,2] > result[,3]
outliers <- result[outliers.idx,]

imgTitle <- paste('95% upper bound for job runtimes via Normal dist. approx.
(', num.tasks, ' tasks/job; ', NROW(result), ' trials; ', NROW(outliers), ' outliers)', sep='')
plot(result[,1], result[,2], ylim=range(result), xlab='Trial #',
  ylab='Runtime (s)', main=imgTitle)
lines(result[,1], result[,3], lty='dotted', col='red', lwd=2)
points(outliers[,1], outliers[,2], pch=16, col='red')


# ---- bootstrap-95pct-ub-1processor-200tasks ----

num.trials <- 100
num.tasks <- 200
num.bootstrap.reps <- 1000

result <- matrix(nrow=num.trials, ncol=2)

for(i in 1:num.trials) {
  idx <- sample(1:NROW(m3xlarge.runtimes.expdist), num.tasks)
  s <- m3xlarge.runtimes.expdist[idx,]

  # get actual job runtime & assignment
  if (NCOL(s) == 1) {
    result[i,1] <- s[2]
    a <- get.initial.assignment(1, s[1], rts)
  } else {
    result[i,1] <- sum(s[,2])
    a <- get.initial.assignment(1, s[,1], rts)
  }

  # 0.95 quantile of job runtime dist.
  score <- get.score(a, rt, rts, 100) # deadline does not matter
  result[i,2] <- attr(score, 'runtime95pct')

} # end for - loop over all trials


result <- result[order(result[,2]),]
result <- cbind(1:num.trials, result)
colnames(result) <- c('idx', 'actual.job.runtime', 'q.95pct')


outliers.idx <- result[,2] > result[,3]
outliers <- result[outliers.idx,]

imgTitle <- paste('95% upper bound for job runtimes via Normal dist. approx.
(', num.tasks, ' tasks/job; ', NROW(result), ' trials; ', NROW(outliers), ' outliers)', sep='')
plot(result[,1], result[,2], ylim=range(result), xlab='Trial #',
  ylab='Runtime (s)', main=imgTitle)
lines(result[,1], result[,3], lty='dotted', col='red', lwd=2)
points(outliers[,1], outliers[,2], pch=16, col='red')



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
