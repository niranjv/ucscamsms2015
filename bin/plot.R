
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


# init vars

# assume 3 instance types with:
# A slower than B slower than C
# A cheaper than B cheaper than C
instance.types <- letters[1:3]

cost.a <- 1 # dollars/hour
cost <- c(cost.a, 1.1*cost.a, 1.3*cost.a)


# define functions

create.validation.data <- function(runtimes.dist) {

    num.tasks <- NROW(runtimes.dist)

    actual.runtimes <- matrix(nrow=num.tasks, ncol=3)
    colnames(actual.runtimes) <- paste('runtimes.', instance.types, sep='')

    means <- matrix(nrow=num.tasks, ncol=3)
    vars <- matrix(nrow=num.tasks, ncol=3)
    dist.params <- list()

    for(i in 1:length(runtimes.dist)) {

      # Discrete Uniform
      if (runtimes.dist[i] == 'unif') {

        means[i,1] <- (1+i)/2
        means[i,2] <- means[i,1] - 0.07 * means[i,1]
        means[i,3] <- means[i,1] - 0.26 * means[i,1]

        actual.runtimes[i,1] <- runif(1, 1, i)
        actual.runtimes[i,2] <- actual.runtimes[i,1] - 0.07 * means[i,1]
        actual.runtimes[i,3] <- actual.runtimes[i,1] - 0.26 * means[i,1]

        vars[i,1] <- ((i-1)^2)/12
        vars[i,2] <- vars[i,1]
        vars[i,3] <- vars[i,1]

        dist.params[[i]] <- list('dist'='unif', 'i'=i)
      }

      # Gamma
      if (runtimes.dist[i] == 'gamma') {

        means[i,1 ] <- i*(2/i)
        means[i,2] <- means[i,1] - 0.07 * means[i,1]
        means[i,3] <- means[i,1] - 0.26 * means[i,1]

        actual.runtimes[i,1] <- rgamma(1, shape=i, scale=2/i)
        actual.runtimes[i,2] <- actual.runtimes[i,1] - 0.07 * means[i,1]
        actual.runtimes[i,3] <- actual.runtimes[i,1] - 0.26 * means[i,1]

        vars[i] <- i*(2/i)^2
        vars[i,2] <- vars[i,1]
        vars[i,3] <- vars[i,1]

        dist.params[[i]] <- list('dist'='gamma', 'shape'=i, 'scale'=2/i)
      }


      # Poisson
      if (runtimes.dist[i] == 'poisson') {

        means[i,1] <- i
        means[i,2] <- means[i,1] - 0.07 * means[i,1]
        means[i,3] <- means[i,1] - 0.26 * means[i,1]

        actual.runtimes[i,1] <- rpois(1, i)
        actual.runtimes[i,2] <- actual.runtimes[i,1] - 0.07 * means[i,1]
        actual.runtimes[i,3] <- actual.runtimes[i,1] - 0.26 * means[i,1]

        vars[i,1] <- i
        vars[i,2] <- vars[i,1]
        vars[i,3] <- vars[i,1]

        dist.params[[i]] <- list('dist'='poisson', 'i'=i)
      }

      # Exponential
      if (runtimes.dist[i] == 'exp') {

        means[i,1] <- 1/i
        means[i,2] <- means[i,1] - 0.07 * means[i,1]
        means[i,3] <- means[i,1] - 0.26 * means[i,1]

        actual.runtimes[i,1] <- rexp(1, i)
        actual.runtimes[i,2] <- actual.runtimes[i,1] - 0.07 * means[i,1]
        actual.runtimes[i,3] <- actual.runtimes[i,1] - 0.26 * means[i,1]

        vars[i] <- 1/(i^2)
        vars[i,2] <- vars[i,1]
        vars[i,3] <- vars[i,1]

        dist.params[[i]] <- list('dist'='exp', 'i'=i)
      }
    }

    return(list(
      'actual.runtimes' = actual.runtimes,
      'means' = means,
      'vars' = vars,
      'dist.params' = dist.params
    ))
} # end function - create.validation.data

get.schedule.deterministic.runtimes <-
function(instance.types, cost, benefit, deadline, runtimes) {

  makespan <- ceiling(apply(runtimes, 2, sum))
  makespan.feasible <- makespan[makespan <= deadline]
  cost.feasible <- cost[makespan <= deadline]
  instance.types.feasible <- instance.types[makespan <= deadline]

  util.feasible <- benefit - (cost.feasible * makespan.feasible)
  max.util <- max(util.feasible)
  max.util.instance.type <- instance.types.feasible[which.max(util.feasible)]
  max.util.makespan <- makespan.feasible[which.max(util.feasible)]

  return(list(
    'max.util' = max.util,
    'max.util.instance.type' = max.util.instance.type,
    'max.util.makespan' = max.util.makespan
  ))

} # end function - get.schedule.deterministic.runtimes


get.schedule.stochastic.runtimes <-
function(instance.types, cost, benefit, deadline, means, vars, threshold.pct) {

  stopifnot(dim(means) == dim(vars))

  means.sum <- apply(means, 2, sum)
  vars.sum <- apply(vars, 2, sum)
  sds <- sqrt(vars.sum)

  makespan.a.pct <- qnorm(threshold.pct, means.sum[1], sds[1])
  makespan.b.pct <- qnorm(threshold.pct, means.sum[2], sds[2])
  makespan.c.pct <- qnorm(threshold.pct, means.sum[3], sds[3])

  makespan.pct <- ceiling(c(
    makespan.a.pct,
    makespan.b.pct,
    makespan.c.pct
  ))

  makespan.feasible <- makespan.pct[makespan.pct <= deadline]
  cost.feasible <- cost[makespan.pct <= deadline]
  instance.types.feasible <- instance.types[makespan.pct <= deadline]

  util.feasible <- benefit - (cost.feasible * makespan.feasible)
  max.util <- max(util.feasible)
  max.idx <- which.max(util.feasible)

  max.util.instance.type <- instance.types.feasible[max.idx]
  max.util.makespan.threshold.pct <- makespan.feasible[max.idx]

  max.util.makespan.mean <- means.sum[max.idx]
  max.util.makespan.sd <- sds[max.idx]
  max.util.makespan.lo <- max.util.makespan.mean - 1.96 * max.util.makespan.sd
  max.util.makespan.hi <- max.util.makespan.mean + 1.96 * max.util.makespan.sd

  return(list(
    'max.util' = max.util,
    'max.util.instance.type' = max.util.instance.type,
    'max.util.makespan.threshold.pct' = max.util.makespan.threshold.pct,
    'max.util.makespan.mean' = max.util.makespan.mean,
    'max.util.makespan.lo' = max.util.makespan.lo,
    'max.util.makespan.hi' = max.util.makespan.hi
  ))

} # end function - get.schedule.stochastic.runtimes


get.schedule.stochastic.runtimes.bootstrap <-
function(instance.types, cost, benefit, deadline, dist.params, bootstrap.reps, threshold.pct) {

  num.tasks <- length(dist.params)

  makespan.bootstrap.dist <- matrix(nrow=bootstrap.reps, ncol=length(instance.types))

  for (i in 1:bootstrap.reps) {

    cur.dist <- matrix(nrow=num.tasks, ncol=3)

    for (j in 1:num.tasks) {

      dist.type <- dist.params[[j]]$dist

      # Discrete Uniform
      if (dist.type == 'unif') {

        b <- dist.params[[j]]$i

        cur.dist[j,1] <- runif(1, 1, b)
        cur.dist[j,2] <- cur.dist[j,1] - 0.07 * cur.dist[j,1]
        cur.dist[j,3] <- cur.dist[j,1] - 0.26 * cur.dist[j,1]
      }

      # Gamma
      if (dist.type == 'gamma') {

        shape = dist.params[[j]]$shape
        scale = dist.params[[j]]$scale

        cur.dist[j,1] <- rgamma(1, shape=shape, scale=scale)
        cur.dist[j,2] <- cur.dist[j,1] - 0.07 * cur.dist[j,1]
        cur.dist[j,3] <- cur.dist[j,1] - 0.26 * cur.dist[j,1]
      }

      # Poisson
      if (dist.type == 'poisson') {

        b <- dist.params[[j]]$i

        cur.dist[j,1] <- rpois(1, b)
        cur.dist[j,2] <- cur.dist[j,1] - 0.07 * cur.dist[j,1]
        cur.dist[j,3] <- cur.dist[j,1] - 0.26 * cur.dist[j,1]

      }

      # Exponential
      if (dist.type == 'exp') {

        b <- dist.params[[j]]$i

        cur.dist[j,1] <- rexp(1, b)
        cur.dist[j,2] <- cur.dist[j,1] - 0.07 * cur.dist[j,1]
        cur.dist[j,3] <- cur.dist[j,1] - 0.26 * cur.dist[j,1]
      }

    } # loop over dist. for all tasks

    makespan.bootstrap.dist[i, 1] <- sum(cur.dist[,1])
    makespan.bootstrap.dist[i, 2] <- sum(cur.dist[,2])
    makespan.bootstrap.dist[i, 3] <- sum(cur.dist[,3])

  } # end for - loop over all bootstrap reps


  means <- apply(makespan.bootstrap.dist, 2, mean)

  makespan.a.pct <- quantile(makespan.bootstrap.dist[,1], prob=threshold.pct)
  makespan.b.pct <- quantile(makespan.bootstrap.dist[,2], prob=threshold.pct)
  makespan.c.pct <- quantile(makespan.bootstrap.dist[,3], prob=threshold.pct)

  makespan.pct <- ceiling(c(
    makespan.a.pct,
    makespan.b.pct,
    makespan.c.pct
  ))

    makespan.feasible <- makespan.pct[makespan.pct <= deadline]
    cost.feasible <- cost[makespan.pct <= deadline]
    instance.types.feasible <- instance.types[makespan.pct <= deadline]

    util.feasible <- benefit - (cost.feasible * makespan.feasible)
    max.util <- max(util.feasible)
    max.idx <- which.max(util.feasible)

    max.util.instance.type <- instance.types.feasible[max.idx]
    max.util.makespan.threshold.pct <- makespan.feasible[max.idx]
    max.util.makespan.dist <- makespan.bootstrap.dist[,max.idx]

    max.util.makespan.mean <- means[max.idx]
    max.util.makespan.lo <- quantile(max.util.makespan.dist, prob=0.025)
    max.util.makespan.hi <- quantile(max.util.makespan.dist, prob=0.975)

  return(list(
    'max.util' = max.util,
    'max.util.instance.type' = max.util.instance.type,
    'max.util.makespan.threshold.pct' = max.util.makespan.threshold.pct,
    'max.util.makespan.mean' = max.util.makespan.mean,
    'max.util.makespan.lo' = max.util.makespan.lo,
    'max.util.makespan.hi' = max.util.makespan.hi
  ))

} # end function - get.schedule.stochastic.runtimes.bootstrap


# ---- 1instance-deterministic-runtimes ----

benefit <- 30 # in dollars
deadline <- 25 # in hours
runtimes <- matrix(nrow=5, ncol=3)
runtimes[,1] = c(5.2, 8.5, 2.5, 4.1, 6) # in hours
runtimes[,2] = 0.93 * runtimes[,1]
runtimes[,3] = 0.74 * runtimes[,1]

result = get.schedule.deterministic.runtimes(instance.types, cost, benefit, deadline, runtimes)

cat ('Instance type "', result$max.util.instance.type, '" has the maximum utility of $', result$max.util, ' with a makespan of ', result$max.util.makespan, ' hrs\n', sep='')



# ---- 1instance-stochastic-runtimes-known-dist-100tasks ----

num.iter <- 250
num.tasks <- 100
num.match <- 0
num.mismatch <- 0
optimal <- data.frame()


for (j in 1:num.iter) {

  # cat('Processing iteration', j, '\n')

  dist <- c('unif', 'poisson', 'gamma', 'exp')
  runtimes.dist <- sample(dist, num.tasks, replace=T)
  validation.data <- create.validation.data(runtimes.dist)

  # a realistic benefit & deadline or no schedule will be found
  deadline = round(4 * sum(validation.data$actual.runtimes[,1]))
  benefit = deadline

  result.actual = get.schedule.deterministic.runtimes(instance.types, cost, benefit, deadline, validation.data$actual.runtimes)
  result.actual

  result.predicted = get.schedule.stochastic.runtimes(instance.types, cost, benefit, deadline, validation.data$means, validation.data$vars, 0.95)
  result.predicted

  if (result.actual$max.util.instance.type == result.predicted$max.util.instance.type) {
    num.match <- num.match + 1
  } else {
    num.mismatch <- num.mismatch + 1
  }

  optimal[j, 1] <- result.actual$max.util
  optimal[j, 2] <- result.actual$max.util.instance.type
  optimal[j, 3] <- result.actual$max.util.makespan

  optimal[j, 4] <- result.predicted$max.util
  optimal[j, 5] <- result.predicted$max.util.instance.type
  optimal[j, 6] <- result.predicted$max.util.makespan.mean
  optimal[j, 7] <- result.predicted$max.util.makespan.lo
  optimal[j, 8] <- result.predicted$max.util.makespan.hi
}

optimal[,2] <- as.factor(optimal[,2])
optimal[,5] <- as.factor(optimal[,5])


optimal <- optimal[order(optimal[,7]),]
optimal <- cbind(1:NROW(optimal), optimal)
colnames(optimal) <- c('index', 'actual.util', 'actual.inst', 'actual.makespan',
'pred.util', 'pred.inst', 'pred.makespan.mean', 'pred.makespan.lo', 'pred.makespan.hi')

y.min <- round(0.9*min(optimal[,c(4,8,9)]))
y.max <- round(1.1*max(optimal[,c(4,8,9)]))


outliers.idx <- optimal[,4] < optimal[,8] | optimal[,4] > optimal[,9]
outliers <- optimal[outliers.idx,]
outliers.pct <- round(100*NROW(outliers)/NROW(optimal),2)

imgTitle <- paste('95% CI for makespan via Normal approx.
(', NROW(optimal), ' trials; ', num.tasks, ' tasks/trial; ',
outliers.pct, '% outliers)', sep='')


plot(optimal[,1], optimal[,4], pch=16, cex=0.6, main=imgTitle,
  ylim=c(y.min, y.max), xlab='Trial #', ylab='Makespan (hr)')
lines(optimal[,1], optimal[,8], col='red', lty='dotted', lwd=2)
lines(optimal[,1], optimal[,9], col='red', lty='dotted', lwd=2)

legend('bottomright', legend='95% CI for predicted makespan', col='red', lty='dotted', lwd=2)


# ---- 1instance-stochastic-runtimes-known-dist-250tasks ----

num.iter <- 250
num.tasks <- 250
num.match <- 0
num.mismatch <- 0
optimal <- data.frame()


for (j in 1:num.iter) {

  # cat('Processing iteration', j, '\n')

  dist <- c('unif', 'poisson', 'gamma', 'exp')
  runtimes.dist <- sample(dist, num.tasks, replace=T)
  validation.data <- create.validation.data(runtimes.dist)

  # a realistic benefit & deadline or no schedule will be found
  deadline = round(4 * sum(validation.data$actual.runtimes[,1]))
  benefit = deadline

  result.actual = get.schedule.deterministic.runtimes(instance.types, cost, benefit, deadline, validation.data$actual.runtimes)
  result.actual

  result.predicted = get.schedule.stochastic.runtimes(instance.types, cost, benefit, deadline, validation.data$means, validation.data$vars, 0.95)
  result.predicted

  if (result.actual$max.util.instance.type == result.predicted$max.util.instance.type) {
    num.match <- num.match + 1
  } else {
    num.mismatch <- num.mismatch + 1
  }

  optimal[j, 1] <- result.actual$max.util
  optimal[j, 2] <- result.actual$max.util.instance.type
  optimal[j, 3] <- result.actual$max.util.makespan

  optimal[j, 4] <- result.predicted$max.util
  optimal[j, 5] <- result.predicted$max.util.instance.type
  optimal[j, 6] <- result.predicted$max.util.makespan.mean
  optimal[j, 7] <- result.predicted$max.util.makespan.lo
  optimal[j, 8] <- result.predicted$max.util.makespan.hi
}

optimal[,2] <- as.factor(optimal[,2])
optimal[,5] <- as.factor(optimal[,5])


optimal <- optimal[order(optimal[,7]),]
optimal <- cbind(1:NROW(optimal), optimal)
colnames(optimal) <- c('index', 'actual.util', 'actual.inst', 'actual.makespan',
'pred.util', 'pred.inst', 'pred.makespan.mean', 'pred.makespan.lo', 'pred.makespan.hi')

y.min <- round(0.9*min(optimal[,c(4,8,9)]))
y.max <- round(1.1*max(optimal[,c(4,8,9)]))


outliers.idx <- optimal[,4] < optimal[,8] | optimal[,4] > optimal[,9]
outliers <- optimal[outliers.idx,]
outliers.pct <- round(100*NROW(outliers)/NROW(optimal),2)

imgTitle <- paste('95% CI for makespan via Normal approx.
(', NROW(optimal), ' trials; ', num.tasks, ' tasks/trial; ',
outliers.pct, '% outliers)', sep='')


plot(optimal[,1], optimal[,4], pch=16, cex=0.6, main=imgTitle,
  ylim=c(y.min, y.max), xlab='Trial #', ylab='Makespan (hr)')
lines(optimal[,1], optimal[,8], col='red', lty='dotted', lwd=2)
lines(optimal[,1], optimal[,9], col='red', lty='dotted', lwd=2)

legend('bottomright', legend='95% CI for predicted makespan', col='red', lty='dotted', lwd=2)



# ---- 1instance-stochastic-runtimes-known-dist-500tasks ----

num.iter <- 250
num.tasks <- 500
num.match <- 0
num.mismatch <- 0
optimal <- data.frame()


for (j in 1:num.iter) {

  # cat('Processing iteration', j, '\n')

  dist <- c('unif', 'poisson', 'gamma', 'exp')
  runtimes.dist <- sample(dist, num.tasks, replace=T)
  validation.data <- create.validation.data(runtimes.dist)

  # a realistic benefit & deadline or no schedule will be found
  deadline = round(4 * sum(validation.data$actual.runtimes[,1]))
  benefit = deadline

  result.actual = get.schedule.deterministic.runtimes(instance.types, cost, benefit, deadline, validation.data$actual.runtimes)
  result.actual

  result.predicted = get.schedule.stochastic.runtimes(instance.types, cost, benefit, deadline, validation.data$means, validation.data$vars, 0.95)
  result.predicted

  if (result.actual$max.util.instance.type == result.predicted$max.util.instance.type) {
    num.match <- num.match + 1
  } else {
    num.mismatch <- num.mismatch + 1
  }

  optimal[j, 1] <- result.actual$max.util
  optimal[j, 2] <- result.actual$max.util.instance.type
  optimal[j, 3] <- result.actual$max.util.makespan

  optimal[j, 4] <- result.predicted$max.util
  optimal[j, 5] <- result.predicted$max.util.instance.type
  optimal[j, 6] <- result.predicted$max.util.makespan.mean
  optimal[j, 7] <- result.predicted$max.util.makespan.lo
  optimal[j, 8] <- result.predicted$max.util.makespan.hi
}

optimal[,2] <- as.factor(optimal[,2])
optimal[,5] <- as.factor(optimal[,5])


optimal <- optimal[order(optimal[,7]),]
optimal <- cbind(1:NROW(optimal), optimal)
colnames(optimal) <- c('index', 'actual.util', 'actual.inst', 'actual.makespan',
'pred.util', 'pred.inst', 'pred.makespan.mean', 'pred.makespan.lo', 'pred.makespan.hi')

y.min <- round(0.9*min(optimal[,c(4,8,9)]))
y.max <- round(1.1*max(optimal[,c(4,8,9)]))


outliers.idx <- optimal[,4] < optimal[,8] | optimal[,4] > optimal[,9]
outliers <- optimal[outliers.idx,]
outliers.pct <- round(100*NROW(outliers)/NROW(optimal),2)

imgTitle <- paste('95% CI for makespan via Normal approx.
(', NROW(optimal), ' trials; ', num.tasks, ' tasks/trial; ',
outliers.pct, '% outliers)', sep='')


plot(optimal[,1], optimal[,4], pch=16, cex=0.6, main=imgTitle,
  ylim=c(y.min, y.max), xlab='Trial #', ylab='Makespan (hr)')
lines(optimal[,1], optimal[,8], col='red', lty='dotted', lwd=2)
lines(optimal[,1], optimal[,9], col='red', lty='dotted', lwd=2)

legend('bottomright', legend='95% CI for predicted makespan', col='red', lty='dotted', lwd=2)



# ---- 1instance-stochastic-runtimes-known-dist-1000tasks ----

num.iter <- 250
num.tasks <- 1000
num.match <- 0
num.mismatch <- 0
optimal <- data.frame()


for (j in 1:num.iter) {

  # cat('Processing iteration', j, '\n')

  dist <- c('unif', 'poisson', 'gamma', 'exp')
  runtimes.dist <- sample(dist, num.tasks, replace=T)
  validation.data <- create.validation.data(runtimes.dist)

  # a realistic benefit & deadline or no schedule will be found
  deadline = round(4 * sum(validation.data$actual.runtimes[,1]))
  benefit = deadline

  result.actual = get.schedule.deterministic.runtimes(instance.types, cost, benefit, deadline, validation.data$actual.runtimes)
  result.actual

  result.predicted = get.schedule.stochastic.runtimes(instance.types, cost, benefit, deadline, validation.data$means, validation.data$vars, 0.95)
  result.predicted

  if (result.actual$max.util.instance.type == result.predicted$max.util.instance.type) {
    num.match <- num.match + 1
  } else {
    num.mismatch <- num.mismatch + 1
  }

  optimal[j, 1] <- result.actual$max.util
  optimal[j, 2] <- result.actual$max.util.instance.type
  optimal[j, 3] <- result.actual$max.util.makespan

  optimal[j, 4] <- result.predicted$max.util
  optimal[j, 5] <- result.predicted$max.util.instance.type
  optimal[j, 6] <- result.predicted$max.util.makespan.mean
  optimal[j, 7] <- result.predicted$max.util.makespan.lo
  optimal[j, 8] <- result.predicted$max.util.makespan.hi
}

optimal[,2] <- as.factor(optimal[,2])
optimal[,5] <- as.factor(optimal[,5])


optimal <- optimal[order(optimal[,7]),]
optimal <- cbind(1:NROW(optimal), optimal)
colnames(optimal) <- c('index', 'actual.util', 'actual.inst', 'actual.makespan',
'pred.util', 'pred.inst', 'pred.makespan.mean', 'pred.makespan.lo', 'pred.makespan.hi')

y.min <- round(0.9*min(optimal[,c(4,8,9)]))
y.max <- round(1.1*max(optimal[,c(4,8,9)]))


outliers.idx <- optimal[,4] < optimal[,8] | optimal[,4] > optimal[,9]
outliers <- optimal[outliers.idx,]
outliers.pct <- round(100*NROW(outliers)/NROW(optimal),2)

imgTitle <- paste('95% CI for makespan via Normal approx.
(', NROW(optimal), ' trials; ', num.tasks, ' tasks/trial; ',
outliers.pct, '% outliers)', sep='')


plot(optimal[,1], optimal[,4], pch=16, cex=0.6, main=imgTitle,
  ylim=c(y.min, y.max), xlab='Trial #', ylab='Makespan (hr)')
lines(optimal[,1], optimal[,8], col='red', lty='dotted', lwd=2)
lines(optimal[,1], optimal[,9], col='red', lty='dotted', lwd=2)

legend('bottomright', legend='95% CI for predicted makespan', col='red', lty='dotted', lwd=2)


# ---- 1instance-stochastic-runtimes-known-dist-10tasks ----

num.iter <- 250
num.tasks <- 10
num.match <- 0
num.mismatch <- 0
optimal <- data.frame()

for (j in 1:num.iter) {

  # cat('Processing iteration', j, '\n')

  dist <- c('unif', 'poisson', 'gamma', 'exp')
  runtimes.dist <- sample(dist, num.tasks, replace=T)
  validation.data <- create.validation.data(runtimes.dist)

  # a realistic benefit & deadline or no schedule will be found
  deadline = round(5 * sum(validation.data$actual.runtimes[,1]))
  benefit = deadline

  result.actual = get.schedule.deterministic.runtimes(instance.types, cost, benefit, deadline, validation.data$actual.runtimes)

  result.predicted = get.schedule.stochastic.runtimes.bootstrap(instance.types, cost, benefit, deadline, validation.data$dist.params, 1000, 0.95)

  if (result.actual$max.util.instance.type == result.predicted$max.util.instance.type) {
    num.match <- num.match + 1
  } else {
    num.mismatch <- num.mismatch + 1
  }

  optimal[j, 1] <- result.actual$max.util
  optimal[j, 2] <- result.actual$max.util.instance.type
  optimal[j, 3] <- result.actual$max.util.makespan

  optimal[j, 4] <- result.predicted$max.util
  optimal[j, 5] <- result.predicted$max.util.instance.type
  optimal[j, 6] <- result.predicted$max.util.makespan.mean
  optimal[j, 7] <- result.predicted$max.util.makespan.lo
  optimal[j, 8] <- result.predicted$max.util.makespan.hi
}


optimal[,2] <- as.factor(optimal[,2])
optimal[,5] <- as.factor(optimal[,5])

optimal <- optimal[order(optimal[,7]),]
optimal <- cbind(1:NROW(optimal), optimal)
colnames(optimal) <- c('index', 'actual.util', 'actual.inst', 'actual.makespan',
'pred.util', 'pred.inst', 'pred.makespan.mean', 'pred.makespan.lo', 'pred.makespan.hi')

y.min <- round(0.9*min(optimal[,c(4,8,9)]))
y.max <- round(1.1*max(optimal[,c(4,8,9)]))

outliers.idx <- optimal[,4] < optimal[,8] | optimal[,4] > optimal[,9]
outliers <- optimal[outliers.idx,]
outliers.pct <- round(100*NROW(outliers)/NROW(optimal),2)

imgTitle <- paste('95% CI for makespan via bootstrap approx.
(', NROW(optimal), ' trials; ', num.tasks, ' tasks/trial; ',
outliers.pct, '% outliers)', sep='')


plot(optimal[,1], optimal[,4], pch=16, cex=0.6, main=imgTitle,
  ylim=c(y.min, y.max), xlab='Trial #', ylab='Makespan (hr)')
lines(optimal[,1], optimal[,8], col='red', lty='dotted', lwd=2)
lines(optimal[,1], optimal[,9], col='red', lty='dotted', lwd=2)

legend('bottomright', legend='95% CI for predicted makespan', col='red', lty='dotted', lwd=2)



# ---- 1instance-stochastic-runtimes-known-dist-25tasks ----

num.iter <- 250
num.tasks <- 25
num.match <- 0
num.mismatch <- 0
optimal <- data.frame()

for (j in 1:num.iter) {

  # cat('Processing iteration', j, '\n')

  dist <- c('unif', 'poisson', 'gamma', 'exp')
  runtimes.dist <- sample(dist, num.tasks, replace=T)
  validation.data <- create.validation.data(runtimes.dist)

  # a realistic benefit & deadline or no schedule will be found
  deadline = round(5 * sum(validation.data$actual.runtimes[,1]))
  benefit = deadline

  result.actual = get.schedule.deterministic.runtimes(instance.types, cost, benefit, deadline, validation.data$actual.runtimes)

  result.predicted = get.schedule.stochastic.runtimes.bootstrap(instance.types, cost, benefit, deadline, validation.data$dist.params, 1000, 0.95)

  if (result.actual$max.util.instance.type == result.predicted$max.util.instance.type) {
    num.match <- num.match + 1
  } else {
    num.mismatch <- num.mismatch + 1
  }

  optimal[j, 1] <- result.actual$max.util
  optimal[j, 2] <- result.actual$max.util.instance.type
  optimal[j, 3] <- result.actual$max.util.makespan

  optimal[j, 4] <- result.predicted$max.util
  optimal[j, 5] <- result.predicted$max.util.instance.type
  optimal[j, 6] <- result.predicted$max.util.makespan.mean
  optimal[j, 7] <- result.predicted$max.util.makespan.lo
  optimal[j, 8] <- result.predicted$max.util.makespan.hi
}


optimal[,2] <- as.factor(optimal[,2])
optimal[,5] <- as.factor(optimal[,5])

optimal <- optimal[order(optimal[,7]),]
optimal <- cbind(1:NROW(optimal), optimal)
colnames(optimal) <- c('index', 'actual.util', 'actual.inst', 'actual.makespan',
'pred.util', 'pred.inst', 'pred.makespan.mean', 'pred.makespan.lo', 'pred.makespan.hi')

y.min <- round(0.9*min(optimal[,c(4,8,9)]))
y.max <- round(1.1*max(optimal[,c(4,8,9)]))

outliers.idx <- optimal[,4] < optimal[,8] | optimal[,4] > optimal[,9]
outliers <- optimal[outliers.idx,]
outliers.pct <- round(100*NROW(outliers)/NROW(optimal),2)

imgTitle <- paste('95% CI for makespan via bootstrap approx.
(', NROW(optimal), ' trials; ', num.tasks, ' tasks/trial; ',
outliers.pct, '% outliers)', sep='')


plot(optimal[,1], optimal[,4], pch=16, cex=0.6, main=imgTitle,
  ylim=c(y.min, y.max), xlab='Trial #', ylab='Makespan (hr)')
lines(optimal[,1], optimal[,8], col='red', lty='dotted', lwd=2)
lines(optimal[,1], optimal[,9], col='red', lty='dotted', lwd=2)

legend('bottomright', legend='95% CI for predicted makespan', col='red', lty='dotted', lwd=2)



# ---- 1instance-stochastic-runtimes-known-dist-50tasks ----

num.iter <- 250
num.tasks <- 50
num.match <- 0
num.mismatch <- 0
optimal <- data.frame()

for (j in 1:num.iter) {

  # cat('Processing iteration', j, '\n')

  dist <- c('unif', 'poisson', 'gamma', 'exp')
  runtimes.dist <- sample(dist, num.tasks, replace=T)
  validation.data <- create.validation.data(runtimes.dist)

  # a realistic benefit & deadline or no schedule will be found
  deadline = round(5 * sum(validation.data$actual.runtimes[,1]))
  benefit = deadline

  result.actual = get.schedule.deterministic.runtimes(instance.types, cost, benefit, deadline, validation.data$actual.runtimes)

  result.predicted = get.schedule.stochastic.runtimes.bootstrap(instance.types, cost, benefit, deadline, validation.data$dist.params, 1000, 0.95)

  if (result.actual$max.util.instance.type == result.predicted$max.util.instance.type) {
    num.match <- num.match + 1
  } else {
    num.mismatch <- num.mismatch + 1
  }

  optimal[j, 1] <- result.actual$max.util
  optimal[j, 2] <- result.actual$max.util.instance.type
  optimal[j, 3] <- result.actual$max.util.makespan

  optimal[j, 4] <- result.predicted$max.util
  optimal[j, 5] <- result.predicted$max.util.instance.type
  optimal[j, 6] <- result.predicted$max.util.makespan.mean
  optimal[j, 7] <- result.predicted$max.util.makespan.lo
  optimal[j, 8] <- result.predicted$max.util.makespan.hi
}


optimal[,2] <- as.factor(optimal[,2])
optimal[,5] <- as.factor(optimal[,5])

optimal <- optimal[order(optimal[,7]),]
optimal <- cbind(1:NROW(optimal), optimal)
colnames(optimal) <- c('index', 'actual.util', 'actual.inst', 'actual.makespan',
'pred.util', 'pred.inst', 'pred.makespan.mean', 'pred.makespan.lo', 'pred.makespan.hi')

y.min <- round(0.9*min(optimal[,c(4,8,9)]))
y.max <- round(1.1*max(optimal[,c(4,8,9)]))

outliers.idx <- optimal[,4] < optimal[,8] | optimal[,4] > optimal[,9]
outliers <- optimal[outliers.idx,]
outliers.pct <- round(100*NROW(outliers)/NROW(optimal),2)

imgTitle <- paste('95% CI for makespan via bootstrap approx.
(', NROW(optimal), ' trials; ', num.tasks, ' tasks/trial; ',
outliers.pct, '% outliers)', sep='')


plot(optimal[,1], optimal[,4], pch=16, cex=0.6, main=imgTitle,
  ylim=c(y.min, y.max), xlab='Trial #', ylab='Makespan (hr)')
lines(optimal[,1], optimal[,8], col='red', lty='dotted', lwd=2)
lines(optimal[,1], optimal[,9], col='red', lty='dotted', lwd=2)

legend('bottomright', legend='95% CI for predicted makespan', col='red', lty='dotted', lwd=2)



# ---- 1instance-stochastic-runtimes-known-dist-75tasks ----

num.iter <- 250
num.tasks <- 75
num.match <- 0
num.mismatch <- 0
optimal <- data.frame()

for (j in 1:num.iter) {

  # cat('Processing iteration', j, '\n')

  dist <- c('unif', 'poisson', 'gamma', 'exp')
  runtimes.dist <- sample(dist, num.tasks, replace=T)
  validation.data <- create.validation.data(runtimes.dist)

  # a realistic benefit & deadline or no schedule will be found
  deadline = round(5 * sum(validation.data$actual.runtimes[,1]))
  benefit = deadline

  result.actual = get.schedule.deterministic.runtimes(instance.types, cost, benefit, deadline, validation.data$actual.runtimes)

  result.predicted = get.schedule.stochastic.runtimes.bootstrap(instance.types, cost, benefit, deadline, validation.data$dist.params, 1000, 0.95)

  if (result.actual$max.util.instance.type == result.predicted$max.util.instance.type) {
    num.match <- num.match + 1
  } else {
    num.mismatch <- num.mismatch + 1
  }

  optimal[j, 1] <- result.actual$max.util
  optimal[j, 2] <- result.actual$max.util.instance.type
  optimal[j, 3] <- result.actual$max.util.makespan

  optimal[j, 4] <- result.predicted$max.util
  optimal[j, 5] <- result.predicted$max.util.instance.type
  optimal[j, 6] <- result.predicted$max.util.makespan.mean
  optimal[j, 7] <- result.predicted$max.util.makespan.lo
  optimal[j, 8] <- result.predicted$max.util.makespan.hi
}


optimal[,2] <- as.factor(optimal[,2])
optimal[,5] <- as.factor(optimal[,5])

optimal <- optimal[order(optimal[,7]),]
optimal <- cbind(1:NROW(optimal), optimal)
colnames(optimal) <- c('index', 'actual.util', 'actual.inst', 'actual.makespan',
'pred.util', 'pred.inst', 'pred.makespan.mean', 'pred.makespan.lo', 'pred.makespan.hi')

y.min <- round(0.9*min(optimal[,c(4,8,9)]))
y.max <- round(1.1*max(optimal[,c(4,8,9)]))

outliers.idx <- optimal[,4] < optimal[,8] | optimal[,4] > optimal[,9]
outliers <- optimal[outliers.idx,]
outliers.pct <- round(100*NROW(outliers)/NROW(optimal),2)

imgTitle <- paste('95% CI for makespan via bootstrap approx.
(', NROW(optimal), ' trials; ', num.tasks, ' tasks/trial; ',
outliers.pct, '% outliers)', sep='')


plot(optimal[,1], optimal[,4], pch=16, cex=0.6, main=imgTitle,
  ylim=c(y.min, y.max), xlab='Trial #', ylab='Makespan (hr)')
lines(optimal[,1], optimal[,8], col='red', lty='dotted', lwd=2)
lines(optimal[,1], optimal[,9], col='red', lty='dotted', lwd=2)

legend('bottomright', legend='95% CI for predicted makespan', col='red', lty='dotted', lwd=2)




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


# ---- sched-113task-4inst ----
job <- c(1,2,2,2,3,3,4,4,5,10,10,10,15,15,15,25,25,25,25,25,30,35,35,35,40,40,45,50,50,50,55,65,70,75,75,80,85,85,95,95,95,100,110,115,125,130,135,150,155,155,155,155,155,165,170,170,175,180,185,185,190,190,195,220,220,230,230,250,250,275,300,300,300,300,325,325,350,350,375,400,450,450,500,550,550,650,650,800,900,900,900,950,1000,1300,1300,1300,1500,1600,1700,1700,1700,1800,1900,1900,1900,2000,2200,2300,2500,2600,2700,2800,2900)
deadline <- 5000
cluster.instance.type <- 'm3xlarge'
cluster.size <- 4
max.iter <- 100
max.temp <- 0.5
reset.score.pct <- 10

best.schedule <- schedule(job, deadline, cluster.instance.type, cluster.size,
max.iter, max.temp, reset.score.pct, debug=TRUE)
scores.ts <- attr(best.schedule, 'scores.ts')

imgTitle <- 'Score of accepted assignments in 100 SA iterations \n (113 tasks on 4 processors)'
plot(scores.ts[,1], scores.ts[,2], type='l', lwd=2, ylim=c(0,1), xlab='Iteration',
ylab='Score', main=imgTitle)
lines(scores.ts[,1], scores.ts[,5], type='l', lwd=2, col='#e41a1c')
grid()
