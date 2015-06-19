
# Code for generate plots in Chapters 2 and 3


# ---- init ----
rm(list=ls())

# Install devtools and use devtools to install schedulr from GitHub to run
# simulated annealing code. This is done only once for each new version of
# schedulr
#
# install.packages("devtools")
# devtools::install_github('niranjv/schedulr', ref='develop')

# load schedulr & setup exponential runtimes dataset
library(schedulr)

data(m3xlarge.runtimes.expdist)
data.env = setup.trainingset.runtimes('m3xlarge', m3xlarge.runtimes.expdist)
rt <- get('m3xlarge.runtimes', envir=data.env)
rts <- get('m3xlarge.runtimes.summary', envir=data.env)


# init vars

# assume 3 instance types available
# e.g., (m3.large, c3.large, c4.large) in AWS EC2 with:
# processing speed = (3.25, 3.5, 4)
# processing cost = (0.14, 0.105, 0.116)
instance.types <- c('m3.large', 'c3.large', 'c4.large')
instance.speed <- c(3.25, 3.5, 4)
instance.costs <- c(0.14, 0.105, 0.116)


#' Create validation dataset
#'
#' Runtime for each tasks can have a different distribution
#' Input array has the name of the distribution of the runtime for each task
#'
#' @param runtimes.dist Array of strings representing the distribution of
#' runtimes. Currently, the distributions must be one of
#' ('unif', 'poisson', 'gamma', 'exp').
#' @return A list containing the details of the distribution of runtimes for
#' each task in the input set of tasks
create.validation.data <- function (runtimes.dist) {

    num.tasks <- NROW(runtimes.dist)
    num.instance.types = NROW(instance.types)

    simulated.runtimes <- matrix(nrow=num.tasks, ncol=num.instance.types)
    colnames(simulated.runtimes) <- paste('runtimes.', instance.types, sep='')

    means <- matrix(nrow=num.tasks, ncol=num.instance.types)
    vars <- matrix(nrow=num.tasks, ncol=num.instance.types)
    dist.params <- list()

    runtimes <- 1 + (1:length(runtimes.dist))/60 # baseline for params
    for(i in 1:length(runtimes.dist)) {

      param.a <- runtimes[i] * instance.speed[1]
      param.b <- runtimes[i] * instance.speed[2]
      param.c <- runtimes[i] * instance.speed[3]

      # Discrete Uniform
      if (runtimes.dist[i] == 'unif') {

        means[i,1] <- (1+param.a)/2
        means[i,2] <- (1+param.b)/2
        means[i,3] <- (1+param.c)/2

        simulated.runtimes[i,1] <- runif(1, 1, param.a)
        simulated.runtimes[i,2] <- runif(1, 1, param.b)
        simulated.runtimes[i,3] <- runif(1, 1, param.c)

        vars[i,1] <- ((param.a-1)^2)/12
        vars[i,2] <- ((param.b-1)^2)/12
        vars[i,3] <- ((param.c-1)^2)/12

        dist.params[[i]] <- list('dist'='unif', 'params'=c(param.a, param.b,
          param.c))
      }

      # Gamma
      if (runtimes.dist[i] == 'gamma') {

        means[i,1] <- param.a^2
        means[i,2] <- param.b^2
        means[i,3] <- param.c^2

        simulated.runtimes[i,1] <- rgamma(1, shape=param.a, scale=param.a)
        simulated.runtimes[i,2] <- rgamma(1, shape=param.b, scale=param.b)
        simulated.runtimes[i,3] <- rgamma(1, shape=param.c, scale=param.c)

        vars[i,1] <- param.a^3
        vars[i,2] <- param.b^3
        vars[i,3] <- param.c^3

        dist.params[[i]] <- list('dist'='gamma', 'params'=c(param.a, param.b,
          param.c))
      }

      # Poisson
      if (runtimes.dist[i] == 'poisson') {

        param.a <- round(param.a)
        param.b <- round(param.b)
        param.c <- round(param.c)

        means[i,1] <- param.a
        means[i,2] <- param.b
        means[i,3] <- param.c

        simulated.runtimes[i,1] <- rpois(1, param.a)
        simulated.runtimes[i,2] <- rpois(1, param.b)
        simulated.runtimes[i,3] <- rpois(1, param.c)

        vars[i,1] <- param.a
        vars[i,2] <- param.b
        vars[i,3] <- param.c

        dist.params[[i]] <- list('dist'='poisson', 'params'=c(param.a, param.b,
          param.c))
      }

      # Exponential
      if (runtimes.dist[i] == 'exp') {

        means[i,1] <- 1/param.a
        means[i,2] <- 1/param.b
        means[i,3] <- 1/param.c

        simulated.runtimes[i,1] <- rexp(1, param.a)
        simulated.runtimes[i,2] <- rexp(1, param.b)
        simulated.runtimes[i,3] <- rexp(1, param.c)

        vars[i,1] <- 1/(param.a^2)
        vars[i,2] <- 1/(param.b^2)
        vars[i,3] <- 1/(param.c^2)

        dist.params[[i]] <- list('dist'='exp', 'params'=c(param.a, param.b,
          param.c))
      }
    }

    return(list(
      'simulated.runtimes' = simulated.runtimes,
      'means' = means,
      'vars' = vars,
      'dist.params' = dist.params
    ))
    
} # end function - create.validation.data


#' Calculate makespan for deterministic runtimes
#'
#' For deterministic runtimes, load is the sum of runtimes of tasks assigned
#' to an instance and makespan is the max of load across all instances.
#'
#' @param instance.types Array of instance types under consideration
#' @param instance.costs Array of cost per hour for above instance types
#' @param benefit Benefit of completing all tasks by deadline
#' @param deadline Time by which all costs must be complete
#' @param runtimes Deterministic runtimes for all tasks for all instance types
#' @return A list containing the maximum utility and the instance type and
#' makespan associated with this utility
get.schedule.deterministic.runtimes <- function (
instance.types, instance.costs, benefit, deadline, runtimes) {

    makespan <- ceiling(apply(runtimes, 2, sum))
    makespan.feasible <- makespan[makespan <= deadline]
    instance.costs.feasible <- instance.costs[makespan <= deadline]
    instance.types.feasible <- instance.types[makespan <= deadline]

    util.feasible <- benefit - (instance.costs.feasible * makespan.feasible)
    max.util.idx <- which.max(util.feasible)
    max.util <- util.feasible[max.util.idx]
    max.util.instance.type <- instance.types.feasible[max.util.idx]
    max.util.makespan <- makespan.feasible[max.util.idx]

    return( list(
      'max.util' = max.util,
      'max.util.instance.type' = max.util.instance.type,
      'max.util.makespan' = max.util.makespan
    ))

} # end function - get.schedule.deterministic.runtimes


#' Calculate schedule for large number of tasks whose runtimes are distributed
#' according to known distributions
#'
#' @param instance.types Array of instance types under consideration
#' @param instance.costs Array of cost per hour for above instance types
#' @param benefit Benefit of completing all tasks by deadline
#' @param deadline Time by which all costs must be complete
#' @param means Array of means of runtime distributions
#' @param vars Array of variances of runtime distributions
#' @param feasible.pctl Percentile of makespan distribution. A schedule is 
#' feasible only if the deadline is greater than this percentile of the 
#' makespan distribution
#' @return A list containing the maximum utility, instance type and details of 
#' the makespan associated with this utility
get.schedule.stochastic.runtimes <- function (
  instance.types, instance.costs, benefit, deadline, means, vars,
  feasible.pctl) {

    stopifnot(dim(means) == dim(vars))

    means.sum <- apply(means, 2, sum)
    vars.sum <- apply(vars, 2, sum)
    sds <- sqrt(vars.sum)

    makespan.pctl <- qnorm(feasible.pctl, means.sum, sds)
    makespan.pctl <- ceiling(makespan.pctl)

    feasible.idx <- makespan.pctl <= deadline
    makespan.feasible <- makespan.pctl[feasible.idx]
    instance.cost.feasible <- instance.costs[feasible.idx]
    instance.types.feasible <- instance.types[feasible.idx]

    util.feasible <- benefit - (instance.cost.feasible * makespan.feasible)
    max.util <- max(util.feasible)
    max.util.idx <- which.max(util.feasible)

    max.util.instance.type <- instance.types.feasible[max.util.idx]
    max.util.makespan.feasible.pctl <- makespan.feasible[max.util.idx]

    # assuming we are summing over enough tasks that the makespn dist. is Normal
    max.util.makespan.mean <- means.sum[max.util.idx]
    max.util.makespan.sd <- sds[max.util.idx]
    max.util.makespan.lo <- max.util.makespan.mean - 1.96 * max.util.makespan.sd
    max.util.makespan.hi <- max.util.makespan.mean + 1.96 * max.util.makespan.sd

    return(list(
        'max.util' = max.util,
        'max.util.instance.type' = max.util.instance.type,
        'max.util.makespan.feasible.pctl' = max.util.makespan.feasible.pctl,
        'max.util.makespan.mean' = max.util.makespan.mean,
        'max.util.makespan.lo' = max.util.makespan.lo,
        'max.util.makespan.hi' = max.util.makespan.hi
    ))

} # end function - get.schedule.stochastic.runtimes


#' Get validation results when makespan distributions are Normally distributed
#'
#' @param instance.types Array of instance types under consideration
#' @param instance.costs Array of cost per hour for above instance types
#' @param feasible.pctl Percentile of makespan distribution. A schedule is 
#' feasible only if the deadline is greater than this percentile of the 
#' makespan distribution
#' @param num.tasks Number of input tasks
#' @param num.trials Number of simulated data sets to process
#' @return Nothing. This function generates a plot as a side-effect
validate.stochastic.runtimes <- function (
  instance.types, instance.costs, feasible.pctl=0.95,
  num.tasks, num.trials=1000) {

  validation.results <- data.frame()

  for (j in 1:num.trials) {

    # cat('Processing iteration', j, '\n')

    dist <- c('unif', 'poisson', 'gamma', 'exp')
    runtimes.dist <- sample(dist, num.tasks, replace=T)
    validation.data <- create.validation.data(runtimes.dist)

    # a realistic benefit & deadline or no schedule will be found
    deadline = round(4 * sum(validation.data$simulated.runtimes[,1]))
    benefit = deadline

    result.actual = get.schedule.deterministic.runtimes(
      instance.types, instance.costs, benefit, deadline,
      validation.data$simulated.runtimes
    )

    result.predicted = get.schedule.stochastic.runtimes(
      instance.types, instance.costs, benefit,
      deadline, validation.data$means,
      validation.data$vars, feasible.pctl
    )

    validation.results[j, 1] <- result.actual$max.util
    validation.results[j, 2] <- result.actual$max.util.instance.type
    validation.results[j, 3] <- result.actual$max.util.makespan

    validation.results[j, 4] <- result.predicted$max.util
    validation.results[j, 5] <- result.predicted$max.util.instance.type
    validation.results[j, 6] <- result.predicted$max.util.makespan.mean
    validation.results[j, 7] <- result.predicted$max.util.makespan.lo
    validation.results[j, 8] <- result.predicted$max.util.makespan.hi
  }

  validation.results <- validation.results[order(validation.results[,7]),]
  validation.results <- cbind(1:NROW(validation.results), validation.results)
  colnames(validation.results) <- c('index', 'actual.util', 'actual.inst',
  'actual.makespan', 'pred.util', 'pred.inst', 'pred.makespan.mean',
  'pred.makespan.lo', 'pred.makespan.hi')

  plot.validation.results(validation.results, num.tasks, num.trials)

}


get.runtime.dist.bootstrap <- function (
    num.instance.types, dist.params) {
    
    num.tasks <- length(dist.params)
    
    runtime.dist <- matrix(nrow=num.tasks,
      ncol=num.instance.types)

    for (j in 1:num.tasks) {
        dist.type <- dist.params[[j]]$dist
        params <- dist.params[[j]]$params
        stopifnot(num.instance.types <= length(params))

        # Discrete Uniform
        if (dist.type == 'unif') {
          runtime.dist[j,] <- runif(num.instance.types, min=1, max=params)
        }

        # Gamma
        if (dist.type == 'gamma') {
            runtime.dist[j,] <- rgamma(num.instance.types, shape=params, 
                scale=params)
        }

        # Poisson
        if (dist.type == 'poisson') {
            runtime.dist[j,] <- rpois(num.instance.types, lambda=params)
        }

        # Exponential
        if (dist.type == 'exp') {
            runtime.dist[j,] <- rexp(num.instance.types, rate=params)
        }

    } # loop over dist. for all tasks


    return (runtime.dist)
        
} # end function - get.runtime.dist.bootstrap


#' Get runtime dist via bootstrap resampling, then get schedule
#'
#' @param instance.types Array of instance types under consideration
#' @param instance.costs Array of cost per hour for above instance types
#' @param num.tasks Number of input tasks
#' @param num.trials Number of simulated data sets to process
#' @param num.bootstrap.reps Number of bootstrap samples to use while generating
#' runtime distribution
#' @param feasible.pctl Threshold to use to detemine makespan for runtime dist.
#' @return Nothing. This function generates a plot as a side-effect
get.schedule.stochastic.runtimes.bootstrap <- function (
  instance.types, instance.costs, benefit, deadline, dist.params,
  num.bootstrap.reps, feasible.pctl) {
    
    num.instance.types <- length(instance.types)
    
    makespan.bootstrap.dist <- 
        matrix(nrow=num.bootstrap.reps, ncol=num.instance.types)

    for (i in 1:num.bootstrap.reps) {
        
        if (1 %% 10 == 0) { cat('Processing trial', i, '\n') }
      
        runtime.dist <- get.runtime.dist.bootstrap(num.instance.types,
            dist.params)
        makespan.bootstrap.dist[i,] <- apply(runtime.dist, 2, sum)
    }

    makespan.pct <- 
        apply(makespan.bootstrap.dist, 2, quantile, prob=feasible.pctl)
    makespan.pct <- ceiling(makespan.pct)

    makespan.feasible <- makespan.pct[makespan.pct <= deadline]
    instance.cost.feasible <- instance.costs[makespan.pct <= deadline]
    instance.types.feasible <- instance.types[makespan.pct <= deadline]

    util.feasible <- benefit - (instance.cost.feasible * makespan.feasible)
    max.util.idx <- which.max(util.feasible)


    return(list(
    'makespan.feasible' = makespan.feasible,
    'util.feasible' = util.feasible,
    'max.util' = util.feasible[max.util.idx],
    'max.util.dist' = makespan.bootstrap.dist[,max.util.idx],
    'max.util.instance.type' = instance.types.feasible[max.util.idx],
    'max.util.makespan.feasible.pctl' = makespan.feasible[max.util.idx],
    'max.util.makespan.mean' = mean(makespan.bootstrap.dist[,max.util.idx]),
    'max.util.makespan.var' = var(makespan.bootstrap.dist[,max.util.idx]),
    'max.util.makespan.lo' = quantile(makespan.bootstrap.dist[,max.util.idx],
      prob=0.025),
    'max.util.makespan.hi' = quantile(makespan.bootstrap.dist[,max.util.idx],
      prob=0.975)
    ))

} # end function - get.schedule.stochastic.runtimes.bootstrap


#' Get validation results when runtime distributions are obtained via bootstrap
#' sampling
#'
#' @param instance.types Array of instance types under consideration
#' @param instance.costs Array of cost per hour for above instance types
#' @param num.tasks Number of input tasks
#' @param num.trials Number of simulated data sets to process
#' @param num.bootstrap.reps Number of bootstrap samples to use while generating
#' runtime distribution
#' @param feasible.pctl Threshold to use to detemine makespan for runtime dist.
#' @return Nothing. This function generates a plot as a side-effect
validate.stochastic.runtimes.bootstrap <- function (
    instance.types, instance.costs, num.tasks, num.trials=1000,
    num.bootstrap.reps=1000, feasible.pctl=0.95) {

    validation.results <- data.frame()

    for (j in 1:num.trials) {

        if (j %% 100 == 0) { cat('Processing trial', j, '\n') }

        dist.list <- c('unif', 'poisson', 'gamma', 'exp')
        runtimes.dist <- sample(dist.list, num.tasks, replace=T)
        runtimes.dist <- rep('poisson', num.tasks)
        validation.data <- create.validation.data(runtimes.dist)

        # a realistic benefit & deadline or no schedule will be found
        deadline = round(4 * sum(validation.data$simulated.runtimes[,1]))
        benefit = deadline

        # this is the makespan if the runtimes were deterministic
        result.actual = get.schedule.deterministic.runtimes(
          instance.types, instance.costs, benefit, deadline,
          validation.data$simulated.runtimes
        )

        result.predicted = get.schedule.stochastic.runtimes.bootstrap(
          instance.types, instance.costs, benefit, deadline,
          validation.data$dist.params, num.bootstrap.reps, feasible.pctl
        )

        validation.results[j,1] <- result.actual$max.util
        validation.results[j,2] <- result.actual$max.util.instance.type
        validation.results[j,3] <- result.actual$max.util.makespan

        validation.results[j,4] <- result.predicted$max.util
        validation.results[j,5] <- result.predicted$max.util.instance.type
        validation.results[j,6] <- result.predicted$max.util.makespan.mean
        validation.results[j,7] <- result.predicted$max.util.makespan.lo
        validation.results[j,8] <- result.predicted$max.util.makespan.hi
    }

    validation.results <- validation.results[order(validation.results[,7]),]
    validation.results <- cbind(1:NROW(validation.results), validation.results)
    colnames(validation.results) <- c('index', 'actual.util', 'actual.inst',
    'actual.makespan', 'pred.util', 'pred.inst', 'pred.makespan.mean',
    'pred.makespan.lo', 'pred.makespan.hi')

    plot.validation.results(validation.results, num.tasks, num.trials)

} # end function - validate.stochastic.runtimes.bootstrap


#' Plot validation results
#'
#' Plot actual runtimes with 95% CI for predicted runtimes
#'
#' @param validation.results Matrix containing the columns (
#'  Actual maximum utility,
#'  Actual instance with maximum utility,
#'  Actual makespan with maximum utility,
#'  Predicted maximum utility,
#'  Predicted instance type with maximum utility,
#'  Mean of predicted makespan with maximum utility,
#'  Lower bound of 95% CI of predicted makespan with maximum utility,
#'  Higher bound of 95% CI of predicted makespan with maximum utility,
#' )
#' @param img.title Title to use in image
#' @param img.filename Path to image file
#' @return Nothing. This function is called for the side-effect of generating
#' a plot in a file.
plot.validation.results <- function (
  validation.results, num.tasks, num.trials) {
      
    # We can compare runtimes only when both the actual and predicted schedules
    # use the same instance type
    validation.results.matched <-
    subset(validation.results, validation.results[,3] == validation.results[,6])

    outliers <- subset(validation.results.matched,
        validation.results.matched[,4] < validation.results.matched[,8] |
        validation.results.matched[,4] > validation.results.matched[,9]
    )
    
    outliers.pct <-
    round(100*NROW(outliers)/NROW(validation.results.matched),2)

    img.title <- paste('95% CI for makespan\n', sep='')
    img.title <- paste(img.title, NROW(validation.results), ' trials; ', 
        num.tasks, ' tasks/trial\n', sep='')
    img.title <- paste(img.title, outliers.pct, '% outliers in the ',
    round(100*NROW(validation.results.matched)/NROW(validation.results),2),
    '% of trials with matched instance types', sep='')

    img.filename <- paste('validate-stochastic-runtimes-',
    num.trials,'-trials-', num.tasks,'-tasks.eps', sep='')

    y.lim <- round(range(validation.results[,c(4,8,9)]))

    img.dir <- 'content/figures';
    if(!file.exists(img.dir)) { dir.create(img.dir) }

    img.filepath <- file.path(img.dir, img.filename)

    postscript(img.filepath, height=7, width=7, onefile=FALSE, horizontal=FALSE)
        plot( validation.results[,1], validation.results[,4], pch=16, cex=0.5,
        main=img.title, ylim=y.lim, xlab='Trial #', ylab='Makespan (hr)' )

        lines(validation.results[,1], validation.results[,8], col='red',
        lty='dotted', lwd=2)

        lines(validation.results[,1], validation.results[,9], col='red',
        lty='dotted', lwd=2)

        legend('bottomright', legend='95% CI for predicted makespan', 
        col='red', lty='dotted', lwd=2)
    dev.off()

    cat('Created plot:', img.filepath, '\n')

} # end function - plot.validation.results



# ---- 1instance-deterministic-runtimes ----

# Calculate makespan for deterministic runtimes
benefit <- 30 # in dollars
deadline <- 25 # in hours
runtimes <- matrix(nrow=5, ncol=3)
runtimes[,1] = c(5.2, 8.5, 2.5, 4.1, 6) # in hours
runtimes[,2] = 0.93 * runtimes[,1]
runtimes[,3] = 0.74 * runtimes[,1]

result = get.schedule.deterministic.runtimes(instance.types, cost, benefit,
  deadline, runtimes)


# ---- 1instance-stochastic-runtimes-known-dist-100tasks ----
start.time <- proc.time()
validate.stochastic.runtimes(instance.types, instance.costs, num.tasks=100)
cat('Time taken: ', round((proc.time()-start.time)[3]/60,2), ' mins')

# ---- 1instance-stochastic-runtimes-known-dist-250tasks ----
start.time <- proc.time()
validate.stochastic.runtimes(instance.types, instance.costs, num.tasks=250)
cat('Time taken: ', round((proc.time()-start.time)[3]/60,2), ' mins')

# ---- 1instance-stochastic-runtimes-known-dist-500tasks ----
start.time <- proc.time()
validate.stochastic.runtimes(instance.types, instance.costs, num.tasks=500)
 cat('Time taken: ', round((proc.time()-start.time)[3]/60,2), ' mins')

# ---- 1instance-stochastic-runtimes-known-dist-1000tasks ----
start.time <- proc.time()
validate.stochastic.runtimes(instance.types, instance.costs, num.tasks=1000)
cat('Time taken: ', round((proc.time()-start.time)[3]/60,2), ' mins')
  

# ---- 1instance-stochastic-runtimes-known-dist-10tasks ----
start.time <- proc.time()
validate.stochastic.runtimes.bootstrap(instance.types, instance.costs,
  num.tasks=10)
cat('Time taken: ', round((proc.time()-start.time)[3]/60,2), ' mins')

# ---- 1instance-stochastic-runtimes-known-dist-20tasks ----
start.time <- proc.time()
validate.stochastic.runtimes.bootstrap(instance.types, instance.costs,
  num.tasks=20)
cat('Time taken: ', round((proc.time()-start.time)[3]/60,2), ' mins')

# ---- 1instance-stochastic-runtimes-known-dist-25tasks ----
start.time <- proc.time()
validate.stochastic.runtimes.bootstrap(instance.types, instance.costs,
  num.tasks=25)
cat('Time taken: ', round((proc.time()-start.time)[3]/60,2), ' mins')

# ---- 1instance-stochastic-runtimes-known-dist-50tasks ----
start.time <- proc.time()
validate.stochastic.runtimes.bootstrap(instance.types, instance.costs,
  num.tasks=50)
cat('Time taken: ', round((proc.time()-start.time)[3]/60,2), ' mins')

# ---- 1instance-stochastic-runtimes-known-dist-75tasks ----
start.time <- proc.time()
validate.stochastic.runtimes.bootstrap(instance.types, instance.costs,
  num.tasks=75)
cat('Time taken: ', round((proc.time()-start.time)[3]/60,2), ' mins')




# ---- SA.validation ----
task.file <- 'validate/10-tasks.jobs.tab'
tasks <- read.table(task.file, header=T, row.names=1, stringsAsFactors=FALSE)
task.sizes <- tasks[,2]

scores <- matrix(nrow=NROW(tasks), ncol=2)
costs <- matrix(nrow=NROW(tasks), ncol=2)

# for (trial in 1:length(task.sizes)) {
for (i in 1:NROW(tasks)) {

  cat('Processing trial', i, '\n')

  job <- as.numeric(strsplit(task.sizes[i], ',')[[1]])
  deadline <- 750
  cluster.instance.type <- 'm3xlarge'
  cluster.size <- 2
  max.iter <- 100
  max.temp <- 10
  reset.score.pct <- 10

  best.schedule <- schedule(job, deadline, cluster.instance.type, cluster.size,
  max.iter, max.temp, reset.score.pct, debug=TRUE)

  leptf.schedule <- get.initial.schedule(cluster.size, job, rts, 'leptf')
  leptf.schedule <- get.score(leptf.schedule, rt, rts, deadline)

  scores[i,1] <- attr(best.schedule, 'score')
  scores[i,2] <- attr(leptf.schedule, 'score')

  costs[i,1] <- attr(best.schedule, 'processing.cost')
  costs[i,2] <- attr(leptf.schedule, 'processing.cost')

}

colnames(costs) <- c('Predicted', 'LEPT')
colnames(scores) <- c('Predicted', 'LEPT')

plot(scores, main='Probability of completing tasks by deadline',
xlab='Simulated Annealing Prediction', ylab='LEPT',
xlim=range(scores), ylim=range(scores))
abline(0,1)


img.filename <- 'validate-SA-LEPT-scores-2inst-100iter-10tasks.eps'
postscript(img.filename, height=7, width=7, onefile=FALSE, horizontal=FALSE)
  par(mar=par()$mar + 2)
  plot(scores, main='Probability of completing tasks by deadline',
  xlab='Simulated Annealing Prediction', ylab='LEPT', xlim=range(scores),
  ylim=range(scores), cex=0.75)
  abline(0,1)
  abline(v=0.95, lty='dotted')
  abline(h=0.95, lty='dotted')
dev.off()


img.filename <- 'validate-SA-LEPT-costs-2inst-100iter-10tasks.eps'
postscript(img.filename, height=7, width=7, onefile=FALSE, horizontal=FALSE)
  par(mar=par()$mar + 2)
  plot(costs, main='Cost of completing tasks by deadline ($)',
  xlab='Simulated Annealing Prediction', ylab='LEPT', xlim=range(costs),
  ylim=range(costs), cex=0.75)
  abline(0,1)
dev.off()
