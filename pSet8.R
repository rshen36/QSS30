# function for counting the number of runs in a given sequence of zeroes and ones
countRuns <- function(vec) {
  count <- 1  # always at least 1 run
  
  cur <- vec[1]
  for (i in 2:length(vec)) {  # already accounted for first entry
     prev <- cur
     cur <- vec[i]
     
     if (cur != prev) { count <- count + 1 }  # account for the new run
  }
  
  return(count)
}


# function for simulating the number of runs across 200 Bernoulli trials
simTrials <- function(p) {
  # generate random outcome of 200 Bernoulli trials
  # assumption: p here is the probability of 1 occurring
  trials <- sample(x = c(1,0), size = 200, replace = TRUE, prob = c(p,1-p))
  
  return(countRuns(trials)) # return the number of runs
}


# run 1000 simulations and generate histograms of the results
results <- rep(x = 0, times = 1000)
for (i in 1:length(results)) {  # p = 0.3
  results[i] <- simTrials(0.3)
}
hist(x = results, main = "Histogram: number of runs (p = 0.3)", xlab = "Runs")
for (i in 1:length(results)) {  # p = 0.3
  results[i] <- simTrials(0.5)
}
hist(x = results, main = "Histogram: number of runs (p = 0.5)", xlab = "Runs")

