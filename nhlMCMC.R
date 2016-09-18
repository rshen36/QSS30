"
  file: nhlMCMC.R
  usage: use MCMC to rank hockey teams from the given team season data
  input: vector of file locations, vector of team names, number of iterations
    Data in the given files are expected to be tab-separated .csv files with column headers.
  output: returns a vector of length n containing the states from a MCMC run
"
nhlMCMC <- function (files, teams, n) {
  # input checks
  if (length(files) != length(teams)) { # incompatible vector inputs
    stop("The given vectors must be of the same length.")
  }
  for (i in 1:length(files)) {  # file accessibility
    if (try(file.access(files[i], mode = 4)) != 0) {
      stop("The given files are not readable.")
    }
  }
  if (class(teams) != "character" | class(files) != "character") {  # data type
    stop("Input vectors must be of the chr data type.")
  }
  
  # read in files
  nhl <- list()  # initialize the list
  for (i in 1:length(teams)) {
    # assumes the input files are tab-separated with column headers
    nhl[[teams[i]]] <- assign(teams[i],read.csv(file = files[i], header = TRUE, sep = "\t"))
  }
  
  states <- rep(NA, times = n)  # vector for the Markov chain states
  topTeam <- sample(teams, 1) # random initial top team
  states[1] <- topTeam
  
  # run the Markov Chain Monte Carlo algorithm
  for (i in 2:n) {
    checkTeam <- sample(teams[-match(topTeam,teams)],1)
    t <- compareTeams(topTeam,nhl[[topTeam]],checkTeam,nhl[[checkTeam]])
    b <- rbinom(1,1,t)  # Bernoulli random variable
    
    if (b == 0) { # top team has changed
      topTeam <- checkTeam
    }
    states[i] <- topTeam  # assign new top team
  }
  
  return(states)
}