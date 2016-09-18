readFiles <- function(directory) {
  library(dplyr)
  
  for (i in 1:4) {
    # passing offense numbers
    passing <- paste("pass201", i, sep = "")
    passFile <- paste("passing-offense-201", i, ".csv", sep = "")
    assign(passing, read.csv(file = paste(directory, passFile, sep = ""), header = TRUE, sep = "\t" ))
    p <- get(passing)
    p <- p[-(33:35),]
    p$Tm <- suppressWarnings(factor(x = p$Tm, exclude = c("Avg Team", "Avg Tm/G", "League Total")))
    assign(passing, p, envir = .GlobalEnv)
    
    # total offense numbers
    offense <- paste("off201", i, sep = "")
    offFile <- paste("team-offense-201", i, ".csv", sep = "")
    assign(offense, readLines(con = paste(directory, offFile, sep = ""), warn = FALSE))
    o <- get(offense)
    o <- o[-1]
    o <- o[-(34:36)]
    o <- read.csv(file = textConnection(o), header = TRUE, sep = "\t")
    o$Tm <- suppressWarnings(factor(x = o$Tm, exclude = c("Avg Team", "Avg Tm/G", "League Total")))
    o$Tm <- sapply(o$Tm,as.character)
    o$avgPass <- o$Yds.1/16
    o$avgRush <- o$Yds.2/16
    o$avgPF <- o$PF/16
    o$avgOppPassAlw <- rep(0, times = 32)
    o$avgOppRushAlw <- rep(0, times = 32)
    o$avgOppPFAlw <- rep(0, times=32)
   
    
    # total defense numbers
    defense <- paste("def201", i, sep = "")
    defFile <- paste("team-defense-201", i, ".csv", sep = "")
    assign(defense, readLines(con = paste(directory, defFile, sep = ""), warn = FALSE))
    d <- get(defense)
    d <- d[-(1:2)]
    d <- d[-(34:36)]
    d <- read.csv(file = textConnection(d), header = TRUE, sep = ",")
    d$Tm <- suppressWarnings(factor(x = d$Tm, exclude = c("Avg Team", "Avg Tm/G", "League Total")))
    d$avgPass <- d$Yds.1/16
    d$avgRush <- d$Yds.2/16
    d$avgPF <- d$PF/16
    assign(defense, d, envir = .GlobalEnv)
    
    # season games
    games <- paste("games201", i, sep = "")
    gamesFile <- paste("games-201", i, ".csv", sep = "")
    assign(games, read.csv(file = paste(directory, gamesFile, sep = ""), header = TRUE, sep = ","), envir = .GlobalEnv)
  
    # put averages with corresponding games
    g <- get(games)
    for (m in 1:nrow(g)) {
      for (n in 1:nrow(d)) {
        if (g$Opp[m] == d$Tm[n]) {
          g$avgPass[m] <- d$avgPass[n]
          g$avgRush[m] <- d$avgRush[n]
          g$avgPF[m] <- d$avgPF[n]
        }
      }
    }
    assign(games, g, envir = .GlobalEnv)
    
    # put averages with corresponding offenses
    for (p in 1:nrow(o)) {
      t <- filter(g, Team == o$Tm[p])
      o[p,]$avgOppPassAlw <- mean(t$avgPass)
      o[p,]$avgOppRushAlw <- mean(t$avgRush)
      o[p,]$avgOppPFAlw <- mean(t$avgPF)
    }
    
    # calculate the differences
    o$passDiff <- o$avgPass - o$avgOppPassAlw
    o$rushDiff <- o$avgRush - o$avgOppRushAlw
    o$PFDiff <- o$avgPF - o$avgOppPFAlw
    o$totalDiff <- o$passDiff + o$rushDiff
    
    # add in the year
    Year <- rep(x = paste("201", i, sep = ""), times = 32)
    o <- cbind(o[1:3], Year, o[4:36])
    o$Year <- sapply(o$Year, as.integer)
    
    assign(offense, o, envir = .GlobalEnv)
  }
}

# copy this line below
#final <- rbind(off2011,off2012,off2013,off2014)